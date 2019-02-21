!Programa principal para simulación con MonteCarlo
!DesExpII, 2018-1
!Gabriela Carretas

!El programa elige la configuración incial óptima para poder realizar
!el movimiento de las partículas utilizando el algoritmo de MonteCarlo.
!Tras obtener las matrices de configuración, se calculan propiedades
!estructurales como la g(r) y la presión.

!Iniciando el programa

!Configuración Inicial Random 2D (n*<=0.6)
!Configuración Inicial Regular 2D (n*>0.6)
!Unidades reducidas: sig
program esqueleto
  use constantes2D
  use variables

  !Declaración de variables
  implicit none

  !dens: concentración reducida
  !L: longitud de la celda de simulación
  !rcut: radio de corte
  !V: energía potencial total
  !xo,yo: posiciones viejas
  !xn,yn: posiciones tentativas
  !Vo: energía potencial vieja
  !Vn: energía potencial nueva
  !dV: diferencia de energía nueva(tentativa)/vieja
  !Vnor: energía potencial total normalizada
  !razon: razon de aceptados de MonteCarlo
  !dmax: desplazamiento máximo en los movimientos
  !racep: razon de aceptados para optimización
  !rand1,rand2: números aleatorios
  real *4 dens, L, rcut, V
  real *4 xo, yo, xn, yn, Vo, Vn
  real *4 dV, Vnor, razon, dmax, racep
  real *4 rand1, rand2

  !ci: variable de entrada para nombres de los archivos de datos
  character ci*40

  !N: número de partículas
  !NSTEP: número de configuraciones del sistema
  !iprint: frecuencia de muestreo en pantalla
  !isave: frecuencia de guardar en el archivo
  !iratio: frecuencia de optimización de paso
  !amc: # de movimientos aceptados por MC
  !tmc: # de movimientos totales
  !ens: tamaño del ensemble (# de configuraciones guardadas totales)
  integer *4 N, NSTEP
  integer *4 iprint, isave, iratio
  integer *4 amc, tmc, ens

  N=100
  NSTEP=100000
  iprint=10
  isave=10

  !calculando la concentración reducida en términos de la
  !fracción en volumen
  !dens=(6.0*phi)/pi

  !iniciando los vectores de posición
  allocate(x(1:N))
  allocate(y(1:N))

  print * , 'La concentracion reducida'
  !print * , 'Numero total de particulas'
  read (*,*) dens

  !calculando la longitud reducida
  L=((sig*real(N))/dens)**r2

  !calculando el radio de corte convencional
  rcut=L/2.0

  !energía inicial en ceros
  V=0.0

  !si la n* es mayor que 0.6 (Recuerda al RCP) elige una regular
  if (dens.gt.0.6) then
    call CIREG2D(L, N)
  !si es menor, comienza con una  aleatoria
  else
    call CIRAN2D(L, N)
  end if

  !llamando a la subrutina del cálculo de la energía de la configuración
  call ENCONF2D(rcut, V, L, N)

  write(*,*) 'Longitud reducida:',L
  write(*,*) 'Energia de la configuracion inicial:',V

  !obtenemos valores de entrada

  print * , 'Desplazamiento maximo'
  read (*,*) dmax
  !print * , 'Frecuencia de muestreo en pantalla y de guardar en el archivo'
  !read (*,*) iprint, isave
  print * , 'Razon de aceptados para optimizacion'
  read (*,*) racep

  !comenzar en ceros los contadores del algoritmo
  amc=0
  tmc=0
  w=0

  !abrimos los archivos
  open(2, file='cf.dat')
  open(3, file='terma.dat')

  !calculando el número total de configuraciones guardadas
  ens=(NSTEP-ter)/isave

  write(*,*) 'Numero de configuraciones a guardar:',ens
  write(*,*) '# de configuracion || E. Potencial normalizada || Razon de aceptados || Longitud max. de desplazamiento'

  !iniciando las matrices de configuración
  allocate(cx(N,ens))
  allocate(cy(N,ens))

  !iniciamos el contador de la configuración
  do k=1,NSTEP
    !iniciamos el contador de las partículas
    do i=1,N
      !posiciones iniciales
      xo=x(i)
      yo=y(i)

      !llamando a la subrutina del cálculo de la energía por partícula
      call ENPAR2D(xo, yo, i, rcut, Vo, L, N)

      !generando números aleatorios
      call random_number(rand1)
      call random_number(rand2)

      !movemos a la i-ésima partícula un desplazamiento aleatorio
      !con tamaño máximo dado por dmax
      xn = xo+(2.0*rand1-1.0)*dmax
      yn = yo+(2.0*rand2-1.0)*dmax

      !condiciones periódicas
      xn = xn-L*anint(xn/L)
      yn = yn-L*anint(yn/L)

      !llamando a la subrutina del cálculo de la energía por partícula
      call ENPAR2D(xn, yn, i, rcut, Vn, L, N)

      !diferencia de energías
      dV=Vn-Vo

      !llamando a la subrutina del Montecarlo
      call MC(dV, V, xn, yn, amc)

      !calculando la energía potencial normalizada
      Vnor=V/real(N)

      !solo guardamos la configuración final
      if (k.eq.NSTEP) then
        write(2,*) x(i), y(i)
      end if

    end do

    !escribe: # configuración, energía potencial normalizada
    write(3,*) k, Vn

    !iprint=iratio
    !si el # de configuración es multiplo del contador iprint
    if(mod(k,iprint).eq.0.0)then
      !calcula la razón de aceptados en el intervalo de la frecuencia
      razon=real(amc)/real(N*iprint)
      !escribe en pantalla para monitorear
      write(*,*) k, Vnor, razon, dmax

      !criterio de optimización de paso
      if(razon.gt.racep) then
        dmax=dmax*1.05
      else
        dmax=dmax*0.95
      end if

      amc=0.0
    end if

    if (k.ge.ter.and.mod(k,isave).eq.0.0) then
        w=w+1
        do j=1,N
          cx(j,w)=x(j)
          cy(j,w)=y(j)
        end do
    end if
  end do

  call gr(rcut, ens, N, L, dens)

   deallocate(x)
   deallocate(y)
   deallocate(cx)
   deallocate(cy)

!fin del programa
end program esqueleto
