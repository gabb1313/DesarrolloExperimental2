!Programa principal para simulación con MonteCarlo
!DesExpII, 2018-1
!Gabriela Carretas

!El programa elige la configuración incial óptima para poder realizar
!el movimiento de las partículas utilizando el algoritmo de MonteCarlo.
!Tras obtener las matrices de configuración, se calculan propiedades
!estructurales como la g(r) y la presión.

!Iniciando el programa

!Configuración Inicial Random 3D con traslapes
!Unidades reducidas: sig
program MonteCarlo
  use constantes3D
  use variables3D

  !Declaración de variables
  implicit none

  !xo,yo,zo: posiciones viejas
  !xn,yn,zn: posiciones tentativas
  !V: energía potencial total
  !Vo: energía potencial vieja
  !Vn: energía potencial nueva
  !dV: diferencia de energía nueva(tentativa)/vieja
  !Vnor: energía potencial total normalizada
  !razon: razon de aceptados de MonteCarlo
  !dmax: desplazamiento máximo en los movimientos
  !racep: razon de aceptados para optimización
  !rand1,rand2: números aleatorios
  real *4 xo, yo, zo, xn, yn, zn, V, Vo, Vn
  real *4 dV, Vnor, razon, dmax, racep
  real *4 rand1, rand2, rand3

  !iprint: frecuencia de muestreo en pantalla
  !isave: frecuencia de guardar en el archivo
  !iratio: frecuencia de optimización de paso
  !amc: # de movimientos aceptados por MC
  !i,j,k: contadores del loop
  !cm: contador de la matriz estructural
  integer *4 iprint, isave, iratio
  integer *4 amc
  integer *4 i, j, k, cm

  N=650
  NSTEP=31000
  ter=10000
  iprint=1000
  isave=6
  !phi=0.5
  dmax=0.1
  racep=0.5

  !obtenemos valores de entrada

  !print * , 'Numero total de particulas y concentracion reducida'
  !read (*,*) N, dens
  !print * , 'Numero de configuraciones y desplazamiento maximo'
  !read (*,*) NSTEP, dmax
  !print * , 'Frecuencia de muestreo en pantalla y de guardar en el archivo'
  !read (*,*) iprint, isave
  !print * , 'Razon de aceptados para optimizacion'
  !read (*,*) racep

  !Parámetros del potencial: DOBLE GAUSSIANA
  T=0.2
  !el valor es el de eta tilde, como aqui se usa eta* lo divido entre T para que est� reducida
  eta=0.0263/T
  xi=3.0

  !calculando la concentración reducida en términos de la
  !fracción en volumen
  !dens=(6.0*phi)/pi
  dens=0.4

  !calculando la longitud reducida
  L=((sig*real(N))/dens)**r3

  !calculando el radio de corte convencional
  rcut=L/2.0

  !calculando el número total de configuraciones guardadas
  ens=(NSTEP-ter)/isave

  !energía inicial en ceros
  V=0.0
  dV=0.0

  !comenzar en ceros los contadores del algoritmo
  amc=0
  cm=0

  !abrimos los archivos
  open(2, file='cf.dat')
  open(3, file='terma.dat')
  open(11, file='gr.dat')

  !iniciando los vectores de posición
  allocate(x(1:N))
  allocate(y(1:N))
  allocate(z(1:N))

  !iniciando las matrices de configuración
  allocate(cx(N,ens))
  allocate(cy(N,ens))
  allocate(cz(N,ens))

  !llamando a la subrutina de la configuraci�n inicial aleatoria con traslapes
  call CIRAN3D
  
  !llamando a la subrutina del cálculo de la energía de la configuración
  call ENCONF3D(V)

  write(*,*) 'Longitud reducida:',L
  write(*,*) 'Concentración reducida:', dens
  write(*,*) 'Energia de la configuracion inicial:',V
  write(*,*) 'Numero de configuraciones a guardar:',ens
  write(*,*) '# de configuracion || E. Potencial normalizada || Razon de aceptados'

  !iniciamos el contador de la configuración
  do k=1,NSTEP
    !iniciamos el contador de las partículas
    do i=1,N
      !posiciones iniciales
      xo=x(i)
      yo=y(i)
      zo=z(i)

      !llamando a la subrutina del cálculo de la energía por partícula
      call ENPAR3D(xo, yo, zo, i, Vo)

      !generando números aleatorios
      call random_number(rand1)
      call random_number(rand2)
      call random_number(rand3)

      !movemos a la i-ésima partícula un desplazamiento aleatorio
      !con tamaño máximo dado por dmax
      xn = xo+(2.0*rand1-1.0)*dmax
      yn = yo+(2.0*rand2-1.0)*dmax
      zn = zo+(2.0*rand3-1.0)*dmax

      !condiciones periódicas
      xn = xn-L*anint(xn/L)
      yn = yn-L*anint(yn/L)
      zn = zn-L*anint(zn/L)

      !llamando a la subrutina del cálculo de la energía por partícula
      call ENPAR3D(xn, yn, zn, i, Vn)

      !diferencia de energías
      dV=Vn-Vo

      !llamando a la subrutina del Montecarlo
      call MC(dV, xn, yn, zn, i, amc, V)

      !calculando la energía potencial normalizada
      Vnor=V/real(N)

      !solo guardamos la configuración final
      if (k.eq.NSTEP) then
        write(2,*) x(i), y(i), z(i)
      end if

    !termina loop de partículas
    end do

    !escribe: # configuración, energía potencial normalizada
    write(3,*) k, Vnor

    !iprint=iratio
    !si el # de configuración es multiplo del contador iprint
    if(mod(k,iprint).eq.0.0)then
      !calcula la razón de aceptados en el intervalo de la frecuencia
      razon=real(amc)/real(N*iprint)
      !escribe en pantalla para monitorear
      write(*,*) k, Vnor, razon

      !criterio de optimización de paso
      !if(razon.gt.racep) then
       ! dmax=dmax*1.05
      !else
      !  dmax=dmax*0.95
      !end if

      amc=0.0
    end if

    !Decisión para guardar en arreglos
    if (k.ge.ter.and.mod(k,isave).eq.0.0) then
        cm=cm+1
        do j=1,N
          !guardando configuraciones de equilibrio
          cx(j,cm)=x(j)
          cy(j,cm)=y(j)
          cz(j,cm)=z(j)
        end do
    end if

  !termina loop de configuraciones
  end do

  !llamando a la subrutina del cálculo de la concentración final
  call DENSCTE3D

  !llamando a la subrutina del cálculo de la g(r)
  call gr3D

  !cerramos los archivos
  close(2)
  close(3)

  !desocupamos los arreglos
   deallocate(x)
   deallocate(y)
   deallocate(z)
   deallocate(cx)
   deallocate(cy)
   deallocate(cz)

!fin del programa
end program MonteCarlo
