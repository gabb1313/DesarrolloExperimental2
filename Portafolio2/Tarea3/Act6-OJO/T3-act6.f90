!Tarea 3 - Actividad 6
!DesExpII, 2018-1
!Gabriela Carretas

!A partir de una CIRAN2D, mueve las N partículas de manera
!aleatoria un número de ciclos NSTEP. Se incluyen las condiciones
!periódicas en el sistema para mantener la misma concentración. Se
!calcula la energía de la configuración y la energía potencial por partícula.

!Iniciando el programa

!Configuración Inicial Random 2D
!Unidades reducidas: sig
program energiapotencial
  use constantes2D

  !Declaración de variables
  implicit none

  !L: longitud de la celda de simulación
  !Lr: longitud donde se acomodarán las partículas
  !dens: concentración reducida del sistema
  !phi: fracción en volumen del sistema
  !dmax: desplazamiento máximo en los movimientos
  !rcut: radio de corte
  !xo,yo: posiciones viejas
  !xn,yn: posiciones tentativas
  !Vo: energía potencial vieja
  !Vn: energía potencial nueva
  !dV: diferencia de energía nueva(tentativa)/vieja
  !Vnor: energía potencial total normalizada
  !V: energía potencial total
  real *4 L, Lr, dens, phi, dmax, rcut
  real *4 xo, yo, xn, yn, Vo, Vn, dV, Vnor, V
  real *4 rand1, rand2

  !N: número de partículas del sistema
  !NSTEP: número de configuraciones del sistema
  !m: contador de la concentración
  integer *4 N, NSTEP, m

  !obtenemos valores de entrada
  print * , 'Numero total de particulas'
  read (*,*) N
  print * , 'Numero total de configuraciones y desplazamiento maximo'
  read (*,*) NSTEP, dmax

  !concentración inicial
  dens=0.05

  !aumentamos la concentración hasta 0.6
  do m=1,12
    dens=dens+0.05

    !iniciando los vectores de posición
    allocate(x(1:N))
    allocate(y(1:N))

  !calculando la longitud reducida
  L=((sig*real(N))/dens)**r2

  !calculando el radio de corte convencional
  rcut=L/2.0

  !energía inicial en ceros
  V=0.0

  !llamando a la subrutina de la configuración inicial aleatoria 2D
  call CIRAN2D(L, N)

  !abrimos los archivos
  open(2, file='cfinal.dat')
  open(3, file='energia.dat')

  !llamando a la subrutina del cálculo de la energía de la configuración
  call ENCONF2D(rcut, V, L, N)

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
      call MC(dV, V, xn, yn)

      !calculando la energía potencial normalizada
      Vnor=V/real(N)

      !solo guardamos la configuración final
      if (k.EQ.NSTEP) then
        write(2,*) x(i), y(i)
      end if

    end do

    !escribe: # configuración, energía potencial normalizada, concentración
    write(3,*) k, Vnor, dens

  end do

  write(*,*) 'Concentracion reducida:', dens
  write(*,*) 'Con', N, 'particulas, la longitud reducida es', L
  write(*,*) 'Energia potencial reducida', Vnor

  !se cierra el archivo
  close(2)
  close(3)

  !desocupamos los vectores de posición tras usarlos
  deallocate(x)
  deallocate(y)

  end do

!fin del programa
end program energiapotencial
