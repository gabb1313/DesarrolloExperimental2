!Tarea 3 - Actividad 5
!DesExpII, 2018-1
!Gabriela Carretas

!A partir de una CIREG3D, mueve las N partículas de manera
!aleatoria un número de ciclos NSTEP. Se incluyen las condiciones
!periódicas en el sistema para mantener la misma concentración. Se
!eligen dos trazadoras con posiciones iniciales distintas (centro y orilla).

!Iniciando el programa

!Configuración Inicial Regular 3D
!Unidades reducidas: sig
program cireg3DmovcpT
  use constantes3D

  !Declaración de variables
  implicit none

  !L: longitud de la celda de simulación
  !Lr: longitud donde se acomodarán las partículas
  !dens: concentración reducida del sistema
  !phi: fracción en volumen del sistema
  !dmax: desplazamiento máximo en los movimientos
  real *4 L, Lr, dens, phi, dmax

  !N: número de partículas del sistema
  !NSTEP: número de configuraciones del sistema
  !P: número de partículas por lado
  integer *4 N, NSTEP, P

  !obtenemos valores de entrada
  print * , 'Numero total de particulas y fraccion en volumen'
  read (*,*) N, phi
  print * , 'Numero total de configuraciones y desplazamiento maximo'
  read (*,*) NSTEP, dmax

  !calculando la concentración reducida en términos de la
  !fracción en volumen
  dens=(6.0*phi)/pi

  !debe tener raiz cúbica para que si sea un arreglo regular
  !si la raiz cubica de N no es entero, entonces M será distinta de N
  P=N**r3

  !alertamos al usuario que cambie el número de partículas
  !el loop no termina hasta que tenga un número con raiz exacta
  do
    if (P**3.NE.N) then
      print * , 'Numero total de particulas no tiene raiz cubica. Ingrese otro numero'
      read (*,*) N
      P=N**r3
    else if (P**3.EQ.N) then
      exit
    end if
  end do

  !calculando la longitud reducida
  L=((sig*real(N))/dens)**r3

  !longitud a utilizar para acomodar todas las partículas dentro
  !de la celda de simulación
  Lr=L-sig

  !iniciando los vectores de posición
  allocate(x(1:N))
  allocate(y(1:N))
  allocate(z(1:N))

  !llamando a la subrutina de la configuración inicial regular 3D
  call CIREG3D(L, Lr, dens, N, P)

  write(*,*) 'Para la fraccion en volumen de', phi
  write(*,*) 'Tenemos una concentracion reducida inicial de', dens
  write(*,*) 'Y con', N, 'particulas, la longitud reducida es', L

  !llamando a la subrutina para mover a las partículas (con condiciones periódicas)
  call MOV3D(dmax, N, NSTEP, L)

  !llamando a la subrutina para calcular la densidad final del sistema
  call DENSCTE3D(N, L)

  !desocupamos los vectores de posición tras usarlos
  deallocate(x)
  deallocate(y)
  deallocate(z)

!fin del programa
end program cireg3DmovcpT
