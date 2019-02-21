!Configuración Inicial Aleatoria 3D con traslapes
!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina

Subroutine ciran3D
  use constantes3D
  use variables3D

  !Declaración de variables
  implicit none

  !Lr: longitud donde se acomodarán las partículas
  !xt,yt: distancia en x,y entre partículas
  !r: distancia radial entre partículas
  real *4 Lr, xt, yt, zt, r

  !i,j: contadores del loop
  integer *4 i, j

  !!!!!GENERADOR DE NÚMEROS ALEATORIOS
    integer*4 hora, dimran, w
    integer, dimension(:), allocatable :: semilla

    dimran=12

    allocate(semilla(dimran))
    call random_seed(size=dimran)
    call system_clock(count=hora)
    semilla = hora + 37 * [(w, w=0,dimran-1)]
    call random_seed(put=semilla)
  !!!!!GENERADOR DE NÚMEROS ALEATORIOS

  Lr=L-sig

  !contador para la posición de la n-ésima partícula
  do i=1,N
    !archivo de datos
    open(1, file='ci.dat')

    call random_number(x(i))
    call random_number(y(i))
    call random_number(z(i))

    !se asigna la posición aleatoriamente
    x(i)=(x(i)-0.5)*Lr
    y(i)=(y(i)-0.5)*Lr
    z(i)=(z(i)-0.5)*Lr
    !escribimos las posiciones aleatorias en el archivo
    write(1,*) x(i), y(i), z(i)
    !guardamos en los vectores dinámicos las posiciones iniciales
    xd(i)=x(i)
    yd(i)=y(i)
    zd(i)=z(i)
  end do
  !se cierra el archivo
  close(1)

  deallocate(semilla)

  return
!fin de la subrutina
end subroutine ciran3D
