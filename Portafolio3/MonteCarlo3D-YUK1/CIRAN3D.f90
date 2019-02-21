!Configuración Inicial Aleatoria 3D
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

52  call random_number(x(i))
    call random_number(y(i))
    call random_number(z(i))

    !se asigna la posición aleatoriamente
    x(i)=(x(i)-0.5)*Lr
    y(i)=(y(i)-0.5)*Lr
    z(i)=(z(i)-0.5)*Lr
    !contador que compara la i-ésima partícula con la
    !j-ésima, comprobando que no se traslapen
    do j=1,i-1
      !calculando el vector posición rij
      xt=x(i)-x(j)
      yt=y(i)-y(j)
      zt=z(i)-z(j)
      r=sqrt((xt)**2+(yt)**2+(zt)**2)
      !si r<=sigma entonces hay traslape y vuelve a asignar
      !una posición aleatoria
      if(r.lt.sig) then
        go to 52
      end if
    end do
    !escribimos las posiciones aleatorias en el archivo
    write(1,*) x(i), y(i), z(i)
  end do
  !se cierra el archivo
  close(1)

  deallocate(semilla)

  return
!fin de la subrutina
end subroutine ciran3D
