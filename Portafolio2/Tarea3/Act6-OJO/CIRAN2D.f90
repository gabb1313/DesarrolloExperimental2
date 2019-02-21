!Configuración Inicial Aleatoria 2D
!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina
!Parámetros de entrada: Longitud de la celda, # de partículas
Subroutine ciran2D(L, N)
  use constantes2D

  !Declaración de variables
  implicit none

  !L: longitud de la celda
  !Lr: longitud donde se acomodarán las partículas
  !xt,yt: distancia en x,y entre partículas
  !r: distancia radial entre partículas
  real *4 L, Lr, xt, yt, r
  integer *4 N

  !!!!!GENERADOR DE NÚMEROS ALEATORIOS!!!!!
    integer*4 hora, dimran
    integer, dimension(:), allocatable :: semilla

    dimran=12

    allocate(semilla(dimran))
    call system_clock(hora)
    call random_seed(size=dimran)
    semilla = hora + 37 * [(i, i=1,dimran)]
    call random_seed(PUT=semilla)
  !!!!!GENERADOR DE NÚMEROS ALEATORIOS!!!!!

  !longitud a utilizar para acomodar todas las partículas dentro
  !de la celda de simulación
  Lr=L-sig

  !contador para la posición de la n-ésima partícula
  do i=1,N
    !archivo de datos
    open(1, file='ciran2D.dat')

    !generando números aleatorios con distribución uniforme entre 0 y 1
52  call random_number(x(i))
    call random_number(y(i))

      !se asigna la posición aleatoriamente
      x(i)=(x(i)-0.5)*Lr
      y(i)=(y(i)-0.5)*Lr
      !contador que compara la i-ésima partícula con la
      !j-ésima, comprobando que no se traslapen
      do j=1,i-1
      !calculando el vector posición rij
      xt=x(i)-x(j)
      yt=y(i)-y(j)
      r=(xt)**2+(yt)**2
      !si r<=sigma entonces hay traslape y vuelve a asignar
      !una posición aleatoria
      if(r.le.sig) then
        go to 52
      end if
    end do
    !escribimos las posiciones aleatorias en el archivo
    write(1,*) x(i), y(i)
  end do
  !se cierra el archivo
  close(1)

  deallocate(semilla)

  return
!fin de la subrutina
end subroutine ciran2D
