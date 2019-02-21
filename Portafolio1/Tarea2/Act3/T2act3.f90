!Tarea 2 - Actividad 2
!DesExpII, 2018-1
!Gabriela Carretas

!El programa coloca N partículas de manera aleatoria y sin
!traslapes, en un plano de longitud de celda L* obtenida en
!base a la concentración n* y al número de partículas. El
!origen se localiza en el punto medio del plano.

!Iniciando el programa

!Configuración Inicial Random 2D
!Parámetro de entrada: N(número de partículas)
!Parámetro de entrada: dens(concentración reducida)
!Unidades reducidas: sig
program cir2d

  !Declaración de variables
  implicit none

  !x,y: vectores de posición para las N partículas
  !r2: exponente para celdas 2D
  !sig: diámetro de las partículas
  real, dimension(:), allocatable :: x, y
  real *4, parameter :: r2=1.0/2.0
  real *4, parameter :: sig=1.0

  !L: longitud de la celda de simulación
  !Lr: longitud donde se acomodarán las partículas
  !xt,yt: distancia en x,y entre partículas
  !r: distancia radial entre partículas
  !dens: concentración reducida del sistema
  real *4 L, Lr, xt, yt, r, dens

  !i,j: contadores del loop
  !N: número de partículas del sistema
  integer *4 i, j, N

  !obtenemos valores de entrada
  print * , 'Numero de particulas'
  read *, N
  print * , 'Concentracion reducida'
  read *, dens

  allocate(x(1:N))
  allocate(y(1:N))

  !calculando la longitud reducida
  L=((sig*real(N))/dens)**r2

  !longitud a utilizar para acomodar todas las partículas dentro
  !de la celda de simulación
  Lr=L-(sig/2.0)

  !contador para la posición de la n-ésima partícula
  do i=1,N
    !archivo de datos
    open(1, file='ciran2d.dat')
      !se asigna la posición aleatoriamente
52    x(i)=(rand()-0.5)*Lr
      y(i)=(rand()-0.5)*Lr
      !contador que compara la i-ésima partícula con la
      !j-ésima, comprobando que no se traslapen
    55 do j=1,i-1
      !calculando el vector posición rij
      xt=x(i)-x(j)
      yt=y(i)-y(j)
      r=(xt)**2+(yt)**2
      !si r<=sigma entonces hay traslape y vuelve a asignar
      !una posición aleatoria
      if(r.le.sig) then
      write(*,*) 'Traslape, calculando una posicion distinta'
      go to 52
      end if
    end do
    !escribimos las posiciones aleatorias en el archivo
    write(1,*) x(i), y(i)
  end do
  !desocupamos los vectores de posición tras usarlos
  deallocate(x)
  deallocate(y)
  !se cierra el archivo
  close(1)

  write(*,*) 'A una concentracion reducida de', dens
  write(*,*) 'y con', N, 'particulas, la longitud reducida es', L

!fin del programa
end program cir2d
