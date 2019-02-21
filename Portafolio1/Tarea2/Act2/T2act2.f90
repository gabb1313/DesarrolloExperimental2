!Tarea 2 - Actividad 2
!DesExpII, 2018-1
!Gabriela Carretas

!El programa coloca N partículas de manera aleatoria y sin
!traslapes, en un cubo de longitud de celda L* obtenida en
!base a la concentración n* y al número de partículas. El
!origen se localiza en el punto medio del plano.

!Iniciando el programa

!Configuración Inicial Random 3D
!Parámetro de entrada: N(número de partículas)
!Parámetro de entrada: dens(concentración reducida)
!Unidades reducidas: sig
program cir3d

  !Declaración de variables
  implicit none

  !x,y,z: vectores de posición para las N partículas
  !r3: exponente para celdas 3D
  !sig: diámetro de las partículas
  real, dimension(:), allocatable :: x, y, z
  real *4, parameter :: r3=1.0/3.0
  real *4, parameter :: sig=1.0

  !L: longitud de la celda de simulación
  !xt,yt,zt: distancia en x,y,z entre partículas
  !r: distancia radial entre partículas
  !dens: concentración reducida del sistema
  real *4 L, xt, yt, zt, r, dens

  !i,j: contadores del loop
  !N: número de partículas del sistema
  integer *4 i, j, N

  !obtenemos valores de entrada
  print * , 'Numero de particulas y concentracion reducida'
  read (*,*) N, dens

  !creamos los vectores para posición
  allocate(x(1:N))
  allocate(y(1:N))
  allocate(z(1:N))

  !calculando la longitud reducida
  L=(real(N)/dens)**r3

  !contador para la posición de la n-ésima partícula
  do i=1,N
    !archivo de datos
    open(1, file='ciran3d.dat')
      !se asigna la posición aleatoriamente
53    x(i)=(rand()-0.5)*L
      y(i)=(rand()-0.5)*L
      z(i)=(rand()-0.5)*L
      !contador que compara la i-ésima partícula con la
      !j-ésima, comprobando que no se traslapen
    57 do j=1,i-1
      !calculando el vector posición rij
      xt=x(i)-x(j)
      yt=y(i)-y(j)
      zt=z(i)-z(j)
      r=(xt)**2+(yt)**2+(zt)**2
      !si r<=sigma entonces hay traslape y vuelve a asignar
      !una posición aleatoria
      if(r.le.sig) then
      write(*,*) 'Traslape, calculando una posicion distinta'
      go to 53
      end if
    end do
    !escribimos las posiciones aleatorias en el archivo
    write(1,*) x(i), y(i), z(i)
  end do
  !desocupamos los vectores de posición tras usarlos
  deallocate(x)
  deallocate(y)
  deallocate(z)
  !se cierra el archivo
  close(1)

  write(*,*) 'A una concentracion reducida de', dens
  write(*,*) 'y con', N, 'particulas, la longitud reducida es', L

!fin del programa
end program cir3d
