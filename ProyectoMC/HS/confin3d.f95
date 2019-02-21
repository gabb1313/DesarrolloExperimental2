!Desarrollo Experimental II
!Ines Valenzuela
!Configuracion inicial aleatoria sin traslapes 3d

subroutine confin3d(Lr,N)

use posiciones3d

! Declaracion de variables
  implicit none
  real*4 Lr,xij,yij,zij,r
  integer*4 clock, i,j,k,N
  integer, dimension(:), allocatable :: iseed

  k=12                               !Tamaño de iseed

! Generador de números aleatorios
  allocate(iseed(k))
  call random_seed(size = k)
  call system_clock(COUNT=clock)
  iseed = clock + 37 * [(i, i = 0,k-1)]
  call random_seed(PUT = iseed) 

  
  open (1, file = '3dconfigin.dat')

! Calculando las posiciones para las partículas
  do i = 1,N
   
!  Posiciones aleatorias 
43 CALL random_number(x(i))
   CALL random_number(y(i))
   CALL random_number(z(i))  

!  Para que se encuentren entre -L/2 y L/2
   x(i)=(x(i)-0.5)*Lr
   y(i)=(y(i)-0.5)*Lr
   z(i)=(z(i)-0.5)*Lr

!  Para evitar los traslapes condicionamos a las partículas
!  de tal forma que la distancia entre ellas sea mayor que
!  sigma, si no se cumple se calcula otro número aleatrio
!  hasta cumplir la condición
   do j=1,i-1	
     xij=x(i)-x(j)
     yij=y(i)-y(j)
     zij=z(i)-z(j)
     r=sqrt(xij*xij+yij*yij+zij*zij)
     if (r.lt.1.0) then
!     write(*,*) 'Traslape de las partículas', i,j
     go to 43
     end if
     end do
  write(1,*) x(i),y(i),z(i)
  end do
return
end
