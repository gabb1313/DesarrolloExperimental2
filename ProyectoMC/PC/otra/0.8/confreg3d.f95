!Ines Valenzuela C.
!Programa para generar configuraciones iniciales en 2D
!con un arreglo regular

subroutine confreg3d(Lr,N)

use posiciones3d

! Declaracion de variables
  implicit none
  real*4 Lr,yy,xx
  integer*4 i,j,k,m,N

  !open (1, file = '3dconfigin.dat')

  k=0
! Para acomodar las partículas de forma regular con una
! separación L/N
  do i=1,N	
        xx = -L/2.0 +((L/real(N))/2.0) + (L/real(N))*real(i-1)
     do j=1,N
         yy = -L/2.0 +((L/real(N))/2.0) + (L/real(N))*real(j-1)
        do m=1,N
          k=k+1
          x(k)=xx
          y(k)=yy
          z(k) = -L/2.0 +((L/real(N))/2.0) + (L/real(N))*real(m-1)
         ! write(1,*) x(k),y(k),z(k)
        end do
     end do
  end do

return
end
