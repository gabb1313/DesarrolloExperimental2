!Desarrollo Experimental II
!Ines Valenzuela
!Energía de la configuracion inicial aleatoria sin traslapes 2d

subroutine energin3d(rcut,N,V)

use posiciones3d

! Declaracion de variables
  implicit none
  real*4 xij,yij,zij,xi,yi,zi,r,Vij
  real*4 rcut
  real*4 V
  integer*4 i,j
  integer*4 N

  V=0
  do i=1,N-1
    xi=x(i)
    yi=y(i)
    zi=z(i)
    do j=i+1,N
    xij= xi-x(j) 
    yij= yi-y(j)
    zij= zi-z(j)

! Condición de imagen mínima para quitar fronteras
   xij=xij-L*anint(xij/L)
   yij=yij-L*anint(yij/L)
   zij=zij-L*anint(zij/L)

! Utilizando el potencial de esfera dura

  r=sqrt(xij*xij+yij*yij+zij*zij)
  !write(*,*) r
  
  if (r.lt.rcut) then
   
   if(r.le.1.0) then
   Vij=1.0E+10
   !write(*,*) i,j,r
   else
   Vij=0.0
   end if
   v=v+vij
  end if
 end do 
 end do
return
end   
