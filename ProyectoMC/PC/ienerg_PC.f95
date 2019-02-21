!Desarrollo Experimental II
!Ines Valenzuela
!Energía de la i-esima partícula

subroutine ienerg_PC(xi,yi,zi,i,rcut,N,V,lambda,Tred)

use posiciones3d

! Declaracion de variables
  implicit none
  real*4 xij,yij,zij,xi,yi,zi,r,V,Vij,rcut
  real*4 lambda,Tred
  integer*4 i,j,N

  V=0
  do j=1,N
    if (i.ne.j) then
    xij= xi-x(j) 
    yij= yi-y(j)
    zij= zi-z(j)

! Condición de imagen mínima para quitar fronteras
   xij=xij-L*anint(xij/L)
   yij=yij-L*anint(yij/L)
   zij=zij-L*anint(zij/L)

! Utilizando el potencial de esfera dura

  r=sqrt(xij*xij+yij*yij+zij*zij)
  
  if (r.lt.rcut) then
   
   if(r.le.1.0) then
   Vij=1.0E+10
   else
   if (r.gt.1.0.and.r.lt.lambda) then
   Vij= -1.0/Tred
   else
   Vij=0.0
   end if
   end if
   v=v+vij
  end if
  end if
 end do
return
end   
