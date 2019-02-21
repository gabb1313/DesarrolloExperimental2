!Desarrollo Experimental II
!Ines Valenzuela
!Energía de la i-esima partícula

subroutine ienerg3d(xi,yi,zi,i,rcut,N,V)

use posiciones3d

! Declaracion de variables
  implicit none
  real*4 xij,yij,zij,xi,yi,zi,r,V,Vij,rcut
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
   Vij=0.0
   end if
   v=v+vij
  end if
  end if
 end do
return
end   
