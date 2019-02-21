!modulo para guardar el arreglo de posiciones

module posiciones3d
 
 implicit none
 real, dimension(:), allocatable,save ::x,y,z
 real, dimension(:,:), allocatable,save ::cx,cy,cz
 real*4 L
 real, parameter ::  pi=4.0*atan(1.0)


end module posiciones3d
