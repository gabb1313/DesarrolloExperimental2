module constantes3D
  implicit None

  !r3: exponente para celdas 3D
  !sig: diámetro de las partículas
  !pi: valor del número Pi
  real *4, parameter :: r3=1.0/3.0
  real *4, parameter :: sig=1.0
  real *4, parameter :: pi=4.0*atan(1.0)

  !i,j,k: contadores del loop
  integer *4 i, j, k

  !x,y,z: vectores de posición para las N partículas
  real, dimension(:), allocatable, save :: x, y, z
  
end module constantes3D
