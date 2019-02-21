module constantes2D
  implicit None

  !r2: exponente para celdas 2D
  !sig: diámetro de las partículas
  !pi: valor del número Pi
  real *4, parameter :: r2=1.0/2.0
  real *4, parameter :: sig=1.0
  real *4, parameter :: pi=4.0*atan(1.0)

  !i,j,k: contadores del loop
  integer *4 i, j, k

  !x,y: vectores de posición para las N partículas
  real, dimension(:), allocatable, save :: x, y

end module constantes2D
