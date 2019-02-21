module constantes3D
  implicit None

  !r3: exponente para celdas 3D
  !sig: diámetro de las partículas
  !pi: valor del número Pi
  !ter: valor donde ya se encuentra termalizado
  real *4, parameter :: r3=1.0/3.0
  real *4, parameter :: sig=1.0
  real *4, parameter :: pi=4.0*atan(1.0)
  real *4, parameter :: ter=15000.0

  !hist: vectores del histograma
  real, dimension(1000000) :: hist

end module constantes3D
