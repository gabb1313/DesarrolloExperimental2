module constantes2D
  implicit None

  !r2: exponente para celdas 2D
  !sig: diámetro de las partículas
  !pi: valor del número Pi
  !ter: valor donde ya se encuentra termalizado
  real *4, parameter :: r2=1.0/2.0
  real *4, parameter :: sig=1.0
  real *4, parameter :: pi=4.0*atan(1.0)
  real *4, parameter :: ter=0.0

  !hist: vectores del histograma
  real, dimension(10000) :: hist

end module constantes2D
