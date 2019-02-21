!Potencial de interacción: Doble Gaussiana

!Parámetro de entrada: distancia radial entre partículas
!Parámetro de entrada: energía potencial entre partículas
Subroutine DG(r,Vij)
  use variables3D
  use constantes3D

  implicit none

  !r: distancia entre partículas
  !Vij: energía potencial entre partículas
  real *4 r, Vij

  !calculando la energía potencial por partícula
  Vij=(1/T)*exp(-r**2)-eta*exp(-(r-xi)**2)

  return
end Subroutine DG
