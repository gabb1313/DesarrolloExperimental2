!Potencial de interacción: Yukawa repulsiva

!Parámetro de entrada: distancia radial entre partículas
!Parámetro de entrada: energía potencial de entrada
!Parámetro de entrada: "parcial" del potencial
Subroutine FYukawa(r,Vij,U2)
  use variables3D

  implicit none

  !r: distancia entre partículas
  !Vij: energía potencial entre partículas
  !U: forma funcional del potencial
  !U2: parámetro general para el calculo de fuerza (-1/r)(dU/dr)
  real *4 r, Vij, U, U2

  !calculando forma funcional del potencial
  U=exp(-zk*r)
  !parámetro general para cálculo de la fuerza
  U2=(yuk*U*(zk*r+1.0))/r**3
  !calculando la energía potencial por partícula
  !sumando la contribución de energía
  Vij=((yuk*U)/r)

  return
end Subroutine FYukawa
