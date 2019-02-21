!Potencial de interacción: Yukawa repulsiva

!Parámetro de entrada: distancia radial entre partículas
!Parámetro de entrada: energía potencial de entrada
Subroutine Yukawa(r,Vij)
  use variables3D

  implicit none

  !r: distancia entre partículas
  !Vij: energía potencial entre partículas
  !U: forma funcional del potencial
  real *4 r, Vij, U

  !calculando forma funcional del potencial
  U=exp(-zk*r)
  !calculando la energía potencial por partícula
  !sumando la contribución de energía
  Vij=((yuk*U)/r)

  return
end Subroutine Yukawa
