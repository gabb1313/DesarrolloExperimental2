!Potencial de interacción: Esfera dura

!Parámetro de entrada: distancia radial entre partículas
!Parámetro de entrada: energía potencial entre partículas
Subroutine HS(r,Vij)
  use variables3D
  use constantes3D

  implicit none

  !r: distancia entre partículas
  !Vij: energía potencial entre partículas
  real *4 r, Vij

  if(r.lt.sig)then
    Vij=1.0E+10
  else
    Vij=0.0
  end if

  return
end Subroutine HS
