!Calculando un número aleatorio con distribución gaussiana
!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina


!Parámetro de entrada: variable a la que se le asignará el número
Subroutine RANGAUSS(numero)
  use constantes3D

  !Declaración de variables
  implicit none

  !rand1,rand2: números aleatorios con distribución uniforme
  !numero: número aleatorio con distribución gaussiana
  real *4 rand1, rand2, numero

  !generando números aleatorios uniformes
12  call random_number(rand1)
    call random_number(rand2)

  !para evitar que diverja el logaritmo al evaluarlo en rand1
  if (rand1.le.1E-8) then
    go to 12
  end if

  !generando un número aleatorios con distribución gaussiana
  numero=sqrt(-2.0*log(rand1))*cos(2.0*pi*rand2)

  return
end subroutine RANGAUSS
