!Algoritmo de MonteCarlo
!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina
!Parámetros de entrada: diferencia de energías, energía potencial
!Parámetros de entrada: posiciones tentativas nuevas, # de aceptados de MC
Subroutine MC(dV, V, xn, yn, amc)
  use constantes2D
  use variables

  !Declaración de variables
  implicit none

  !dV: diferencia de energías
  !xn,yn: posiciones nuevas
  !V: energía potencial
  !FB: factor de Boltzmann
  real *4 dV, xn, yn, V, FB, rand3

  !amc: número de aceptados de MonteCarlo
  integer *4 amc

  !generando un número aleatorio
  call random_number(rand3)

  !si la energía nueva es menor que un número muy grande,
  !entonces MC acepta el movimiento
  if (dV.lt.75.0) then
    !si la energía nueva es menor que 0, se acepta el movimiento
    if(dV.le.0.0) then
      !suma la contribución de energía
      V=V+dV
      !renombra las posiciones
      x(i)=xn
      y(i)=yn
      !aumenta el contador de aceptados
      amc=amc+1
    !si la energia nueva está entre 0 y 75, se evalua el factor de Boltzmann
    !y se compara con un número aleatorio para aceptar o rechazar el movimiento
    else if (FB(dV).gt.rand3) then
      !suma la contribución de energía
      V=V+dV
      !renombra las posiciones
      x(i)=xn
      y(i)=yn
      !aumenta el contador de aceptados
      amc=amc+1
    end if
  end if

  return
end subroutine MC

!función del factor de Boltzmann
real function FB(factor)
  implicit none
  real*4 factor
  FB = exp(-factor)
  return
end function FB