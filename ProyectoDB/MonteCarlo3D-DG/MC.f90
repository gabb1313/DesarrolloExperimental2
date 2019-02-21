!Algoritmo de MonteCarlo
!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina
!Parámetros de entrada: diferencia de energías, energía potencial
!Parámetros de entrada: posiciones tentativas nuevas, # de aceptados de MC
Subroutine MC(dV, xn, yn, zn, i, amc, V)
  use constantes3D
  use variables3D

  !Declaración de variables
  implicit none

  !dV: diferencia de energías
  !xn,yn,zn: posiciones nuevas
  !FB: factor de Boltzmann
  real *4 dV, xn, yn, zn, FB, rand4, V

  !amc: número de aceptados de MonteCarlo
  !i: contador del arreglo
  integer *4 amc, i

  !generando un número aleatorio
  call random_number(rand4)

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
      z(i)=zn
      !aumenta el contador de aceptados
      amc=amc+1
    !si la energia nueva está entre 0 y 75, se evalua el factor de Boltzmann
    !y se compara con un número aleatorio para aceptar o rechazar el movimiento
    else if (exp(-dV).gt.rand4) then
      !suma la contribución de energía
      V=V+dV
      !renombra las posiciones
      x(i)=xn
      y(i)=yn
      z(i)=zn
      !aumenta el contador de aceptados
      amc=amc+1
    end if
  end if

  return
end subroutine MC

!función del factor de Boltzmann
!real function FB(factor)
!  implicit none
!  real*4 factor
!  FB = exp(-factor)
!  return
!end function FB
