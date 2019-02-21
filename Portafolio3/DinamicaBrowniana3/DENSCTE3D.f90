!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina
!Parámetros de entrada: # de partículas, longitud de la celda
Subroutine DENSCTE3D
  use constantes3D
  use variables3D

  !Declaración de variables
  implicit none

  !L: longitud de la celda de simulación
  real *4 densf

  !M: número de partículas final
  !i: contador del loop
  integer *4 M, i

  !número final de partículas dentro de la celda
  M=0
  do i=1,N
    if(abs(x(i)).le.(L/2.0).and.abs(y(i)).le.(L/2.0).and.abs(z(i)).le.(L/2.0)) then
      M=M+1
    end if
  end do

  !calculando la nueva concentración reducida
  densf=(sig*real(M))/L**3

  write(*,*) 'Hay', M, 'particulas dentro de la celda inicial'
  write(*,*) 'La nueva concentracion reducida es', densf

  return
!fin de la subrutina
end subroutine DENSCTE3D
