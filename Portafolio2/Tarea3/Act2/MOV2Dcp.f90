!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina
!Parámetros de entrada: Desplazamiento máximo, # de partículas
!Parámetros de entrada: # de configuraciones, longitud de la celda
Subroutine MOV2Dcp(dmax, N, NSTEP, L)
  use constantes2D

  !Declaración de variables
  implicit none

  !L: longitud de la celda de simulación
  !dr: desplazamiento máximo en términos de sigma
  !dmax: desplazamiento máximo en los movimientos
  real *4 L, dr, dmax

  !N: número de partículas del sistema
  !NSTEP: número de configuraciones del sistema
  integer *4 N, NSTEP

  !desplazamiento máximo de las partículas
  dr=dmax*sig

  !iniciamos el contador de configuraciones
  do j=1,NSTEP
    !archivo de datos
    open(2, file='ciran2dmov.dat')

    !iniciamos el contador de partículas
    do i=1,N
      !movemos a la i-ésima partícula un desplazamiento aleatorio
      !con tamaño máximo dado por dr
      x(i) = x(i)+(2.0*rand()-1.0)*dr
      y(i) = y(i)+(2.0*rand()-1.0)*dr

      !Condiciones periódicas
      x(i) = x(i)-L*anint(x(i)/L)
      y(i) = y(i)-L*anint(y(i)/L)

      !solo guardamos la configuración final
      if (j==NSTEP) then
        write(2,*) x(i), y(i)
      end if

    end do
  end do

    !se cierra el archivo
    close(2)

    write(*,*) 'Tras', NSTEP, 'ciclos, con movimientos por particula de tamanio maximo', dr

  return
!fin de la subrutina
end subroutine MOV2Dcp
