!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina
!Parámetros de entrada: Desplazamiento máximo, # de partículas
!Parámetros de entrada: # de configuraciones, longitud de la celda
Subroutine MOV2DcpT(dmax, N, NSTEP, L)
  use constantes2D

  !Declaración de variables
  implicit none

  !L: longitud de la celda de simulación
  !dr: desplazamiento máximo en términos de sigma
  !dmax: desplazamiento máximo en los movimientos
  !rand1,rand2: números aleatorios
  real *4 L, dr, dmax
  real *4 rand1, rand2

  !N: número de partículas del sistema
  !NSTEP: número de configuraciones del sistema
  !T1,T2: partículas trazadoras
  integer *4 N, NSTEP, T1, T2

  !desplazamiento máximo de las partículas
  dr=dmax*sig

  !iniciamos el contador de configuraciones
  do j=1,NSTEP
    !archivo de datos
    open(2, file='ciran2dmov.dat')

    !iniciamos el contador de partículas
    do i=1,N

      !generando números aleatorios
      call random_number(rand1)
      call random_number(rand2)

      !movemos a la i-ésima partícula un desplazamiento aleatorio
      !con tamaño máximo dado por dr
      x(i) = x(i)+(2.0*rand1-1.0)*dr
      y(i) = y(i)+(2.0*rand2-1.0)*dr

      !Condiciones periódicas
      x(i) = x(i)-L*anint(x(i)/L)
      y(i) = y(i)-L*anint(y(i)/L)

      !solo guardamos la configuración final
      if (j==NSTEP) then
        write(2,*) x(i), y(i)
      end if
    end do

    !Trazadoras
    open(3, file='trazadoras.dat')
    !Elegimos trazadoras aleatorias
    T1=rand()*N
    T2=rand()*N
      !Evitamos que la trazadora sea la partícula 0
      if (T1==0.or.T2==0) then
        T1=1
        T2=1
      end if
    !Escribimos las posiciones de las trazadoras
    write(3,*) x(T1), y(T1), x(T2), y(T2)
  end do

    !se cierra el archivo
    close(2)
    close(3)

    write(*,*) 'Partículas tazadoras', T1, T2
    write(*,*) 'Tras', NSTEP, 'ciclos, con movimientos por particula de tamanio maximo', dr

  return
!fin de la subrutina
end subroutine MOV2DcpT
