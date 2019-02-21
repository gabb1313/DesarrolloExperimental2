!Energía de la I-ÉSIMA PARTÍCULA
!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina
!Parámetros de entrada: posiciones de referencia, contador de partícula
!Parámetros de entrada: radio de corte, energía potencial
!Parámetros de entrada: longitud de la celda, # de partículas
Subroutine ENPAR2D(xt, yt, z, rcut, V, L, N)
  use constantes2D

  !Declaración de variables
  implicit none

  !L: longitud de la celda de simulación
  !rcut: radio de corte
  !V: energía potencial total
  !x1,y1: posiciones iniciales
  !xt,yt: distancia entre la i-ésima y j-ésima
  !r: distancia entre las partículas
  !Vij: potencial entre partículas
  real *4 L, rcut, V
  real *4 x1, y1, xt, yt, r, Vij

  !N: número de partículas
  !z: contador de partícula
  integer *4 N, z

  !la energía potencial inicial de la configuración es cero
  V=0.0

    !comienza contador de la partícula de comparación
    do j=1,N
      !condición que evita la autointeracción
      if (z.ne.j) then
        !distancia entre la i-ésima y j-ésima
        x1=xt-x(j)
        y1=yt-y(j)

        !condición de imagen minima
        x1 = x1-L*anint(x1/L)
        y1 = y1-L*anint(y1/L)

        r=(xt)**2+(yt)**2

        !POTENCIAL DE INTERACCIÓN
        !ESFERA DURA

        !solo considera contribuciones dentro del rcut
        if(r.le.rcut) then
          if(r.lt.sig)then
            Vij=1.0E+10
          else
            Vij=0.0
          end if

          !suma la contribución de energía
          V=V+Vij
        end if
      end if

    end do

  return
end subroutine ENPAR2D
