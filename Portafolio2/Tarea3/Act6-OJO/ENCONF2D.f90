!Energía de la configuración
!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina
!Parámetros de entrada: radio de corte, energía potencial
!Parámetros de entrada: longitud de la celda, # de partículas
Subroutine ENCONF2D(rcut, V, L, N)
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
  integer *4 N

    !comienza contador de la partícula de referencia
    do i=1,N-1
    !posiciones iniciales
    x1=x(i)
    y1=y(i)
      !comienza contador de la partícula de comparación
      do j=i+1,N
        !distancia entre la i-ésima y j-ésima
        xt=x1-x(j)
        yt=y1-y(j)

        !condiciones periódicas
        xt = xt-L*anint(xt/L)
        yt = yt-L*anint(yt/L)

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

      end do
  end do

  return
end subroutine ENCONF2D
