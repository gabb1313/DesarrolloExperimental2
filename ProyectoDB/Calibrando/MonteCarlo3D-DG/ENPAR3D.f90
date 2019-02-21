!Energía de la I-ÉSIMA PARTÍCULA
!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina
!Parámetros de entrada: posiciones de referencia
!Parámetros de entrada: contador de partícula
Subroutine ENPAR3D(x1, y1, z1, i, V1)
  use constantes3D
  use variables3D

  !Declaración de variables
  implicit none

  !x1,y1,z1: posiciones iniciales
  !xt,yt,zt: distancia entre la i-ésima y j-ésima
  !Vij: energía potencial entre partículas
  !r: distancia entre las partículas
  !V1: energía potncial de entrada
  real *4 x1, y1, z1, xt, yt, zt, Vij, r, V1

  !i,j: contador del loop
  integer *4 i, j

  !la energía potencial inicial de la configuración es cero
  V1=0.0
  Vij=0.0

    !comienza contador de la partícula de comparación
    do j=1,N
      !condición que evita la autointeracción
      if (i.ne.j) then
        !distancia entre la i-ésima y j-ésima
        xt=x1-x(j)
        yt=y1-y(j)
        zt=z1-z(j)

        !condiciones periódicas
        xt = xt-L*anint(xt/L)
        yt = yt-L*anint(yt/L)
        zt = zt-L*anint(zt/L)

        !calculando la distancia entre partículas
        r=sqrt((xt)**2+(yt)**2+(zt)**2)

        !solo considera contribuciones dentro del rcut
        if(r.le.rcut) then
          !calculando la energía con el potencial de interacción
          call DG(r,Vij)
          !suma la contribución de energía
          V1=V1+Vij
        end if

      end if
    !termina el contador de la partícula en comparación
    end do

  return
end subroutine ENPAR3D
