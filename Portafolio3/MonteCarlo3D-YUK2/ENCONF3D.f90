!Energía de la configuración
!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina
Subroutine ENCONF3D(V)
  use constantes3D
  use variables3D

  !Declaración de variables
  implicit none

  !x1,y1,z1: posiciones iniciales
  !xt,yt,zt: distancia entre la i-ésima y j-ésima
  !r: distancia entre las partículas
  real *4 x1, y1, z1, xt, yt, zt, r, Vij, V

  !i,j: contadores del loop
  integer *4 i, j

  !iniciando energía entre partículas en ceros
  Vij=0.0

  !comienza contador de la partícula de referencia
  do i=1,N-1
    !posiciones iniciales
    x1=x(i)
    y1=y(i)
    z1=z(i)
    !comienza contador de la partícula de comparación
    do j=i+1,N
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
        call Yukawa(r,Vij)
        !suma la contribución de energía
        V=V+Vij
      end if

    end do
  end do

  return
end subroutine ENCONF3D
