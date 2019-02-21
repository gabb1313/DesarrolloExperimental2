!Cálculo de la fuerza de interacción entre
!partículas y energía de la configuración
!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina
!Parámetros de entrada: contador de la configuración
Subroutine FUERZAS(conf)
  use constantes3D
  use variables3D

  !Declaración de variables
  implicit none

  !V: energía potencial total
  !Vij: energía potencial por partícula
  !xt,yt,zt: distancia entre la k-ésima y j-ésima
  !r: distancia entre las partículas
  !fx1,fy1,fz1: fuerzas por componente
  !fxt,fyt,fzt: fuerzas por pareja
  !U: forma funcional del potencial
  !U2: parámetro general para cálculo de la fuerza (-1/r)(dU/dr)
  !pre: contribución a la presión del jk-ésimo par
  !vol: volumen de la caja
  !time: tiempo recorrido para la configuración actual
  real *4 V, Vij, xt, yt, zt, r
  real *4 fx1, fy1, fz1, fxt, fyt, fzt
  real *4 U, U2
  real *4 pre, vol
  real *4 time

  !conf: contador de la configuración
  !j,k: contadores del loop
  integer *4 conf
  integer *4 j, k

  open(3, file='terma.dat')
  open(4, file='presion.dat')

  !valores iniciales en ceros
  V=0.0
  pre=0.0
  Vij=0.0

  !calculando el tiempo de la configuración
  time=dt*conf

  !iniciando arreglos de fuerzas en ceros
  do k=1,N
    fx(k)=0.0
    fy(k)=0.0
    fz(k)=0.0
  end do

  !comienza contador de la partícula de referencia
  do k=1,N-1
    !posiciones iniciales
    fx1=fx(k)
    fy1=fy(k)
    fz1=fz(k)
    !comienza contador de la partícula de comparación
    do j=k+1,N
      !distancia por componente entre la k-ésima y j-ésima
      xt=x(k)-x(j)
      yt=y(k)-y(j)
      zt=z(k)-z(j)

      !condición de imagen mínima
      xt=xt-L*anint(xt/L)
      yt=yt-L*anint(yt/L)
      zt=zt-L*anint(zt/L)

      !distancia entre la k-ésima y la j-ésima
      r=sqrt((xt)**2+(yt)**2+(zt)**2)

      !verificando traslapes (si hay muchos->BAJAR DT)
      !if (r.le.1.0) then
      !  write(*,*) 'Traslape entre las partículas', k, j
      !end if

      !POTENCIAL DE INTERACCIÓN
      !Doble Gaussiana

      !solo considera contribuciones dentro del rcut
      if(r.lt.rcut) then

        !llamando a la subrutina del cálculo del potencial
        call FDG(r,Vij,U2)

        !calculando la contribución a la presión del sistema
        pre=pre-r*r*U2

        !calculando la energía potencial por partícula
        !sumando la contribución de energía
        V=Vij+V

        !cálculo de las fuerzas por pareja
        fxt=xt*U2
        fyt=yt*U2
        fzt=zt*U2

        !fuerza total por componente
        fx1=fx1+fxt
        fy1=fy1+fyt
        fz1=fz1+fzt

        !fuerza par "volteada" (usando tercera ley de Newton)
        fx(j)=fx(j)-fxt
        fy(j)=fy(j)-fyt
        fz(j)=fz(j)-fzt
      end if
    !termina el loop de partícula de comparación
    end do

    !fuerza volteada ya calculada
    fx(k)=fx1
    fy(k)=fy1
    fz(k)=fz1

  !termina loop de partícula de referencia
  end do

  !calculando la energía potencial reducida
  Vnor=V/real(N)

  !volumen de la caja
  vol=L*L*L

  !calculando la presión del sistema
  presion=dens-(1.0/(3.0*vol))*pre

  !escribimos en el archivo la curva de termalización
  write(3,*) conf, time, Vnor
  write(4,*) conf, time, presion

  return
end subroutine FUERZAS
