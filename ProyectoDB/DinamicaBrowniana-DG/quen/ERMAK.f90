!Algoritmo de Ermak
!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina
!Parámetros de entrada: contador de la partícula
Subroutine ERMAK(c)
  use constantes3D
  use variables3D

  !Declaración de variables
  implicit none

  !agx,agy,agz: número aleatorio con distribución gaussiana
  real *4 agx, agy, agz

  !c: contador de la partícula
  integer *4 c

  !generando números aleatorios con distribución gaussiana
  call RANGAUSS(agx)
  call RANGAUSS(agy)
  call RANGAUSS(agz)

  !aplicando la ecuación de Langevin para el movimiento browniano
  !con el algoritmo de Ermak para el desplazamiento
  x(c)=x(c)+fx(c)*dt+var*agx
  y(c)=y(c)+fy(c)*dt+var*agy
  z(c)=z(c)+fz(c)*dt+var*agz

  xd(c)=xd(c)+fx(c)*dt+var*agx
  yd(c)=yd(c)+fy(c)*dt+var*agy
  zd(c)=zd(c)+fz(c)*dt+var*agz

  !condiciones periódicas (SOLO PARA PROPIEDADES ESTRUCTURALES)
  x(c)=x(c)-L*anint(x(c)/L)
  y(c)=y(c)-L*anint(y(c)/L)
  z(c)=z(c)-L*anint(z(c)/L)

  return
end subroutine ERMAK
