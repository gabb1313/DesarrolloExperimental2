!Calculando propiedades de autodifusión
!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina

Subroutine wdt
  use constantes3D
  use variables3D

  !Declaración de variables
  implicit none

  !tim: tiempo entre configuraciones almacenadas
  !time: tiempo real entre configuraciones guardadas
  !      (por la cadencia del loop)
  !dif: coeficiente de difusión
  !cte: constante del desplazamiento cuadrático medio
  !wtx,wty,wtz: desplazamiento cuadrático por coordenada
  !wt: desplazamiento cuadrático medio
  real *4 tim, time, dif, cte
  real *4 wtx, wty, wtz, wt

  !Nmax: Número de partícula máxima para la cadencia temporal
  !i,j,k: contadores del loop
  integer *4 Nmax, i, j, k

  !abrimos el archivo de datos
  open(6, file='wdt.dat')

  !calculando el tiempo entre las configuraciones almacenadas
  tim=real(isave)*dt

  !iniciando el loop para dar cadencia en el barrimiento temporal
  do i=1,cm-1
    !el número máximo de partículas en el avance temporal
    !cambia según la cadencia dda por el loop
    Nmax=cm-i

    !calculando el tiempo real entre configuraciones con el barrimiento
    time=tim*real(i)

    !los desplazamientos cuadráticos en ceros
    wtx=0.0
    wty=0.0
    wtz=0.0
    wt=0.0

    !iniciando el loop de partículas
    do j=1,N
      !iniciando el loop de las configuraciones, respetando
      !el barrimiento temporal dado por i
      do k=1,Nmax
        !calculando el desplazamiento cuadrático por coordenada
        !el barrimiento va incluido al sumar i en las configuraciones
        wtx=wtx+(cxd(j,i+k)-cxd(j,k))**2
        wty=wty+(cyd(j,i+k)-cyd(j,k))**2
        wtz=wtz+(czd(j,i+k)-czd(j,k))**2
      !termina loop de configuraciones
      end do
    !termina loop de partículas
    end do

    !constante propia del desplazamiento cuadrático medio
    cte=6*Nmax*N

    !calculando el desplazamiento cuadrático medio
    wt=(wtx+wty+wtz)/cte

    !calculando el coeficiente de difusión
    dif=wt/time

    !escribimos en el archivo
    write(6,*) time, wt, dif

  !termina loop del barrimiento temporal
  end do

  return
end Subroutine wdt
