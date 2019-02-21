!Configuración Inicial Aleatoria 2D
!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina
!Parámetros de entrada: Longitud de la celda, concentración, # de partículas
Subroutine ciran2D(Lr, dens, N)
  use constantes2D

  !Declaración de variables
  implicit none

  !Lr: longitud donde se acomodarán las partículas
  !dens: concentración reducida del sistema
  !xt,yt: distancia en x,y entre partículas
  !r: distancia radial entre partículas
  real *4 Lr, dens, xt, yt, r

  !N: número de partículas del sistema
  integer *4 N

  !contador para la posición de la n-ésima partícula
  do i=1,N
    !archivo de datos
    open(1, file='ciran2d.dat')
      !se asigna la posición aleatoriamente
52    x(i)=(rand()-0.5)*Lr
      y(i)=(rand()-0.5)*Lr
      !contador que compara la i-ésima partícula con la
      !j-ésima, comprobando que no se traslapen
    55 do j=1,i-1
      !calculando el vector posición rij
      xt=x(i)-x(j)
      yt=y(i)-y(j)
      r=(xt)**2+(yt)**2
      !si r<=sigma entonces hay traslape y vuelve a asignar
      !una posición aleatoria
      if(r.le.sig) then
        go to 52
      end if
    end do
    !escribimos las posiciones aleatorias en el archivo
    write(1,*) x(i), y(i)
  end do
  !se cierra el archivo
  close(1)

  return
!fin de la subrutina
end subroutine ciran2D
