!Configuración Inicial Regular 3D
!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina

Subroutine cireg3D
  use constantes3D
  use variables3D

  !Declaración de variables
  implicit none

  !Lr: longitud donde se acomodarán las partículas
  !dens: concentración reducida del sistema
  !d: distancia entre las partículas
  !o: valor de media longitud de celda (para centrar el cubo en el origen)
  real *4 d, o, xx, yy

  !P: número de partículas por lado
  !i,j,w: contadores del loop
  !k: contadores de la posición real de la partícula
  integer *4 P, i, j, w, k

  !debe tener raiz cuadrada para que si sea un arreglo regular
  !si la raiz cuadrada de N no es entero, entonces M será distinta de N
  P=N**r3

  !alertamos al usuario que cambie el número de partículas
  !el loop no termina hasta que tenga un número con raiz exacta
  do
    if (P**3.ne.N) then
      print * , 'Numero total de particulas no tiene raiz cúbica. Ingrese otro numero'
      read (*,*) N
      P=N**r3
    else
      exit
    end if
  end do

  !Número de partículas por lado
  P=N**r3

  !Distancia entre las partículas para que sean tangentes a la caja
  d=(L-sig)/(real(P)-1)

  !Separando el plano
  o=L/2.0

  k=0.0
  !Archivo de datos
  open(1, file='ci.dat')
    !Contador para la posición en x
    do i=1,P
      !Calculamos la posición desde el extremo positivo
      xx=o-d*(i-1)-(sig/2.0)
      !Contador para la posición en y
      do j=1,P
        !Calculamos todas las posiciones de y para cada x
        !comenzando del extremo positivo
        yy=o-d*(j-1)-(sig/2.0)
        !Contador para la posición en z
        do w=1,P
          !contador de la posición real en el arreglo
          k=k+1.0
          x(k)=xx
          y(k)=yy
          !Calculamos todas las posiciones de z para cada x,y
          !comenzando del extremo positivo
          z(k)=o-d*(w-1)-(sig/2.0)
          write(1,*) x(k), y(k), z(k)
          !guardamos en los vectores dinámicos las posiciones iniciales
          xd(k)=x(k)
          yd(k)=y(k)
          zd(k)=z(k)
        end do
      end do
    end do
  !cerramos el archivo de configuración regular
  close(1)

  return
!fin de la subrutina
end subroutine cireg3D
