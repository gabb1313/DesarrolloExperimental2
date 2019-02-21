!Configuración Inicial Regular 3D
!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina

Subroutine cireg2D(L, N)
  use constantes2D
  use variables

  !Declaración de variables
  implicit none

  !L: longitud de la celda de simulación
  !Lr: longitud donde se acomodarán las partículas
  !dens: concentración reducida del sistema
  !d: distancia entre las partículas
  !o: valor de media longitud de celda (para centrar el cubo en el origen)
  real *4 L, d, o, xx

  !N: número de partículas del sistema
  !P: número de partículas por lado
  integer *4 N, P

  !debe tener raiz cuadrada para que si sea un arreglo regular
  !si la raiz cuadrada de N no es entero, entonces M será distinta de N
  P=N**r2

  !alertamos al usuario que cambie el número de partículas
  !el loop no termina hasta que tenga un número con raiz exacta
  do
    if (P**2.ne.N) then
      print * , 'Numero total de particulas no tiene raiz cuadrada. Ingrese otro numero'
      read (*,*) N
      P=N**r2
    else
      exit
    end if
  end do

  !Número de partículas por lado
  P=N**r2

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
        k=k+1.0
        x(k)=xx
        !Calculamos todas las posiciones de y para cada x
        !comenzando del extremo positivo
        y(k)=o-d*(j-1)-(sig/2.0)
        write(1,*) x(k), y(k)
      end do
    end do
  !cerramos el archivo de configuración regular
  close(1)

  return
!fin de la subrutina
end subroutine cireg2D
