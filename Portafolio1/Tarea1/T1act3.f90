!Tarea 1 - Actividad 3
!DesExpII, 2018-1
!Gabriela Carretas

!El programa coloca N partículas separadas uniformemente
!en un cubo de longitud de celda L. El origen se localiza
!en el punto medio del plano.

!Iniciando el programa

!Configuración Inicial Regular 2D
!Parámetro de entrada: N(número de partículas)
!Parámetro de entrada: L(longitud del lado de la celda)
program plano

  !Declaración de variables
  implicit none

  !r2: exponente para celdas 2D
  real *4, parameter :: r2=1.0/2.0

  !d: distancia entre las partículas
  !x,y: posición de las partículas en la celda
  !L: longitud de la celda de simulación
  !o: valor de media longitud de celda (para centrar el plano en el origen)
  real *4 d, x, y, L, o

  !i,j: contadores del loop
  !N: número de partículas del sistema
  !M: número de partículas por lado
  integer *4 i, j, N, M

  !obtenemos valores de entrada
  print * , 'Numero de particulas y longitud de la red'
  read (*,*) N, L

  !debe tener raiz cuadrada para que si sea un arreglo regular
  !si la raiz cuadrada de N no es entero, entonces M será distinta de N
  M=N**r2

  !alertamos al usuario que cambie el número de partículas
  !el loop no termina hasta que tenga un número con raiz exacta
  do
    if (M**2.NE.N) then
      print * , 'Numero total de particulas no tiene raiz cuadrada. Ingrese otro numero'
      read (*,*) N
      M=N**r2
    else if (M**2.EQ.N) then
      exit
    end if
  end do

  !número de partículas por lado
  M=N**r2

  !Distancia entre las partículas para que sean tangentes a la caja
  d=L/real(M)

  !Separando el plano
  o=L/2.0

  !Archivo de datos
  open(1, file='cireg2d.dat')
    !Contador para la posición en x
    do i=1,M
      !Calculamos la posición desde el extremo positivo
      x=o-d*(i-1)-d/2
      !Contador para la posición en y
      do j=1,M
        !Calculamos todas las posiciones de y para cada x
        !comenzando del extremo positivo
        y=o-d*(j-1)-d/2
        !escribimos las posiciones regulares en archivo y pantalla
        write(1,*) x,y
      end do
    end do
  !cerramos el archivo de configuración regular
  close(1)

!fin del programa
end program plano
