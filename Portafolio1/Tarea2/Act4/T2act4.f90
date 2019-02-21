!Tarea 2 - Actividad 4
!DesExpII, 2018-1
!Gabriela Carretas

!El programa coloca N partículas separadas uniformemente
!en un cubo de longitud de celda L* obtenida en base a la
!concentración n* y al número de partículas. El origen se
!localiza en el punto medio del plano.

!Iniciando el programa

!Configuración Inicial Regular 3D
!Parámetro de entrada: N(número de partículas)
!Parámetro de entrada: dens(concentración reducida)
!Unidades reducidas: sig
program cubo

  !Declaración de variables
  implicit none

  !r3: exponente para celdas 3D
  !sig: diámetro de las partículas
  real *4, parameter :: r3=1.0/3.0
  real *4, parameter :: sig=1.0

  !d: distancia entre las partículas
  !x,y,z: posición de las partículas en la celda
  !L: longitud de la celda de simulación
  !dens: concentración reducida del sistema
  !o: valor de media longitud de celda (para centrar el cubo en el origen)
  real *4 d, x, y, z, L, dens, o

  !i,j,k: contadores del loop
  !N: número de partículas del sistema
  !M: número de partículas por lado
  integer *4 i, j, k, N, M

  !obtenemos valores de entrada
  print * , 'Numero total de particulas y concentracion reducida'
  read (*,*) N, dens

  !debe tener raiz cúbica para que si sea un arreglo regular
  !si la raiz cubica de N no es entero, entonces M será distinta de N
  M=N**r3

  !alertamos al usuario que cambie el número de partículas
  !el loop no termina hasta que tenga un número con raiz exacta
  do
    if (M**3.NE.N) then
      print * , 'Numero total de particulas no tiene raiz cubica. Ingrese otro numero'
      read (*,*) N
      M=N**r3
    else if (M**3.EQ.N) then
      exit
    end if
  end do

  !número de partículas por lado
  M=N**r3

  !calculando la longitud reducida
  L=((sig*real(N))/dens)**r3

  !Distancia entre las partículas para que sean tangentes a la caja
  d=L/real(M)

  !Separando el plano
  o=L/2.0

  !Archivo de datos
  open(1, file='cireg3d.dat')
    !Contador para la posición en x
    do i=1,M
      !Calculamos la posición desde el extremo positivo
      x=o-d*(i-1)-d/2.0
      !Contador para la posición en y
      do j=1,M
        !Calculamos todas las posiciones de y para cada x
        !comenzando del extremo positivo
        y=o-d*(j-1)-d/2.0
        !Contador para la posición en z
        do k=1,M
          !Calculamos todas las posiciones de z para cada x,y
          !comenzando del extremo positivo
          z=o-d*(k-1)-d/2.0
          !escribimos las posiciones regulares en archivo y pantalla
          write(1,*) x,y,z
        end do
      end do
    end do
  !cerramos el archivo de configuración regular
  close(1)

  write(*,*) 'A una concentracion reducida de', dens
  write(*,*) 'y con', N, 'particulas, la longitud reducida es', L

!fin del programa
end program cubo
