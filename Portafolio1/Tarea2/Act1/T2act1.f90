!Tarea 2 - Actividad 1
!DesExpII, 2018-1
!Gabriela Carretas

!El programa estima el valor del número "pi" utilizando un
!generador de números aleatorios. Calcula el error relativo
!y la desviación con respecto al valor de pi con 4 decimales

!Iniciando el programa

program estimandopi

  !Declaración de variables

  implicit none

  !x,y: posición del frijol en el cuadro de 1*1
  !r: distancia del origen al frijol
  !pi: número estimado de pi
  !e: error relativo
  !despi: desviación respecto al valor 3.1415
  real *8 x, y, r, pi, e, despi

  !i,j: contadores del loop
  !N: número total inicial del frijoles
  !M: número de frijoles dentro del pedazo círculo
  !K: número máximo que puede tomar N para estudiar el
  !   comportamiento de la estimación del número pi
  integer *8 N, i, M, j, K

  !pir: valor tomado como base para pi
  real, parameter :: pir=3.1415

  !obtenemos valores de entrada
  print * , 'Numero de frijoles a utilizar (inicialmente)'
  read *, N

  !seleccionamos el número máximo de N
  K=N*10000

    !abrimos el archivo de N vs. pi
    open(2, file='Npi.dat')

    !correrá desde el valor inicial de N hasta K, aumentando de 100 en 100
    do j=1,K,1000
      !abrimos el archivo de e vs. pi
      open(1, file='epi.dat')
        !inicia con 0 frijoles en el pedazo de círculo
        M=0

        !correrá desde 1 hasta N frijoles
        do i=1,N
          !asignando la posición en el cuadrante aleatoriamente
          x=rand()
          y=rand()
          !se calcula su distancia respecto al origen
          r=(x)**2+(y)**2
          !si la distancia es menor al radio unitario, entonces cae dentro
          !del círculo y aumentamos el contador respectivo
          if(r.lt.1) M = M+1
        end do

        !con N y M fijos, calculamos el valor estimado de pi
        pi=(4*real(M))/real(N)
        !la desviación respecto al valor 'real'
        despi=abs(pi-pir)
        !y el error relativo
        e=(despi/pir)*100
        !se escribe el valor de N y del error en el archivo 1
        write(1,*) N, e
      !se escribe el valor de N y de pi estimado en el archivo 2
      write(2,*) N, pi
    !aumentamos el valor de N
    N=N+j
    if (N.GE.1E7) then
      exit
    end if
    end do

    !se cierran los archivos de datos
    close(1)
    close(2)

!Fin del programa
end program estimandopi
