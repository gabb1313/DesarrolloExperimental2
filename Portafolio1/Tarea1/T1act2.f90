!Tarea 1 - Actividad 2
!DesExpII, 2018-1
!Gabriela Carretas

!El programa coloca N partículas en una recta de longitud L
!separadas uniformemente. El origen se localiza en el punto
!medio de la recta; las partículas etiquetadas con números
!pares se localizan en posiciones positivas, y las impares
!en las negativas.

!Iniciando el programa
program recta
  implicit none
  real *4 d, x, L, o, M
  integer *4 i, ip, iimp, N

    print * , 'Numero de particulas y longitud de la recta'
    read (*,*) N, L

 !Distancia entre las partículas
 d=L/(N-1)

 !Separando la recta en dos
 o=L/2

 !Archivo de datos
  open(1, file='recta.dat')

 !El primer if diferencía entre N par/impar, para que acomode
 !o no una partícula en el origen (arreglo simétrico)

 !Analizando caso para N par
  if (mod(N,2)==0) then
    !contador para la n-ésima partícula
    do i=1,N
    M=real(N)
    !Contador de pares/impares
    ip=i/2
    iimp=(i+1)/2
      !Si es par, entonces la acomoda en el extremo derecho
      if (mod(i,2)==0) then
        x=o-d*(M/2-ip)
      !Si es impar, la acomoda en el extremo izquierdo
      else
        x=-o+d*(M/2-iimp)
      end if
      write(1,*) i,x
      write(*,*) i,x
    end do

  !Analizando caso para N impar
  else
    do i=1,N
    M=real(N)
    !Contador de pares/impares
    ip=i/2
    iimp=(i+1)/2
      !Si es par, entonces la acomoda en el extremo derecho
      if (mod(i,2)==0) then
        x=o-d*((M-1)/2-ip)
      else
      !Si es impar, la acomoda en el extremo izquierdo
        x=-o+d*((M+1)/2-iimp)
      end if
      write(1,*) i,x
      write(*,*) i,x
    end do
  end if

  !cerramos el archivo
  close(1)

!fin del programa
end program recta
