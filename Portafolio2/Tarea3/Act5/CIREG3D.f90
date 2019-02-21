!Configuración Inicial Regular 3D
!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina
!Parámetros de entrada: Longitud de la celda, concentración
!Parámetros de entrada: # de partículas, # de partículas por lado
Subroutine cireg3D(L, Lr, dens, N, P)
  use constantes3D

  !Declaración de variables
  implicit none

  !L: longitud de la celda de simulación
  !Lr: longitud donde se acomodarán las partículas
  !dens: concentración reducida del sistema
  !d: distancia entre las partículas
  !o: valor de media longitud de celda (para centrar el cubo en el origen)
  real *4 L, Lr, dens, d, o

  !N: número de partículas del sistema
  !P: número de partículas por lado
  integer *4 N, P

  !Distancia entre las partículas para que sean tangentes a la caja
  d=Lr/(real(P)-1)

  !Separando el plano
  o=L/2.0

  !Archivo de datos
  open(1, file='cireg3d.dat')
    !Contador para la posición en x
    do i=1,P
      !Calculamos la posición desde el extremo positivo
      x(i)=o-d*(i-1)-(sig/2.0)
      !Contador para la posición en y
      do j=1,P
        !Calculamos todas las posiciones de y para cada x
        !comenzando del extremo positivo
        y(i)=o-d*(j-1)-(sig/2.0)
        !Contador para la posición en z
        do k=1,P
          !Calculamos todas las posiciones de z para cada x,y
          !comenzando del extremo positivo
          z(i)=o-d*(k-1)-(sig/2.0)
          !escribimos las posiciones regulares en archivo y pantalla
          write(1,*) x(i),y(i),z(i)
        end do
      end do
    end do
  !cerramos el archivo de configuración regular
  close(1)

  return
!fin de la subrutina
end subroutine cireg3D
