!Tarea 3 - Actividad 1
!DesExpII, 2018-1
!Gabriela Carretas

!A partir de una CIRAN2D, mueve las N partículas de manera
!aleatoria un número de ciclos NSTEP (SIN CONDICIONES PERIÓDICAS)

!Iniciando el programa

!Configuración Inicial Random 2D
!Unidades reducidas: sig
program ciran2Dmov
  use constantes2D

  !Declaración de variables
  implicit none

  !L: longitud de la celda de simulación
  !Lr: longitud donde se acomodarán las partículas
  !dens: concentración reducida del sistema
  !phi: fracción en volumen del sistema
  !dmax: desplazamiento máximo en los movimientos
  real *4 L, Lr, dens, phi, dmax

  !N: número de partículas del sistema
  !NSTEP: número de configuraciones del sistema
  integer *4 N, NSTEP

  !obtenemos valores de entrada
  print * , 'Numero total de particulas y fraccion en volumen'
  read (*,*) N, phi
  print * , 'Numero total de configuraciones y desplazamiento maximo'
  read (*,*) NSTEP, dmax

  !iniciando los vectores de posición
  allocate(x(1:N))
  allocate(y(1:N))

  !calculando la concentración reducida en términos de la
  !fracción en volumen
  dens=(4.0*phi)/pi

  !calculando la longitud reducida
  L=((sig*real(N))/dens)**r2

  !longitud a utilizar para acomodar todas las partículas dentro
  !de la celda de simulación
  Lr=L-sig

  !llamando a la subrutina de la configuración inicial aleatoria 2D
  call CIRAN2D(Lr, dens, N)

  write(*,*) 'Para la fraccion en volumen de', phi
  write(*,*) 'Tenemos una concentracion reducida inicial de', dens
  write(*,*) 'Y con', N, 'particulas, la longitud reducida es', L

  !llamando a la subrutina para mover a las partículas
  call MOV2D(dmax, N, NSTEP)

  !llamando a la subrutina para calcular la densidad final del sistema
  call DENSCTE2D(N, L)

  !desocupamos los vectores de posición tras usarlos
  deallocate(x)
  deallocate(y)

!fin del programa
end program ciran2Dmov
