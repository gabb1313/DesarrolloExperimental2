!Tarea 3 - Actividad 4
!DesExpII, 2018-1
!Gabriela Carretas

!A partir de una CIRAN3D, mueve las N partículas de manera
!aleatoria un número de ciclos NSTEP. Se incluyen las condiciones
!periódicas en el sistema para mantener la misma concentración. Tres
!trazadoras eligen partículas aleatorias para seguir su movimiento.

!Iniciando el programa

!Configuración Inicial Random 3D
!Unidades reducidas: sig
program ciran3DmovcpT
  use constantes3D

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
  allocate(z(1:N))

  !calculando la concentración reducida en términos de la
  !fracción en volumen
  dens=(6.0*phi)/pi

  !calculando la longitud reducida
  L=((sig*real(N))/dens)**r3

  !longitud a utilizar para acomodar todas las partículas dentro
  !de la celda de simulación
  Lr=L-sig

  !llamando a la subrutina de la configuración inicial aleatoria 3D
  call CIRAN3D(Lr, dens, N)

  write(*,*) 'Para la fraccion en volumen de', phi
  write(*,*) 'Tenemos una concentracion reducida inicial de', dens
  write(*,*) 'Y con', N, 'particulas, la longitud reducida es', L

  !llamando a la subrutina para mover a las partículas (con condiciones periódicas)
  call MOV3D(dmax, N, NSTEP, L)

  !llamando a la subrutina para calcular la densidad final del sistema
  call DENSCTE3D(N, L)

  !desocupamos los vectores de posición tras usarlos
  deallocate(x)
  deallocate(y)
  deallocate(z)

!fin del programa
end program ciran3DmovcpT
