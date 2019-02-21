!Programa principal para simulación con Dinámica Browniana
!DesExpII, 2018-1
!Gabriela Carretas

!El programa elige la configuración incial óptima para poder realizar
!el movimiento de las partículas utilizando el algoritmo de Dinámica Browniana.
!Tras obtener las matrices de configuración, se calculan propiedades
!estructurales como la g(r) y la presión, y propiedades dinámicas como
!la W(t) y el desplazamiento cuadrático medio.

!Iniciando el programa

!Configuración Inicial Random 3D (n*<=1.0)
!Configuración Inicial Regular 3D (n*>1.0)
!Unidades reducidas: sig
program DinBrown
  use constantes3D
  use variables3D

  !Declaración de variables
  implicit none

  !i,j,w: contadores del loop
  integer *4 w, i, j

  !obtenemos valores de entrada

  !print * , 'Numero total de particulas y concentracion reducida'
  !read (*,*) N, dens
  !print * , 'Numero de configuraciones y tiempo de paso'
  !read (*,*) NSTEP, dt
  !print * , 'Frecuencia de muestreo en pantalla y de guardar en el archivo'
  !read (*,*) iprint, isave

  !DATOS PARA CALIBRACIÓN: GAYLOR
  N=400
  NSTEP=16000
  ter=2000
  iprint=1000
  isave=100
  dt=0.0004
  phi=0.00044

  !Parámetros del potencial: YUKAWA
  A=556.0
  zk=0.149
  yuk=A*exp(zk)

  !calculando la concentración reducida en términos de la
  !fracción en volumen
  dens=(6.0*phi)/pi

  !calculando la longitud reducida
  L=((sig*real(N))/dens)**r3

  !calculando el radio de corte convencional
  rcut=L/2.0

  !calculando la varianza
  var=sqrt(2.0*dt)

  !calculando el número total de configuraciones guardadas
  ens=(NSTEP-ter)/isave

  !iniciando los vectores de posición
  allocate(x(1:N))
  allocate(y(1:N))
  allocate(z(1:N))

  !iniciando los vectores de posición dinámicos
  allocate(xd(1:N))
  allocate(yd(1:N))
  allocate(zd(1:N))

  !iniciando las matrices de configuración
  allocate(cx(N,ens))
  allocate(cy(N,ens))
  allocate(cz(N,ens))

  !iniciando las matrices de configuración dinámicas
  allocate(cxd(N,ens))
  allocate(cyd(N,ens))
  allocate(czd(N,ens))

  !iniciando los vectores de fuerzas
  allocate(fx(1:N))
  allocate(fy(1:N))
  allocate(fz(1:N))

  !si la n* es mayor que 1.0 (Recuerda al RCP) elige una regular
  if (dens.gt.1.0) then
    call CIREG3D
  !si es menor, comienza con una  aleatoria
  else
    call CIRAN3D
  end if

  !comenzar en ceros los contadores del algoritmo
  cm=0

  !abrimos los archivos
  open(2, file='cf.dat')

  write(*,*) 'Longitud reducida:',L
  write(*,*) 'Concentración reducida:',dens
  write(*,*) 'Numero de configuraciones a guardar:',ens
  write(*,*) '# de configuracion || E. Potencial normalizada'

  !iniciamos el contador de la configuración
  do w=1,NSTEP

    !llamamos a la subrutina de fuerzas
    call FUERZAS(w)

    !iniciamos el contador de las partículas
    do i=1,N
      !llamando a la subrutina del algoritmo de Ermak
      call ERMAK(i)
      !solo guardamos la configuración final
      if (w.eq.NSTEP) then
        write(2,*) x(i), y(i), z(i)
      end if
    !termina loop de partículas
    end do

    if(mod(w,iprint).eq.0.0) then
      !escribimos en pantalla
      write(*,*) w, Vnor
    end if

    !Decisión para guardar en arreglos
    if (w.gt.ter.and.mod(w,isave).eq.0.0) then
      cm=cm+1
      do j=1,N
        !guardando configuraciones de equilibrio (estructurales)
        cx(j,cm)=x(j)
        cy(j,cm)=y(j)
        cz(j,cm)=z(j)
        !guardando configuraciones de equilibrio (dinámicas)
        cxd(j,cm)=xd(j)
        cyd(j,cm)=yd(j)
        czd(j,cm)=zd(j)
      end do
    end if

  !termina loop de configuraciones
  end do

  !llamando a la subrutina del cálculo de la concentración final
  call DENSCTE3D

  !llamando a la subrutina del cálculo de la g(r)
  call gr3D

  !llamando a la subrutina del cálculo de la w(t)
  call wdt

  !cerramos el archivo
  close(2)

  !desocupamos los arreglos
   deallocate(x)
   deallocate(y)
   deallocate(z)
   deallocate(xd)
   deallocate(yd)
   deallocate(zd)
   deallocate(cx)
   deallocate(cy)
   deallocate(cz)
   deallocate(cxd)
   deallocate(cyd)
   deallocate(czd)
   deallocate(fx)
   deallocate(fy)
   deallocate(fz)

!fin del programa
end program DinBrown
