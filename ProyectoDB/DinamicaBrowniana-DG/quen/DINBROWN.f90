!Programa principal para simulación con Dinámica Browniana
!DesExpII, 2018-1
!Gabriela Carretas

!El programa elige la configuración incial óptima para poder realizar
!el movimiento de las partículas utilizando el algoritmo de Dinámica Browniana.
!Tras obtener las matrices de configuración, se calculan propiedades
!estructurales como la g(r) y la presión, y propiedades dinámicas como
!la W(t) y el desplazamiento cuadrático medio.

!Iniciando el programa

!Configuración Inicial Random 3D con traslapes
!Unidades reducidas: sig
program DinBrown
  use constantes3D
  use variables3D

  !Declaración de variables
  implicit none

  !i,j,w,k: contadores del loop
  integer *4 w, i, j, k

  !obtenemos valores de entrada

  !print * , 'Concentracion reducida'
  !read *, dens

  !print * , 'Numero de configuraciones y tiempo de paso'
  !read (*,*) NSTEP, dt
  !print * , 'Frecuencia de muestreo en pantalla y de guardar en el archivo'
  !read (*,*) iprint, isave

  !Para que el pozo atractivo aparezca dentro del rcut
  dens=1.6
  N=1000*dens
  NSTEP=20000
  ter=10000
  iprint=1000
  isave=2
  dt=0.0004
  !phi=0.5

  !Parámetros del potencial: DOBLE GAUSSIANA
  T=0.2
  !el valor es el de eta tilde, como aqui se usa eta* lo divido entre T para que est� reducida
  eta=0.0263/T
  xi=3.0

  !calculando la concentración reducida en términos de la
  !fracción en volumen
  !dens=(6.0*phi)/pi

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

  !llamando a la subrutina de la configuración inicial aleatoria con traslapes
  call CIRAN3D

  !comenzar en ceros los contadores del algoritmo
  cm=0

  !abrimos los archivos
  open(2, file='cf.dat')

  write(*,*) 'Longitud reducida:',L
  write(*,*) 'Concentración reducida y numero de particulas:',dens, N
  write(*,*) 'Temperatura reducida:',T
  write(*,*) 'Numero de configuraciones a guardar:',ens
  write(*,*) '# de configuracion || E. Potencial normalizada || Presion del sistema'

  !iniciamos el contador de la configuración
  do w=1,NSTEP

    if (w.eq.8000) then
      T=0.07
    end if

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
      write(*,*) w, Vnor, presion
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
  close(3)
  close(4)
  close(5)

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
