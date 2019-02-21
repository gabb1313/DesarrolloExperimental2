!Calculando la g(r)
!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina

Subroutine gr3D
  use constantes3D
  use variables3D

  !Declaración de variables
  implicit none

  !integrando: argumento de la integral para
  !            la estimación del # de partículas
  !NRcut: # de partículas estimado
  !h: paso para integración por trapecio
  !int: valor de la integral del trapecio
  real, dimension(1000000) :: integrando
  real, dimension(1000000) :: hist
  real *4 NRcut, h, int

  !dr: tamaño de las cintas
  !cx1,cx2,cy1,cy2,cz1,cz2: ubicación de la j-ésima/k-ésima
  !cxr,cyr,czr: distancia en la componente entre la j-ésima y k-ésima
  !r: distancia entre la j-ésima y k-ésima
  real *4, parameter :: dr=0.05
  real *4 cx1, cx2, cy1, cy2, cz1, cz2, cxr, cyr, czr, r
  real *4 c3d, rlow, rup, rcar, Ngi, Nsis, gdr, gr1, pre

  !maxr: número total de cintas en rcut
  !nr: contador de la cinta
  !i,j,k: contadores del loop
  integer *4 maxr, nr
  integer *4 i, j, k

  !calculando el número total de cintas en rcut
  maxr=(rcut/dr)

  !iniciando el arreglo del histograma en ceros
  hist=0.0

  !contador de configuración
  do i=1,ens
    !contador de partícula
    do j=1,N
      !contador de partícula
      do k=1,N
        !evitando la cuenta de autointeracción
        if (k.ne.j) then
          !ubicación de las partículas en las matrices
          !de configuración
          cx1=cx(j,i)
          cx2=cx(k,i)
          !distancia en la componente entre las partículas
          cxr=cx1-cx2

          !ubicación de las partículas en las matrices
          !de configuración
          cy1=cy(j,i)
          cy2=cy(k,i)
          !distancia en la componente entre las partículas
          cyr=cy1-cy2

          !ubicación de las partículas en las matrices
          !de configuración
          cz1=cz(j,i)
          cz2=cz(k,i)
          !distancia en la componente entre las partículas
          czr=cz1-cz2

          !condiciones de imagen mínima
          cxr=cxr-L*anint(cxr/L)
          cyr=cyr-L*anint(cyr/L)
          czr=czr-L*anint(czr/L)

          !distancia entre la j-ésima y k-ésima
          r=sqrt(cxr**2+cyr**2+czr**2)

          !localizando el número de cinta
          nr=(r/dr)+1

          !si se encuentra dentro de las cintas en rcut
          !escribe en el histograma
          if (nr.le.maxr) then
            hist(nr)=hist(nr)+1
          end if

        end if
      !termina loop de la partícula en comparación
      end do
    !termina loop de la partícula de referencia
    end do
  !termina loop de la configuración
  end do

  c3d=(4.0/3.0)*pi*dens
  gr1=0.0

  !contador del número de cinta
  do k=1,maxr
    !calculando radio inferior
    rlow=real(k-1)*dr
    !calculando radio superior
    rup=rlow+dr
    !calculando el radio característico de la cinta
    rcar=rlow+(dr/2.0)

    !calculando número promedio de partículas para gas ideal
    Ngi=c3d*(rup**3-rlow**3)
    !calculando número promedio de partículas para el sistema
    Nsis=real(hist(k))/(real(ens)*real(N))

    !calculando la g(r)
    gdr=Nsis/Ngi

    !si el valor de contacto es diferente de cero, continua
    !el calculo de la g(r)
    if(gr1.ne.0.0) then
      go to 91
    end if

    !si el valor de la g(r) es diferente de cero, guarda
    !el valor de contacto gr1
    if (gdr.ne.0.0) then
      gr1=gdr
    end if

      !guarda en el arreglo el integrando para trapecio
  91  integrando(k)=gdr*rcar*rcar

    !escribe en el archivo: radio característico, g(r)
    write(11,*) rcar, gdr

  !termina contador del número de cinta
  end do

  !Calculando la presión
  !pre=1+r3*pi*dens*gr1

  !INTEGRACIÓN DE TRAPECIO
  !Tamaño del paso
  h=rcut/maxr
  !Valor inicial del integrando
  int=maxr*(integrando(1)/2.0)

  !valores siguientes del integrando con trapecio
  do k=2,maxr
    int=int+integrando(k)*h
  end do
  !Valor final del integrando
  int=int+(integrando(maxr)/2.0)*h

  !Estimación del número de partículas
  NRcut=24.0*dens*int

  write(*,*) '# de particulas'
  write(*,*) 'INICIAL || CALCULADO'
  write(*,*) N, '||', NRcut

  !write(*,*) 'Presion del sistema || Concentracion'
  !write(*,*) pre, dens

  close(11)

  return
end subroutine gr3D
