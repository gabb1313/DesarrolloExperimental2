!Calculando la g(r)
!DesExpII, 2018-1
!Gabriela Carretas

!Iniciando subrutina

Subroutine gr(rcut, ens, N, L, dens)
  use constantes2D
  use variables

  !Declaración de variables
  implicit none

  !dr: tamaño de las cintas
  real *4, parameter :: dr=0.01
  real, dimension(:), allocatable, save :: integrando
  character ci*40

  real *4 rcut, L, dens
  real *4 cx1, cx2, cy1, cy2, cxr, cyr, r
  real *4 c2d, rlow, rup, rcar, Ngi, Nsis, gdr, gr1, pre
  real *4 NRcut, h, int

  !N: número de partículas
  !ens: número de elementos del ensemble
  !maxr: número total de cintas en rcut
  !nr: contador de la cinta
  integer *4 N, ens, maxr, nr, z

  maxr=(rcut/dr)

  do z=1,10000
    hist(z)=0
  end do

    !contador de ensemble
    do i=1,ens
      !contador de partícula
      do j=1,N
          !contador de partícula
          do z=1,N
              if (z.ne.j) then

                cx1=cx(j,i)
                cx2=cx(z,i)
                cxr=cx1-cx2

                cy1=cy(j,i)
                cy2=cy(z,i)
                cyr=cy1-cy2

                cxr=cxr-L*anint(cxr/L)
                cyr=cyr-L*anint(cyr/L)

                r=sqrt(cxr**2+cyr**2)
                nr=(r/dr)+1

                if (nr.le.maxr) then
                  hist(nr)=hist(nr)+1
                end if

              end if
            end do
        end do
      end do

      print * , 'Nombre de la g(r)'
      read (*,*) ci

      open(5, file=ci)

      c2d=pi*dens
      gr1=0.0

      !DESDE AQUI
      allocate(integrando(1:maxr))

      do z=1,maxr
        rlow=real(z-1)*dr
        rup=rlow+dr
        rcar=rlow+(dr/2.0)

        Ngi=c2d*(rup**2-rlow**2)
        Nsis=real(hist(z))/(real(ens)*real(N))

        gdr=Nsis/Ngi

        if(gr1.ne.0.0) then
          go to 91
        end if

        if (gdr.ne.0.0) then
          gr1=gdr
        end if

      91  integrando(z)=gdr*rcar

        write(5,*) rcar, gdr
      end do

      !Calculando la presión
      pre=1+r2*pi*dens*gr1

      !TRAPECIO
      !Verificación de datos
      h=rcut/maxr


      int=maxr*(integrando(1)/2.0)

      do z=2,maxr
        int=int+integrando(z)*h
      end do

      int=int+(integrando(maxr)/2.0)*h

      NRcut=8.0*dens*int

      write(*,*) '# de particulas'
      write(*,*) 'INICIAL || CALCULADO'
      write(*,*) N, '||', NRcut

      write(*,*) 'Presion del sistema || Concentracion'
      write(*,*) pre, dens

      deallocate(integrando)

    return
end subroutine gr
