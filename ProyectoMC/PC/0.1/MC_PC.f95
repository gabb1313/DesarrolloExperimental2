!Desarrollo Experimental II
!Inés Valenzuela C.
!Monte Carlo para pozo cuadrado
!unidades reducidas sigma y beta

program MC_PC

use posiciones3d

! Declaracion de variables
  implicit none
  real*4 rho,phi,Lr,rcut,V,VN,deltv,deltvb,q,p
  real*4 xinew,yinew,zinew,xiold,yiold,ziold,Vnew,Vold
  real*4 alpha,beta,omega,eta                           !Números aleatorios
  integer*4 clock, i,j,k,N,Nf,C
  integer, dimension(:), allocatable :: iseed
  real*4 maxl,ratio                                  !desplazamiento máximo
  integer*4 npasos,iratio,iprint,isave               !número de pasos
  integer*4 iacep,NENER,NN2                          !movimientos aceptados
  integer*4 M,M1
  real*4 Lambda,Tred


  rho=0.1
! Valores de entrada
  !write(*,*) 'Proporcione una valor para las siguientes variables'
  !write(*,*) 'Número de partículas'
  !read *, N
 !write(*,*) 'Concentración reducida'
  !read *, rho
  !write(*,*) 'Número de pasos'
  !read *, npasos
  !write(*,*) 'Desplazamiento máximo inicial'
  !read *, maxl
  !write(*,*) 'Frecuencia para corregir el desplazamiento máximo'
  !read *, iratio
  !write(*,*) 'Razón de aceptados para aumentar el paso'
  !read*, q
  !write(*,*) 'Frecuencia para imprimir en pantalla'
  !read *, iprint
  !write(*,*) 'Frecuencia para guardar las configuraciones'
  !read *, isave

  N=343
  npasos=10500
  maxl=0.1
  iprint=500
  isave=2
  NENER=500                         !Configuración donde
  NN2=(npasos-nener)/isave          !Tamaño de el arreglo
  k=12                             !Tamaño de iseed
  L=(real(N)/rho)**(1.0/3.0)       !Longitud de la celda

! Parámetros del pozo cuadrado
  lambda=1.25
  Tred=1.0

! Para evitar que las partículas en la configuración inicial
! tengan una parte fuera de la caja reducimos L un radio
  Lr=L-1.0
  rcut= L/0.2
  M1=0

! Generador de números aleatorios
  allocate(iseed(k))
  call random_seed(size = k)
  call system_clock(COUNT=clock)
  iseed = clock + 37 * [(i, i = 0,k-1)]
  call random_seed(PUT = iseed)

! vectores para x,y y z
  allocate(x(N))
  allocate(y(N))
  allocate(z(N))

! vectores para los arreglos
  allocate(Cx(N,NN2))
  allocate(Cy(N,NN2))
  allocate(Cz(N,NN2))

! Archivos para guardar la configuración final y la termalización
  !open (2, file = '3dconfigfin.dat')
  open (3, file = 'terma.dat')


  if (rho.le.0.65) then
! LLamando a la configuración inicial
    CALL confin3d(Lr,N)
  else
    C=N**(1.0/3.0) !Calculando el número de partículas por lado
! Condición para saber si el número de particulas tiene raiz cubica
    p=real(C)**3
    if (N.eq.p) then
      CALL confreg3d(Lr,C)
! si el número de partículas no tiene raiz regresa a pedir otro número
    else
      write(*,*) 'El número de partículas no tiene raiz cúbica'
      deallocate(iseed,x,y,z,cx,cy,cz)
    end if
  end if


! Calculando la energía total de la configuración inicial
  CALL energin_PC(rcut,N,V,lambda,Tred)

  write(*,*) V

! Intentamos mover a las partículas npasos aplicando el criterio de MC
  do j=1,npasos
    do i=1,N

    xiold= x(i)
    yiold= y(i)
    ziold= z(i)

! Calculamos la energía inicial de la partícula
   CALL ienerg_PC(xiold,yiold,ziold,i,rcut,N,Vold,Lambda,Tred)

!Intentamos mover a una partícula
   CALL random_number(alpha)
   CALL random_number(beta)
   CALL random_number(omega)
   CALL random_number(eta)

   xinew= xiold + (alpha-0.5)*2.0*maxl
   yinew= yiold + (beta-0.5)*2.0*maxl
   zinew= ziold + (omega-0.5)*2.0*maxl

! Condiciones periodicas
   xinew = xinew -L*ANINT(xinew/L)
   yinew = yinew -L*ANINT(yinew/L)
   zinew = zinew -L*ANINT(zinew/L)

! Calculamos la energía de la partícula después de movimiento
   CALL ienerg_PC(xinew,yinew,zinew,i,rcut,N,Vnew,Lambda,Tred)

! Aplicamos el criterio de aceptación o rechazo
   deltv= Vnew-Vold
   deltvb=deltv

   if (deltvb.lt.75.0) then

     if(deltvb.le.0.0) then

! Sumamos el cambio en la energía
      V=V+deltv
      x(i)=xinew
      y(i)=yinew
      z(i)=zinew

      iacep=iacep + 1

    elseif (exp(-deltvb).gt. eta) then

      V=V+deltv
      x(i)=xinew
      y(i)=yinew
      z(i)=zinew

      iacep=iacep + 1
    end if
   end if

   VN= V/real(N)

! Guardamos la configuración final
   if (j==npasos) then
   ! write(2,*) x(i),y(i),z(i)
    if (abs(x(i)).le.(L/2.0).and.abs(y(i)).le.(L/2.0).and.abs(z(i)).le.(L/2.0)) then
      Nf=Nf+1
    end if
   end if

  end do


! Monitoreamos por pantalla cada iprint
  if (mod(j,iprint).eq.0) then
   write(*,*) j,VN
  end if

! Guardamos la energía por partícula en cada paso
  write(3,*) j,VN

! Optimización para el desplazamiento máximo
  !if (mod(j,iratio).eq.0) then
  !  ratio=real(iacep)/real(N*iratio)

  !  if(ratio.gt.q) then
  !   maxl=maxl*1.05
  !  else
  !   maxl=maxl*0.95
  !  end if

  !  iacep=0
  !end if

! Guardando las configuraciones de equilibrio
  if(mod(j,isave).eq.0.and.j.gt.NENER) then
    M1=M1+1

    do M=1,N
      CX(M,M1)=x(M)
      CY(M,M1)=y(M)
      CZ(M,M1)=z(M)
    end do
   end if
  end do

! Calculamos la gdr
  CALL gdr(rho,rcut,N,M1)

 write(*,*) '=================================================='
 write(*,*) 'La concentración reducida es ', rho
 write(*,*) 'La longitud de la celda es de', L
 write(*,*) 'Las partículas dentro de la caja son',Nf
 write(*,*) '=================================================='

  deallocate(x,y,z,cx,cy,cz,iseed)

 ! close(2)
  close(3)

end program
