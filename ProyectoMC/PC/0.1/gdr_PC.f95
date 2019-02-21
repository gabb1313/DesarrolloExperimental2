!Desarrollo Experimental II
!Ines Valenzuela
!CÃ¡lculo de la funciÃ³n de correlaciÃ³n

subroutine gdr(rho,rcut,N,M1)

use posiciones3d

! Declaracion de variables
  implicit none
  real*4 deltar,xl0,xlt,xl0t,yl0,ylt,yl0t,zl0,zlt,zl0t,r,rho,rt,gr
  real*4 rcut,rl,ru,c1,Ngi,Nsis,gr1,r1
  integer*4 i,j,k,m,maxbin,nbin,ng,maxrm,maxrp
  integer*4 N,NN2,NN3,M1
  integer, dimension(:), allocatable :: nhist
  real, dimension(:), allocatable, save :: integrando
  real*4 h, Inte, NRcut
  real *4 a1,a74,b, p, pcs1, pcs2, pcs, phi
  real*4 lambda,Tred,grlamm,grlamp
  character nombre*40

  NN3=10000            !TamaÃ±o del histograma
  allocate(nhist(NN3))

! ParÃ¡metros del potencial perturbativo
  lambda=1.25

! Ponemos al histograma en 0
  do i=1,NN3
  nhist(i)=0
  end do

  Inte=0.0

  phi=(pi*rho)/6.0
  deltar=0.05               !TamaÃ±o de cada cinta
  maxbin=int(L/2.0/deltar)  !NÃºmero de cintas
  ng=int(1.0/deltar)+1      !Primer punto diferente de 0
  maxrp=int(lambda/deltar)+1 !lambda mas
  maxrm=int(lambda/deltar)   !lamnda menos

  allocate(integrando(maxbin))

!Comparamos la distancia de la partÃ­cula j a la k en la configuracion
!M1 y la guardamos en el histograma dependiendo el nÃºmero de cintas
do j=1,N

 do k=1,N

  if(j.ne.k) then

  do m=1,M1
  xl0=cx(j,m)
  xlt=cx(k,m)
  xl0t= xl0-xlt

  yl0=cy(j,m)
  ylt=cy(k,m)
  yl0t= yl0-ylt

  zl0=cz(j,m)
  zlt=cz(k,m)
  zl0t= zl0-zlt

  xl0t=xl0t-L*anint(xl0t/L)
  yl0t=yl0t-L*anint(yl0t/L)
  zl0t=zl0t-L*anint(zl0t/L)

  r=sqrt(xl0t*xl0t + yl0t*yl0t + zl0t*zl0t)
  nbin=int(r/deltar)+1

  if(nbin.le.maxbin) then
  nhist(nbin)=nhist(nbin)+1
  end if

  end do
  end if
 end do
end do


  c1=(4.0/3.0)*pi*rho

  !write(*,*) 'Ingrese nombre de la g(r)'
  !read *, nombre

! Archivo para guardar la gdr
  open (1, file = 'gr.dat')

! Calculamos la gdr
  do nbin=1,maxbin

  rl=real(nbin-1)*deltar  !radio inferior
  ru=rl+deltar            !radio superior
  rt= rl+deltar/2.0       !distancia media
  Ngi=c1*(ru*ru*ru - rl*rl*rl)  !Para el gas ideal
  Nsis=real(nhist(nbin))/(real(M1)*real(N)) !Para el sistema
  gr=Nsis/Ngi

! Primer valor diferente de cero en grd (para calcular p)
  if (nbin.eq.ng) then
  gr1=gr
  end if

! Valor de la gdr en Lambda
  if (nbin.eq.maxrm) then
  grlamm=gr
  end if

  if (nbin.eq.maxrp) then
  grlamp=gr
  end if


  write(1,*) rt, gr

  integrando(nbin)=gr*rt*rt

  end do

! IntegraciÃ³n por trapecio para P
  inte=(1.0/2.0)*rcut*rcut

  do i=2, maxbin-1
  inte=inte + integrando(i)
  end do

  inte=deltar*inte

  NRcut=24.0*rho*inte

! Calculamos la presiÃ³n de esferas duras

  p=rho+(2.0/3.0)*pi*rho*rho*(gr1+(lambda**3)*(grlamp-grlamm))


 write(*,*) '======================================================='
 write(*,*) 'n*, phi, GR1, GRL+, GRL-, p'
 write(*,*) rho, phi, gr1, grlamp, grlamm, p
 write(*,*) 'El numero  de parti­culas es', N
 write(*,*) 'El numero de parti­culas calculado es', NRcut
 write(*,*) '======================================================='

 close(1)

 deallocate(integrando)
 deallocate(nhist)

return
end
