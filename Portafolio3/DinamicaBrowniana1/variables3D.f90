module variables3D
  implicit None

  !dens: concentración reducida
  !phi: fracción en volumen
  !L: longitud de la celda de simulación
  !rcut: radio de corte
  !Vnor: energía potencial normalizada
  real *4 dens, phi, L, rcut, Vnor

  !dt: tiempo de paso
  !var: varianza (ruido aleatorio)
  real *4 dt, var

  !VARIABLES DEL POTENCIAL
  real *4 A, zk, yuk

  !N: número de partículas
  !NSTEP: número de configuraciones del sistema
  !ens: tamaño del ensemble (# de configuraciones guardadas totales)
  integer *4 N, NSTEP, ens

  !x,y,z: vectores de posición (propiedades estructurales) para las N partículas
  real, dimension(:), allocatable, save :: x, y, z

  !x,y,z: vectores de posición (propiedades dinámicas) para las N partículas
  real, dimension(:), allocatable, save :: xd, yd, zd

  !cx,cy,cz: matrices de configuracion (propiedades estructurales) para las dimensiones
  real, dimension(:,:), allocatable, save :: cx, cy, cz

  !cxd,cyd,czd: matrices de configuracion (propiedades dinámicas) para las dimensiones
  real, dimension(:,:), allocatable, save :: cxd, cyd, czd

  !fx,fy,fz: vectores de fuerza para las N partículas
  real, dimension(:), allocatable, save :: fx, fy, fz

end module variables3D
