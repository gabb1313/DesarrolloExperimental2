module variables3D
  implicit None

  !dens: concentración reducida
  !phi: fracción en volumen
  !L: longitud de la celda de simulación
  !rcut: radio de corte
  real *4 dens, phi, L, rcut, ter

  !VARIABLES DEL POTENCIAL
  real *4 T, eta, xi

  !N: número de partículas
  !NSTEP: número de configuraciones del sistema
  !ens: tamaño del ensemble (# de configuraciones guardadas totales)
  integer *4 N, NSTEP, ens

  !x,y,z: vectores de posición (propiedades estructurales) para las N partículas
  real, dimension(:), allocatable, save :: x, y, z

  !cx,cy,cz: matrices de configuracion (propiedades estructurales) para las dimensiones
  real, dimension(:,:), allocatable, save :: cx, cy, cz

end module variables3D
