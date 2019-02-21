module variables
  implicit None

  !i,j,k,w: contadores del loop
  integer *4 i, j, k, w

  !x,y: vectores de posición para las N partículas
  real, dimension(:), allocatable, save :: x, y

  !cx,cy: matrices de configuracion para las dimensiones
  real, dimension(:,:), allocatable, save :: cx, cy

  end module variables
