gfortran posiciones3d.f95 -c
gfortran confin3d.f95 -c
gfortran confreg3d.f95 -c
gfortran energin3d_HS.f95 -c
gfortran ienerg3d_HS.f95 -c
gfortran gdr3d_HS.f95 -c
gfortran MC_HS.f95 posiciones3d.o confin3d.o confreg3d.o energin3d_HS.o ienerg3d_HS.o gdr3d_HS.o -o HS
