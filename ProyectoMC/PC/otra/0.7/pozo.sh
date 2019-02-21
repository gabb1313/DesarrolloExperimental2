gfortran posiciones3d.f95 -c
gfortran confin3d.f95 -c
gfortran confreg3d.f95 -c
gfortran energin_PC.f95 -c
gfortran ienerg_PC.f95 -c
gfortran gdr_PC.f95 -c
gfortran MC_PC.f95 posiciones3d.o confin3d.o confreg3d.o energin_PC.o ienerg_PC.o gdr_PC.o -o PC

