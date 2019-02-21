gfortran constantes3D.f90 -c
gfortran variables3D.f90 -c
gfortran CIRAN3D.f90 -c
gfortran FUERZAS.f90 -c
gfortran FDG.f90 -c
gfortran ERMAK.f90 -c
gfortran RanGauss.f90 -c
gfortran DENSCTE3D.f90 -c
gfortran gr.f90 -c
gfortran wdt.f90 -c
gfortran DINBROWN.f90 constantes3D.o variables3D.o CIRAN3D.o FUERZAS.o FDG.o ERMAK.o RanGauss.f90 DENSCTE3D.o gr.o wdt.o -o DB
