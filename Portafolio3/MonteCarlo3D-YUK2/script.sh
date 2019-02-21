gfortran constantes3D.f90 -c
gfortran variables3D.f90 -c
gfortran CIRAN3D.f90 -c
gfortran CIREG3D.f90 -c
gfortran ENCONF3D.f90 -c
gfortran Yukawa.f90 -c
gfortran ENPAR3D.f90 -c
gfortran MC.f90 -c
gfortran gr3d.f90 -c
gfortran DENSCTE3D.f90 -c
gfortran MC3D.f90 constantes3D.o variables3D.o CIRAN3D.o CIREG3D.o ENCONF3D.o Yukawa.o ENPAR3D.o MC.f90 gr3d.o DENSCTE3D.o -o MC3D
