gfortran constantes3D.f90 -c
gfortran CIREG3D.f90 -c
gfortran MOV3D.f90 -c
gfortran DENSCTE3D.f90 -c
gfortran T3-act5.f90 constantes3D.o CIREG3D.o MOV3D.o DENSCTE3D.o -o t3-5.exe
