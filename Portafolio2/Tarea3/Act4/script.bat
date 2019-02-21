gfortran constantes3D.f90 -c
gfortran CIRAN3D.f90 -c
gfortran MOV3D.f90 -c
gfortran DENSCTE3D.f90 -c
gfortran T3-act4.f90 constantes3D.o CIRAN3D.o MOV3D.o DENSCTE3D.o -o t3-4.exe
