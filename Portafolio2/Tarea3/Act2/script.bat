gfortran constantes2D.f90 -c
gfortran CIRAN2D.f90 -c
gfortran MOV2Dcp.f90 -c
gfortran DENSCTE2D.f90 -c
gfortran T3-act2.f90 constantes2D.o CIRAN2D.o MOV2Dcp.o DENSCTE2D.o -o t3-2.exe
