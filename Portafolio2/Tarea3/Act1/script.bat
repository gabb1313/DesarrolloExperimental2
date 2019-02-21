gfortran constantes2D.f90 -c
gfortran CIRAN2D.f90 -c
gfortran MOV2D.f90 -c
gfortran DENSCTE2D.f90 -c
gfortran T3-act1.f90 constantes2D.o CIRAN2D.o MOV2D.o DENSCTE2D.o -o t3-1.exe
