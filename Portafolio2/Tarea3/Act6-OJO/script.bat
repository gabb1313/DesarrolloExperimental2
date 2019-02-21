gfortran constantes2D.f90 -c
gfortran CIRAN2D.f90 -c
gfortran ENCONF2D.f90 -c
gfortran ENPAR2D.f90 -c
gfortran MC.f90 -c
gfortran T3-act6.f90 constantes2D.o CIRAN2D.o ENCONF2D.o ENPAR2D.o MC.o -o t3-6.exe
