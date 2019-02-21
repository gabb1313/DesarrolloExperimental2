gfortran constantes2D.f90 -c
gfortran variables.f90 -c
gfortran CIRAN2D.f90 -c
gfortran CIREG2D.f90 -c
gfortran ENCONF2D.f90 -c
gfortran ENPAR2D.f90 -c
gfortran MC.f90 -c
gfortran gr.f90 -c
gfortran ESQUELETO.f90 constantes2D.o variables.o CIRAN2D.o CIREG2D.o ENCONF2D.o ENPAR2D.o MC.f90 gr.o -o esq.exe
