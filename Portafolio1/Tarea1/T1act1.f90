!Tarea 1 - Actividad 1
!DesExpII, 2018-1
!Gabriela Carretas

!El programa evalúa la respectiva función en un punto x0
!arbitrario, y también evalúa la función en un intervalo
!x1-x2 arbitario, con salida a pantalla y a un archivo.

!Funciones
real*4 function g(x)
  implicit None
  real*4 x
  g=exp(-x**2/2)
end function g

real*4 function h (x)
  implicit None
  real*4 x
  h=(1+x*x)**(-1)
end function h

!Iniciando el programa
program funcion
  implicit none
  real *4 x0, h, g, c, ci, ii, x, d, di, e
  integer *4 i, x1, x2

    print * , 'Evaluar en'
    read * , x0
    print * , 'Extremos del intervalo'
    read (*,*) x1, x2
    print * , 'Finura de muestreo'
    read * , e

  !Evaluando en la función
  open(1, file='funcionh.dat')
  c=h(x0)
  write(*,*) 'La funcion h evaluada en',x0,'es',c

  !Evaluando entre el intervalo dado
  write(*,*) 'Evaluando la funcion h en sus extremos'
  !Contador de inicio del intervalo
  x=x1-e
  !Realizará el loop hasta que llegue al otro extremo
  do while (x.le.x2)
    !Añadimos los pasos
    x=x+e
    !Evaluando la función
    ci=h(x)
    !Salida a pantalla y archivo
    write(*,*) x,ci
    write(1,*) x,ci
  end do
  close(1)

  open(2, file='funciong.dat')
  d=g(x0)
  write(*,*) 'La funcion g evaluada en',x0,'es',d

  write(*,*) 'Evaluando la funcion g en sus extremos'
  x=x1-e
  do while (x.le.x2)
    x=x+e
    di=g(x)
    write(*,*) x,di
    write(2,*) x,di
  end do
  close(2)

end program funcion
