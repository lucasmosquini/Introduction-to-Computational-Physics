PROGRAM exer04
  IMPLICIT NONE
  REAL(8) :: x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4
  REAL(8) :: a1, a2, a3, a4, diag1, diag2, det, vol 
  REAL(8), DIMENSION(3) :: v1, v2, v3, v4, v5
  REAL(8), DIMENSION(3) :: prod_vet1, prod_vet2, prod_vet3, prod_vet4

!Le o arquivo dado

  OPEN (unit = 9, file = 'vet_in.dat')

  READ(9,*) x1, y1, z1
  READ(9,*) x2, y2, z2
  READ(9,*) x3, y3, z3
  READ(9,*) x4, y4, z4

  CLOSE(9)

!Coordenadas dos vetores para a matriz de cálculo do volume

  v1(1) = (x2 - x1)
  v1(2) = (y2 - y1)
  v1(3) = (z2 - z1)

  v2(1) = (x3 - x1)
  v2(2) = (y3 - y1)
  v2(3) = (z3 - z1)

  v3(1) = (x4 - x1)
  v3(2) = (y4 - y1)
  v3(3) = (z4 - z1)

  v4(1) = (x3 - x2)
  v4(2) = (y3 - y2)
  v4(3) = (z3 - z2)

  v5(1) = (x4 - x2)
  v5(2) = (y4 - y2)
  v5(3) = (z4 - z2)

 !Volume, de acordo com o que aprendemos em GA (Laplace)

  diag1 = (v1(1)*v2(2)*v3(3)) + (v1(2)*v2(3)*v3(1)) + (v2(1)*v3(2)*v1(3))
  diag2 = (v1(3)*v2(2)*v3(1)) + (v1(2)*v2(1)*v3(3)) + (v2(3)*v3(2)*v1(1))
  det = diag1 - diag2
  vol = (ABS(det))/6

!Area, de acordo com o que aprendemos em GA (basicamente so usar a matematica e ctrl c e v em cada processo mudando os vetores

  prod_vet1(1) = (v1(2)*v2(3)) - (v1(3)*v2(2))
  prod_vet1(2) = (v1(3)*v2(1)) - (v1(1)*v2(3))
  prod_vet1(3) = (v1(1)*v2(2)) - (v1(2)*v2(1))
  a1 = DSQRT((prod_vet1(1)**2) + (prod_vet1(2)**2) + (prod_vet1(3)**2))/2

  prod_vet2(1) = (v1(2)*v3(3)) - (v1(3)*v3(2))
  prod_vet2(2) = (v1(3)*v3(1)) - (v1(1)*v3(3))
  prod_vet2(3) = (v1(1)*v3(2)) - (v1(2)*v3(1))
  a2 = DSQRT((prod_vet2(1)**2) + (prod_vet2(2)**2) + (prod_vet2(3)**2))/2

  prod_vet3(1) = (v3(2)*v2(3)) - (v3(3)*v2(2))
  prod_vet3(2) = (v3(3)*v2(1)) - (v3(1)*v2(3))
  prod_vet3(3) = (v3(1)*v2(2)) - (v3(2)*v2(1))
  a3 = DSQRT((prod_vet3(1)**2) + (prod_vet3(2)**2) + (prod_vet3(3)**2))/2

  prod_vet4(1) = (v5(2)*v4(3)) - (v5(3)*v4(2))
  prod_vet4(2) = (v5(3)*v4(1)) - (v5(1)*v4(3))
  prod_vet4(3) = (v5(1)*v4(2)) - (v5(2)*v4(1))
  a4 = DSQRT((prod_vet4(1)**2) + (prod_vet4(2)**2) + (prod_vet4(3)**2))/2

!Calculada a área e volume, escreve no arquivo pedido, ordenando as áreas com a subrotina,
!Definida abaixo 

  OPEN (unit = 10, file = 'tetra_out.dat')
  WRITE(10,*) vol
  WRITE(10,*) a1 + a2 + a3 + a4

  CALL sort(a1, a2, a3, a4)
  CLOSE(10)

END PROGRAM exer04

!Professor, aq vale a aula que o filipe deu da diferença entre function() subroutine(),
!Confesso que tentei usar alguns metodos de sort, que sei em python, e me inspirei em algumas ideias na internet também.

SUBROUTINE sort(e1, e2, e3, e4)
  INTEGER :: k, i
  REAL(8) :: e1, e2, e3, e4, t
  REAL(8), DIMENSION(:) :: v(4)

  v(1) = e1
  v(2) = e2
  v(3) = e3
  v(4) = e4

  k = 2
  DO i = 1, 30
    IF (v(k - 1) > v(k)) THEN
      t = v(k - 1)
      v(k - 1) = v(k)
      v(k) = t
      k = k - 1
    ELSE
      k = k + 1
    END IF
    IF (k == 5) EXIT
  END DO

  WRITE(10,*) v(1)
  DO i = 2, 4
    IF (v(i) /= v(i-1)) THEN
      WRITE(10,*) v(i)
    END IF
  END DO
END
