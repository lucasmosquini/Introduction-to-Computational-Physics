PROGRAM exer03
  IMPLICIT NONE
  
  INTEGER ::  d, n , M
  !Le o arquivo dado
  OPEN (unit = 8, file = 'primos_out.dat')
  !Le o m dado
  DO
    READ(*,*) M
    IF (M >= 2) EXIT
  END DO

  !Começa o processo de verificação de primos
  
  WRITE(8,*) 2
  DO n = 3, M, 2
    d = 3
    DO
      IF (d*d > n .OR. MOD(n, d) == 0) EXIT
      d = d + 2
    END DO
    
    IF (d*d > n) THEN
      
      WRITE(8,*) n
    END IF
    !Professor, fiz um primeiro código masm o tempo ficou meio ruim, então usei algumas ideias da internet
  END DO

END PROGRAM exer03
