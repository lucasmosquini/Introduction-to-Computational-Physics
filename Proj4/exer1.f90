PROGRAM exer1
  IMPLICIT NONE
  INTEGER :: N, N_0, N_Dec  
  INTEGER :: j , k
  REAL(8) :: lamb, dt                          !
  !O arquivo pedido:
  OPEN(1, file = 'decai_out')
  READ(*,*) N_0
  READ(*,*) dt            
  READ(*,*) lamb          

  N = N_0
  N_Dec = 0

  WRITE(1,*) 0.0d0, N_0

  DO j = 1, int(10/dt), 1
    DO k = 1, N, 1
      !A função rand proposta, tentanto ser minimizada em quantidades. Se menor, decai adicionando 1
      IF (rand() < lamb*dt) THEN
        N_Dec = N_Dec + 1
      END IF
    END DO
    N = N - N_Dec
    N_Dec = 0
    WRITE(1,*) j*dt, N
  END DO

END PROGRAM
