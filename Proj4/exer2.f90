PROGRAM exer2
  IMPLICIT NONE
  INTEGER :: N, N_0, N_dec 
  INTEGER :: j , k  
  REAL(8) :: lamb, dt, tmed_dado, tmed 
 
  READ(*,*) N_0
  READ(*,*) dt
  READ(*,*) lamb

  N = N_0
  N_dec = 0
  tmed_dado = 1.0d0/(lamb)
  tmed = 0.0d0

  DO j = 1, int(10/dt), 1
    DO k = 1, N, 1
      IF (rand() < lamb*dt) THEN
        N_dec = N_dec + 1
      END IF
    END DO
    tmed = tmed + ((j*N_dec*dt)/N_0)
    N = N - N_dec
    N_dec = 0
  END DO
  tmed = tmed + ((10.0d0*N)/N_0)
  PRINT*, tmed, tmed_dado

END PROGRAM
