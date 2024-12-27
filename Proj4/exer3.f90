PROGRAM exer3
  IMPLICIT NONE
  REAL(8) :: lamb, N, dt, t
  INTEGER :: k, N_0

  OPEN(3, file = 'decai_in')
  OPEN(4, file = 'decai_out')

  READ(3,*) t
  READ(3,*) N_0
  READ(3,*) dt
  READ(3,*) lamb

  WRITE(4,*) 0.0d0, N_0
  N = N_0
  DO k = 1, int(t/dt), 1
    N = N - (lamb*dt*N)
    WRITE(4,*) (dt*k), N
  END DO
END PROGRAM