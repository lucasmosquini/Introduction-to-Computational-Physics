PROGRAM exerA
  IMPLICIT NONE

  REAL(8) ::  v0, v, t, dt            
  REAL(8), PARAMETER :: pot = 400.0d0  , m = 80.0d0 
  INTEGER :: i, it                                   

  !Terminal 
  READ(*,*) t, dt, v0
  !seta a variável do loop nula
  i = 0
  OPEN(1, file = 'velA_out.dat')

  !numero de iterações
  it = int(t/dt)

  WRITE(1,*) 1.0d0*i, v0

  DO i= 1, it, 1
    v = v0 + ((pot*dt)/(m*v0))
    v0 = v
    WRITE(1,*) i*dt, v
  END DO

END PROGRAM
