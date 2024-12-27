PROGRAM exerC1
  IMPLICIT NONE
  REAL(8) :: m, dt, T, tet_0, L  
  INTEGER ::  i, it
  REAL(8), PARAMETER :: pi_8 = 4*atan(1.0_8)                                                    
  REAL(8) :: E, w_0, tet, g, w                          
                              
  OPEN(6, file = 'energiaC1_out.dat')
  OPEN(2, file = 'exerC1_out.dat')
  !Dados:
  READ(*,*) tet_0
  READ(*,*) L
  READ(*,*) m
  READ(*,*) dt
  READ(*,*) T

  g= 10.0d0
  w_0 = 0.0d0
  
  tet_0 = tet_0*(pi_8/180.0d0)

  it = int(T/dt)
  E = (m*(w_0**2)*(L**2))/2.0d0 + m*L*(1-cos(tet_0))*g

  WRITE(2,*) 0.0d0, tet_0 
  WRITE(6,*) 0.0d0, E 

  
  DO i = 1, it, 1
    w = w_0 - (g*tet_0*dt)/L
    tet = tet_0 + (w_0*dt)
    w_0 = w

    
    IF (tet > pi_8) THEN
      tet = tet - (2.0d0*pi_8)
    ELSE
      IF (tet < -pi_8) THEN
        tet = tet + (2.0d0*pi_8)
      END IF
    END IF

    tet_0 = tet
    E = (m*(w**2)*(L**2))/2.0d0 + m*L*(1-cos(tet))*g
    WRITE(2,*) i*dt, tet
    WRITE(6,*) i*dt, E
  END DO

END PROGRAM
