PROGRAM exerC2
  IMPLICIT NONE
  INTEGER :: j, it                                            
  REAL(8) :: tet_0, L, m, dt, t      
  REAL(8) :: E, w_0, g , tet, w                        
  REAL(8), PARAMETER :: pi_8 = 4*atan(1.0_8)                            !

  
  OPEN(3, file = 'exerC2_out.dat')
  OPEN(7, file = 'energiaC2_out.dat')
  !Dados
  READ(*,*) tet_0
  READ(*,*) L
  READ(*,*) m
  READ(*,*) dt
  READ(*,*) t


  w_0 = 0.0d0
  g = 10.0d0
  tet_0 = tet_0*(pi_8/180.0d0)
  it = int(t/dt)
  
  E = (m*(w_0**2)*(L**2))/2.0d0 + m*L*(1-cos(tet_0))*g

  WRITE(3,*) 0.0d0, tet_0 
  WRITE(7,*) 0.0d0, E 

  
  DO j = 1, it, 1
    w = w_0 - (g*tet_0*dt)/L
    tet = tet_0 + (w*dt) !AQ MUDA
    w_0 = w

    IF (tet > pi_8) THEN
      tet = tet - (2.0d0*pi_8)
    ELSE
      IF (tet < -pi_8) THEN
        tet = tet + (2.0d0*pi_8)
      END IF
    END IF

    tet_0 = tet
    E = (m*(w**2)*(L**2))/20.d0 + m*L*(1-cos(tet))*g
    WRITE(3,*) j*dt, tet
    WRITE(7,*) j*dt, E
  END DO

END PROGRAM
