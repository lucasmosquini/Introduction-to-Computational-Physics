PROGRAM exerB
  IMPLICIT NONE
  INTEGER :: i  
  REAL(8) :: y , x, x_i, y_i, x_0, v_0, y_0 
  REAL(8) :: y_jup, x0_jup , xi_jup, y0_jup, x_jup, yi_jup, v0_jup  
  REAL(8) :: fator, R, R_j, t , dt, R_T_J     
  REAL(8), PARAMETER :: cte_S = 4*((4*ATAN(1.0d0))**2)                                                                        
  REAL(8), PARAMETER :: cte_T = (cte_S*(6.0d0*10e24))/(2.0d0*10e30)                              
  REAL(8), PARAMETER :: cte_J = (cte_S*(1.9d0*10e27))/(2.0d0*10e30)                                                                                                                    

  v_0 = 2*(4*ATAN(1.0d0))  
  dt = 0.001d0
  R = 1.0d0 
  t = 0.0d0        
  y_0 = 0.0d0
  x_0 = R
  x = R
  y = 0.0d0
  y = y_0 + (v_0*dt)  
  R_j = 5.2d0                                 
  v0_jup = sqrt(4*((4*ATAN(1.0d0))**2) / R_j)  
  y0_jup = 0.0d0
  x0_jup = R_j
  x_jup = R_j
  y_jup = 0.0d0
  y_jup = y0_jup + (v0_jup*dt)

  !Seguindo o modelo do exerA, usei para calcular e posteriormente plotar o gráfico análogo em phyton:
  OPEN(4, file = 'trajB_out.dat')
  WRITE(*,*) 'fator de massa:'
  READ(*,*) fator

  DO i = 1, 20000
    R = sqrt((x**2) + (y**2))
    R_j = sqrt((x_jup**2) + (y_jup**2))
    R_T_J = sqrt(((x - x_jup)**2) + ((y - y_jup)**2))

    !numerico
    yi_jup = 2*y_jup - y0_jup - (((dt**2)*cte_S*y_jup) / (R_j**3)) - (((dt**2)*cte_T*(x_jup - x)) / (R_T_J**3))
    xi_jup = 2*x_jup - x0_jup - (((dt**2)*cte_S*x_jup) / (R_j**3)) - (((dt**2)*cte_T*(y_jup - y)) / (R_T_J**3))
    y_i = 2*y - y_0 - (((dt**2)*cte_S*y) / (R**3)) - (((dt**2)*fator*cte_J*(x - x_jup)) / (R_T_J**3))
    x_i = 2*x - x_0 - (((dt**2)*cte_S*x) / (R**3)) - (((dt**2)*fator*cte_J*(y - y_jup)) / (R_T_J**3))
    y_0 = y
    x_0 = x
    y = y_i
    x = x_i
    y0_jup = y_jup
    x0_jup = x_jup
    x_jup = xi_jup
    y_jup = yi_jup
    t = t + dt
    WRITE(4,*) t, x_i, y_i
  END DO

  WRITE(*,*) 'Dos testes, observa-se que um fator de massa, aproximadamente até 100, é menosprezável, sendo diretamente ', &
  'proporcional à deformação da órbita. Notavelmente quando próximo a 1000 temos o seu auge, expulsando o planeta da órbita.'
END PROGRAM
