PROGRAM exerA
  IMPLICIT NONE 
  REAL(8) :: R , x, y, x_0, x_i, y_0, y_i, periodo, dte, t, v_0    
  INTEGER :: k , i 
  REAL(8), PARAMETER :: cte = 4*((4*ATAN(1.0d0))**2) !Constantes Gravitacionais e um jeito de chamar pi em fortran 
  REAL(8), DIMENSION(9) :: v_sol, t_sol,  R_sol                                        

  !Tabela dada, do mais interno ao externo:
  R_sol(1) = 0.39d0    
  R_sol(2) = 0.72d0    
  R_sol(3) = 1.0d0     
  R_sol(4) = 1.52d0    
  R_sol(5) = 5.20d0    
  R_sol(6) = 9.24d0    
  R_sol(7) = 19.19d0   
  R_sol(8) = 30.06d0   
  R_sol(9) = 39.53d0   
  
  !Transformando as distancia em velocidade, ao Sol:
  DO i = 1, 9, 1
    v_sol(i) = sqrt((4*(4*ATAN(1.0d0))**2) / R_sol(i))
  END DO

  READ(*,*) R
  READ(*,*) v_0
  READ(*,*) dte

  !Cond Ini:
  t = 0.0d0
  x = R
  x_0 = R
  y_0 = 0.0d0
  y = y_0 + (v_0*dte)

  OPEN(1, file = 'trajA1_out.dat') 
  OPEN(2, file = 'tabA1_out.dat')   

  !Cabeçalho da tabela
  WRITE(1,*) t, x_0, y_0

  i = 0
  DO WHILE (i == 0)
    R = sqrt((x**2) + (y**2))
    y_i = 2*y - y_0 - (dte**2)*cte*y / (R**3)
    x_i = 2*x - x_0 - (dte**2)*cte*x / (R**3)
    IF (y_i > 0 .AND. y < 0) THEN
      i = i + 1
    END IF

    t = t + dte
    x_0 = x
    x = x_i
    y_0 = y
    y = y_i

    !Agora sim a tabela com os números:
    WRITE(1,*) t, x_i, y_i
  END DO
  periodo = t
  !cabeçalho da outra tabela pedida: 
  WRITE(2,*) '(Planeta)', '         (v_0)', '                  (T^2 / R^3)'
  DO k = 1, 9, 1
    t = 0.0d0
    i = 0
    x_0 = R_sol(k)
    x = R_sol(k)
    y_0 = 0.0d0
    y = y_0 + v_sol(k)*dte
    
    DO WHILE (i == 0)
      R = sqrt((x**2) + (y**2))
      x_i = 2.0d0*x - x_0 - (dte**2)*cte*x / (R**3)
      y_i = 2.0d0*y - y_0 - (dte**2)*cte*y / (R**3)
      IF (y_i > 0 .AND. y < 0) THEN
        i = i + 1
      END IF

      t = t + dte
      x_0 = x
      x = x_i
      y_0 = y
      y = y_i
      
    END DO

    t_sol(k) = (t**2) / (R_sol(k)**3)

  END DO
    !a outra tabela pedida
    WRITE(2,*) 'Mercúrio', v_sol(1), t_sol(1)
    WRITE(2,*) 'Vênus', v_sol(2), t_sol(2)
    WRITE(2,*) 'Terra', v_sol(3), t_sol(3)
    WRITE(2,*) 'Marte', v_sol(4), t_sol(4)
    WRITE(2,*) 'Júpiter', v_sol(5), t_sol(5)
    WRITE(2,*) 'Saturno', v_sol(6), t_sol(6)
    WRITE(2,*) 'Urano', v_sol(7), t_sol(7)
    WRITE(2,*) 'Netuno', v_sol(8), t_sol(8)
    WRITE(2,*) 'Plutão', v_sol(9), t_sol(9)

    PRINT*, 'Tarefa A.2 - Resposta: Visto as equações númericas, que por sua vez baseiam-se no método de Taylor, ', &
    'dte tem que ser pequeno, pois, caso contrário, o termo elevado a 2 dominará nas iterações ', &
    'Portanto, valores em torno de 10elevado(-2) são razoáveis. '

    !Fechando os arquivos
    CLOSE(1)
    CLOSE(2)
  END PROGRAM
