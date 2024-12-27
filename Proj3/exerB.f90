PROGRAM exerB
  IMPLICIT NONE
  REAL(8) :: A, t_term, x, dt, T, v_0, vel, vm
  REAL(8), PARAMETER ::  c = 1.0d0/2.0d0 , rho = 1.2d0 , m = 80.0d0, pot = 400.0d0
  INTEGER :: j, it
  !Terminal
  READ(*,*) T, dt, v_0, A

  x = 0.0d0
  t_term = 0.0d0
  j = 0

  OPEN(2, file = 'velB_out.dat')
  it = int(T/dt)
  !Cond inicial
  WRITE(2,*) 1.0d0*j, v_0

  DO j = 1, it, 1
    x = x + (v_0*dt)
    vel = v_0 + ((pot*dt)/(m*v_0)) - ((c*rho*A*((v_0)**2)*dt)/m)
    !Vel terminal
    IF (vel == v_0 .AND. t_term == 0.0d0) THEN
      t_term = (j-1)*dt
    END IF
    v_0 = vel
    WRITE(2,*) j*dt, vel
  END DO

  vm = x/T

  PRINT*, '01 - Ao se curvar o ciclista reduz a sua área corporal de contato com o ar, ou seja, diminuindo o efeito de arreste',&
  'e aumento a sua velocidade. Contudo é importante lembrar que existe uma velocidade limite atingida no infito',&
  'chamada velocidade terminal . 02 - Ao correr em grupo, ou mais precisamente, com alguem na frente, gera algo que é comumente',&
  ' chamado de vácuo, o qual propicia uma redução da resistência do ar no ciclista, uma vez que concentra-se no da frente',&
  'Reduzindo a perda de energia e aumentado a velocidade. 03 -  O que ja foi dito no argumento anterior, notando que permanecer',&
  'atras de alguem, perpetuaria esse efeito.'

  PRINT*, 'Espaço percorrido no tempo T:', x
  PRINT*, 'Velocidade final no tempo T:', vel
  PRINT*, 'tempo que atinge velocidade terminal', t_term
  PRINT*, 'Velocidade média no tempo T:', vm

END PROGRAM
