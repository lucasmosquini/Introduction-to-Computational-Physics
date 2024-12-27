PROGRAM exer01
IMPLICIT NONE


  REAL(4) :: a1           
  REAL(8) :: a2           
  REAL (16) :: a3         
  INTEGER :: bit1, bit2, bit3


  a1 = 1.0e0
  a2 = 1.0d0
  a3 = 1.0_16
  bit1 = 0
  bit2 = 0
  bit3 = 0


  PRINT*, 'SIMPLES'
  DO WHILE ((1.0e0 + a1) /= 1.0e0)
    a1 = a1/(2.0e0)
    PRINT*, (1.0e0 + a1), a1
    bit1 = bit1 + 1
  END DO


  PRINT*, 'DUPLA'
  DO WHILE ((1.0d0 + a2) /= 1.0d0)
    a2 = a2/(2.0d0)
    PRINT*, (1.0d0 + a2), a2
    bit2 = bit2 + 1
  END DO


  PRINT*, 'QUÁDRUPLA'
  DO WHILE ((1.0_16 + a3) /= 1.0_16)
    a3 = a3/(2.0_16)
    PRINT*, (1.0_16 + a3), a3
    bit3 = bit3 + 1
  END DO


PRINT*, bit1, a1*2.0e0
PRINT*, bit2, a2*2.0d0
PRINT*, bit3, a3*2.0_16

END PROGRAM exer01
