REAL(8) FUNCTION g(x,r)
	IMPLICIT NONE
	REAL(8) :: x, r
	g = r*x*(1.0d0 - x)
END FUNCTION g
REAL(8) FUNCTION dg(x,r)
	IMPLICIT NONE
	REAL(8) :: x, r
	dg = r - 2.0d0*r*x
END FUNCTION dg

PROGRAM exer1
	IMPLICIT NONE
	REAL(8) :: x, x_epislon, epislon, r, lambda_dec, lambda_soma, g , dg, di, d10
	INTEGER :: i
	READ(*,*) x
	READ(*,*) r
	READ(*,*) epislon
	lambda_soma = 0.0d0
	x_epislon = x + epislon
	OPEN(10,file = 'dist_out.dat')
	WRITE(10,*) 0, x, epislon
	IF (r < 3.0d0) THEN
		DO i = 1, 1000
			IF (i .LE. 10) THEN
				lambda_soma = lambda_soma + log(abs(dg(x,r)))
				x_epislon = g(x_epislon,r)
				x = g(x,r)
				IF (i == 10) THEN
					d10 = abs(g(x_epislon,r) - g(x,r))
				END IF
			ELSE
				di = abs(g(x_epislon,r) - g(x,r))
				lambda_soma = lambda_soma + log(abs(dg(x,r)))
				WRITE(10,*) i, x, di
				IF (di < 1e-15) THEN
					EXIT
				END IF
				x_epislon = g(x_epislon,r)
				x = g(x,r)
			END IF
		END DO
		lambda_dec = (log(di) - log(d10))/(i-10.0d0)
	END IF

	IF (r .GE. 3.6d0) THEN
		DO i = 1, 1000
			di = abs(g(x_epislon,r) - g(x,r))
			WRITE(10,*) i, x, di
			x_epislon = g(x_epislon,r)
			x = g(x,r)
			lambda_soma = lambda_soma + log(abs(dg(x,r)))
			IF (di .GT. 0.5d0) THEN
				EXIT
			END IF
			lambda_dec = (log(di) - log(epislon))/i
		END DO
	END IF
	CLOSE(10)
	lambda_soma = lambda_soma / i
	WRITE(*,*) 'decaimento exponencial:', lambda_dec
	WRITE(*,*) 'somatório:', lambda_soma
END PROGRAM exer1
