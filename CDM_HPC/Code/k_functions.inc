contains

! Find determiante of square matrix
FUNCTION k_mat_det(inmatrix, n)
	IMPLICIT NONE
	REAL(DP), DIMENSION(n,n),INTENT(in) :: inmatrix
	REAL(DP), DIMENSION(n,n) :: matrix
	REAL(DP)                 :: k_mat_det
	INTEGER, INTENT(IN) :: n
	REAL(DP) :: m, temp
	INTEGER :: i, j, k, l
	LOGICAL :: DetExists = .TRUE.
	matrix=inmatrix
	l = 1
	!Convert to upper triangular form
	DO k = 1, n-1
		IF (matrix(k,k) == 0) THEN
			DetExists = .FALSE.
			DO i = k+1, n
				IF (matrix(i,k) /= 0) THEN
					DO j = 1, n
						temp = matrix(i,j)
						matrix(i,j)= matrix(k,j)
						matrix(k,j) = temp
					END DO
					DetExists = .TRUE.
					l=-l
					EXIT
				ENDIF
			END DO
			IF (DetExists .EQV. .FALSE.) THEN
				k_mat_det = 0
				return
			END IF
		ENDIF
		DO j = k+1, n
			m = matrix(j,k)/matrix(k,k)
			DO i = k+1, n
				matrix(j,i) = matrix(j,i) - m*matrix(k,i)
			END DO
		END DO
	END DO
	
	!Calculate determinant by finding product of diagonal elements
	k_mat_det = l
	DO i = 1, n
		k_mat_det = k_mat_det * matrix(i,i)
	END DO
	
END FUNCTION k_mat_det