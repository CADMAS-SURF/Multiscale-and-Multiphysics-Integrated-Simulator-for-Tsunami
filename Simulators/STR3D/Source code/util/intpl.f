      SUBROUTINE INTPL(Y,X,TBL,N)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X0(N),Y0(N),TBL(2,N)
C-----------------------------------------------------------------------
      DO 200 I=1,N
        X0(I)=TBL(1,I)
        Y0(I)=TBL(2,I)
  200 CONTINUE
C
      DO 100 I=2,N-1
        IF( X .LE. X0(I) ) THEN
          IF( X .EQ. X0(I) .AND. X0(I) .EQ. X0(I+1) ) THEN
            Y = .5*( Y0(I) + Y0(I+1) )
          ELSE
            A = ( Y0(I) - Y0(I-1) ) / ( X0(I) - X0(I-1) ) 
            Y = A*( X - X0(I-1) ) + Y0(I-1)
          ENDIF
          RETURN
        ENDIF
  100 CONTINUE
C
      A = ( Y0(N) - Y0(N-1) ) / ( X0(N) - X0(N-1) )
      Y = A*( X - X0(N-1) ) + Y0(N-1)
C
      RETURN
      END
