      SUBROUTINE YFUNC(F,S,IYLD,ALP)
C
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 J1,J2D
      DIMENSION S(6)
C-----------------------------------------------------------------------
      J2D =   ( S(1) - S(2) )*( S(1) - S(2) ) 
     &      + ( S(2) - S(3) )*( S(2) - S(3) )
     &      + ( S(3) - S(1) )*( S(3) - S(1) ) 
     &      + 6.D0*( S(4)*S(4) + S(5)*S(5) + S(6)*S(6) )
C
      J2D = J2D/6.D0
C
      IF( IYLD == 1 ) THEN       ! von Mieses
        F = DSQRT (3.D0 * J2D)
      ELSEIF( IYLD == 2 ) THEN  ! Drucker-Prager  
        J1 = S(1) + S(2) + S(3)
        A = 1.D0 / ( 1.D0/DSQRT(3.D0) - ALP )
        F = A*( ALP*J1 + DSQRT(J2D) )
      ENDIF
C
      END