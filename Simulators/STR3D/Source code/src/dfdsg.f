      SUBROUTINE DFDSG(DFDS,S,IYLD,ALP)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DFDS(6),S(6)
C-----------------------------------------------------------------------
C
C     ----- von Mieses -----
C
      VMEQ =   ( S(1) - S(2) )*( S(1) - S(2) ) 
     &       + ( S(2) - S(3) )*( S(2) - S(3) )
     &       + ( S(3) - S(1) )*( S(3) - S(1) ) 
     &       + 6.D0*( S(4)*S(4) + S(5)*S(5) + S(6)*S(6) )
C
      VMEQ = DSQRT(VMEQ)/DSQRT(2.D0)
C
      DFDS(1) = ( S(1) - .5D0*S(2) - .5D0*S(3) ) / VMEQ
      DFDS(2) = ( S(2) - .5D0*S(3) - .5D0*S(1) ) / VMEQ
      DFDS(3) = ( S(3) - .5D0*S(1) - .5D0*S(2) ) / VMEQ
      DFDS(4) = 3.D0*S(4) / VMEQ
      DFDS(5) = 3.D0*S(5) / VMEQ
      DFDS(6) = 3.D0*S(6) / VMEQ
C
      IF( IYLD == 1 ) RETURN
C
C     ----- Drucker-Prager -----
C
      A = 1.D0 / ( 1.D0/DSQRT(3.D0) - ALP )
      B = 1.D0 / DSQRT(3.D0)
C
      DFDS(1:3) = A*( ALP + B*DFDS(1:3) )
      DFDS(4:6) = A*B*DFDS(4:6)
C
      END