      SUBROUTINE SOL2(IST,SY,S,D,E,ANU,IYLD,HD,ALP)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(6),WK2(6),WK3(21),D(21),DE(21),DFDS(6),DRDE(6)
     &         ,DEPDE(6,6)
C-----------------------------------------------------------------------
      CALL YFUNC(F,S,IYLD,ALP)
C
      IF( F >= SY ) THEN
C
        SY = F
C
        CALL DSOL0(DE,E,ANU)
        CALL DFDSG(DFDS,S,IYLD,ALP)
C
C       ----- [DRAM/DEPS] -----
C
        CALL HARFC(WK2,DFDS,DE,1,6)
        DRDE(:) = WK2(:) / ( HD + DOT_PRODUCT(WK2,DFDS) )
C
C       ----- [DEPS_P/DEPS] -----
C
        CALL AXB(DEPDE,DFDS,DRDE,6,1,6)
C
C       ----- [Dp] -----
C
        CALL HARFAB(WK3,DE,DEPDE,6)
        D(:) = DE(:) - WK3(:)
C
      ELSE
C
        IST = 0
C
        SY = F
C
        CALL DSOL0(D,E,ANU)
C
      ENDIF
C
      END