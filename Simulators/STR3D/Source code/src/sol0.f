      SUBROUTINE SOL0(IST,SY,S,D,E,ANU,ST,IYLD,HD,ALP,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION S(6),PR(6),TR(3,3),T1(6,6),T2(6,6),DCR(21),WK1(6,6)
     &         ,WK2(6),WK3(21),D(21),DE(21),DFDS(6),DRDE(6),DEPDE(6,6)
C-----------------------------------------------------------------------
      IF( ST > 0. ) THEN
C
        CALL KOJI3D(S,PR,TR,ITO)
C
        IF( PR(1) >= ST ) THEN
C
          IST = 1
C
          CALL CHTNSR(TR,T1,T2)
C
          PR(1) = 0.
          PR(4:6) = 0.
          CALL AXB(S,T1,PR,6,6,1)
C
          CALL DSOL1(DCR,E,ANU)
          CALL HARFC(WK1,T1,DCR,6,6)
          CALL HARFTA2(D,WK1,T1,6,6)
C
          RETURN
C
        ENDIF
C
      ENDIF
C
      IF( IYLD > 0 ) THEN
C
        CALL YFUNC(F,S,IYLD,ALP)
C
        IF( F >= SY ) THEN
C
          IST = 2
C
          SY = F
C
          CALL DSOL0(DE,E,ANU)
          CALL DFDSG(DFDS,S,IYLD,ALP)
C
C         ----- [DRAM/DEPS] -----
C
          CALL HARFC(WK2,DFDS,DE,1,6)
          DRDE(:) = WK2(:) / ( HD + DOT_PRODUCT(WK2,DFDS) )
C
C         ----- [DEPS_P/DEPS] -----
C
          CALL AXB(DEPDE,DFDS,DRDE,6,1,6)
C
C         ----- [Dp] -----
C
          CALL HARFAB(WK3,DE,DEPDE,6)
          D(:) = DE(:) - WK3(:)
C
        ENDIF
C
      ENDIF
C
      END