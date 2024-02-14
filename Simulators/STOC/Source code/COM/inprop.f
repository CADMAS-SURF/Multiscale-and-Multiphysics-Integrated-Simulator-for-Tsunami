      SUBROUTINE INPROP
C======================================================================
C     流体の物性値を読み込む
C     入力データの追加方法は README_INPUT を参照
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
C
      REAL(8)::RHO0=0.0D0,RHOA0=0.0D0,CP0=0.0D0,
     $         AMUH0=0.0D0,AMUV0=0.0D0,CNDH0=0.0D0,
     $         CNDV0=0.0D0,DIFH0=0.0D0,DIFV0=0.0D0
C
      INTEGER::IE,IERR,IS,N
C
      DO 100 N=1,100000
         CALL GET1(IS,IE,IERR)
         IF( IERR.GT.0 ) GO TO 900
c         write(*,*) 'debug:inprop:variable=',cline(is:ie)
C
         IF( CLINE(IS:IE) .EQ. '%END' ) THEN
            GO TO 200
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DENSITY' ) THEN
            CALL GETR(RHO0)
            IF(RHO0.GT.0.0D0) RHO=RHO0
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DENSITY-AIR' ) THEN
            CALL GETR(RHOA0)
            IF(RHOA0.GT.0.0D0) RHOA=RHOA0
C
         ELSE IF( CLINE(IS:IE) .EQ. 'HEAT-CAPACITY' ) THEN
            CALL GETR(CP0)
            IF(CP0.GT.0.0D0) CP=CP0
C
         ELSE IF( CLINE(IS:IE) .EQ. 'VISCOSITY-H' ) THEN
            CALL GETR(AMUH0)
            IF(AMUH0.GE.0.0D0) AMUH=AMUH0
C
         ELSE IF( CLINE(IS:IE) .EQ. 'VISCOSITY-V' ) THEN
            CALL GETR(AMUV0)
            IF(AMUV0.GE.0.0D0) AMUV=AMUV0
C
         ELSE IF( CLINE(IS:IE) .EQ. 'CONDUCTIVITY-H' ) THEN
            CALL GETR(CNDH0)
            IF(CNDH0.GE.0.0D0) CNDH=CNDH0
C
         ELSE IF( CLINE(IS:IE) .EQ. 'CONDUCTIVITY-V' ) THEN
            CALL GETR(CNDV0)
            IF(CNDV0.GE.0.0D0) CNDV=CNDV0
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DIFFUSION-H' ) THEN
            CALL GETR(DIFH0)
            IF(DIFH0.GE.0.0D0) DIFH=DIFH0
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DIFFUSION-V' ) THEN
            CALL GETR(DIFV0)
            IF(DIFV0.GE.0.0D0) DIFV=DIFV0
C
         ELSE
            CALL ERRMSG('INPROP',6690)
            WRITE(LP,*) 'UNKNOWN VARIABLE NAME: ',CLINE(IS:IE)
            CALL ABORT1('')
         END IF
  100 CONTINUE
  200 CONTINUE
C
C ... ANU,ALPの設定
      IF(RHO.GT.0.0D0) THEN
        ADRHO = RHOA/RHO
        ANUH  = AMUH/RHO
        ANUV  = AMUV/RHO
        ALPH  = CNDH/RHO/CP
        IF(CNDH0.LT.0.0D0) THEN
          ALPH = ANUH/PRT
          CNDH = ALPH*RHO*CP
        END IF
        ALPV  = CNDV/RHO/CP
        IF(CNDV0.LT.0.0D0) THEN
          ALPV = ANUV/PRT
          CNDV = ALPV*RHO*CP
        END IF
        IF(DIFH0.LT.0.0D0) DIFH=ANUH/SCT
        IF(DIFV0.LT.0.0D0) DIFV=ANUV/SCT
      ELSE
        WRITE(LP,*) 'DENSITY OF WATER = 0.0  AT SUB.INPROP'
      END IF
C
      RETURN
C
C ... 読み込みエラー
  900 CONTINUE
      CALL ERRMSG('INPROP',6691)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
      END
