      SUBROUTINE SFNQU2(SFNC,RINT,SINT,IEJO,NJ)
C
C     SFNC : OUT : SFNC(i) = Ni(ξ,η)
C     RINT : IN  : ξ
C     SINT : IN  : η
C     IEJO : IN  : 節点番号
C     NJ   : IN  : 節点数
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SFNC(9),IEJO(8)
C
      SP1=1.0D0+RINT
      SM1=1.0D0-RINT
      TP1=1.0D0+SINT
      TM1=1.0D0-SINT
      SFNC(1)=0.25D0*SP1*TP1
      SFNC(2)=0.25D0*SM1*TP1
      SFNC(3)=0.25D0*SM1*TM1
      SFNC(4)=0.25D0*SP1*TM1
      IF(NJ.GT.4) THEN
        DO J=5,8
          SFNC(J)=0.0
        ENDDO
        SM2=1.0D0-RINT*RINT
        TM2=1.0D0-SINT*SINT
        IF(IEJO(5).GT.0) SFNC(5)=0.5D0*SM2*TP1
        IF(IEJO(6).GT.0) SFNC(6)=0.5D0*SM1*TM2
        IF(IEJO(7).GT.0) SFNC(7)=0.5D0*SM2*TM1
        IF(IEJO(8).GT.0) SFNC(8)=0.5D0*SP1*TM2
        SFNC(1)=SFNC(1)-0.5D0*SFNC(5)-0.5D0*SFNC(8)
        SFNC(2)=SFNC(2)-0.5D0*SFNC(6)-0.5D0*SFNC(5)
        SFNC(3)=SFNC(3)-0.5D0*SFNC(7)-0.5D0*SFNC(6)
        SFNC(4)=SFNC(4)-0.5D0*SFNC(8)-0.5D0*SFNC(7)
      END IF
C
      IF( NJ .EQ. 9 ) THEN
        SFNC(9) = SP1*SM1*TP1*TM1
        SFNC(1) = SFNC(1) + .25D0*SFNC(9)
        SFNC(2) = SFNC(2) + .25D0*SFNC(9)
        SFNC(3) = SFNC(3) + .25D0*SFNC(9)
        SFNC(4) = SFNC(4) + .25D0*SFNC(9)
        SFNC(5) = SFNC(5) - .50D0*SFNC(9)
        SFNC(6) = SFNC(6) - .50D0*SFNC(9)
        SFNC(7) = SFNC(7) - .50D0*SFNC(9)
        SFNC(8) = SFNC(8) - .50D0*SFNC(9)
      ENDIF
C
      RETURN
      END
