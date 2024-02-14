      SUBROUTINE VRTXCHK(ISLV,MASTER,FC,EN,IEDG,IELC,ICELA,ICEL,IBTE
     &                  ,ICEDA,ICED,POS,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IEDG(6,*),POS(3,*),ISLIP(2,20),FSLIP(20),CSLIP(20),FC(3)
     &         ,EN(3),IELC(3,*),ISLV(2),ICELA(2,*),ICEL(*),ICEDA(2,*)
     &         ,ICED(*),IBTE(4,*)
      DATA TOL /75.D0/
C----&------------------------------------------------------------------
      IS=ICELA(1,MASTER)
      IE=ICELA(2,MASTER)
C
      ICHK=0
C
      DO 200 I=IS,IE
        J=ICEL(I)
        CALL VRTXSEPA(ICHK,MASTER,FC,IBTE(1,J),POS,ITO)
        IF(ICHK .EQ. 1) GOTO 210
  200 CONTINUE
C
      RETURN
C
  210 IS=ICEDA(1,MASTER)
      IE=ICEDA(2,MASTER)
C
      NSLIP=0
C
      INCOMP=0
C
      DO 100 I=IS,IE
        J=ICED(I)
        CALL VRTXSLIP(NSLIP,ISLIP,FSLIP,CSLIP,INCOMP,MASTER,FC,EN,J
     &               ,IEDG(1,J),IELC,POS,ITO)
  100 CONTINUE
C
      IF(NSLIP .GT. 0) THEN
C
        COST=DCOS(TOL/90.D0*DASIN(1.D0))
C
        IP=0
        FSMAX=0.
C
        DO 300 I=1,NSLIP
          IF(CSLIP(I) .GT. COST .AND. FSLIP(I) .GT. FSMAX) THEN
            IP=I
            FSMAX=FSLIP(I)
          ENDIF
  300   CONTINUE
C
        IF(IP .EQ. 0) THEN
C
          CMAX=-1.1D0
C
          DO 400 I=1,NSLIP
            IF(CSLIP(I) .GT. CMAX) THEN
              IP=I
              CMAX=CSLIP(I)
            ENDIF
  400     CONTINUE
C
        ENDIF
C
        ISLV(1)=ISLIP(1,IP)
        ISLV(2)=ISLIP(2,IP)
C
      ELSEIF(INCOMP .EQ. 0) THEN
C
        ISLV(1)=1
        ISLV(2)=MASTER
C
      ENDIF
C
      END
