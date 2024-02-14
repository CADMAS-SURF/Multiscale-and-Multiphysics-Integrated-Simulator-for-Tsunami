      SUBROUTINE VRTXSLIP(NSLIP,ISLIP,FSLIP,CSLIP,INCOMP,IP1,F,EN,IEDGE
     &                   ,IEDG,IELC,POS,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IEDG(6),POS(3,*),V12(3),V13(3),E1(3),E2R(3),E2L(3)
     &         ,E3R(3),E3L(3),E(3),IELC(3,*),F(3),ISLIP(2,*),FSLIP(*)
     &         ,CSLIP(*),EN(3),EA(3)
C----&------------------------------------------------------------------
      IG1=IEDG(1)
      IG2=IEDG(2)
      IE1=IEDG(3)
      IE2=IEDG(5)
C
C     IF(IE2 .EQ. 0) THEN
C       WRITE(ITO,'(/X,A)') 'CONTACT SURFACE IS SMALL.'
C       CALL ERRSTP(51,ITO)
C     ENDIF
C
      IF(IP1 .EQ. IG1) THEN
        IP2=IG2
        IEL=IE2
        IER=IE1
        IF(IEL .EQ. 0) THEN
          INCOMP=1
          RETURN
        ENDIF
        IP3L=IELC(IEDG(6),IEL)
        IP3R=IELC(IEDG(4),IER)
      ELSEIF(IP1 .EQ. IG2) THEN
        IP2=IG1
        IEL=IE1
        IER=IE2
        IP3L=IELC(IEDG(4),IEL)
        IP3R=IELC(IEDG(6),IER)
      ELSE
        WRITE(ITO,'(/X,A)') 'STOP IN SUB. VRTXSLIP!'
        CALL ERRSTP(90,ITO)
      ENDIF
C
      CALL SUBVEC(V12,POS(1,IP2),POS(1,IP1),3)
      CALL DIRCOS(E1,V12,3)
      CALL VECML1(FX,F,E1,3)
C
C     --- LEFT FACE ---
C
      CALL SUBVEC(V13,POS(1,IP3L),POS(1,IP1),3)
      CALL CROSS2(V13,E1,E2L)
      CALL CROSS2(E1,E2L,E3L)
C
      CALL VECML1(FYL,F,E2L,3)
      CALL VECML1(FZL,F,E3L,3)
C
      CALL VECML1(X3,V13,E1,3)
      CALL VECML2(RL13,V13,3)
      COS0=X3/RL13
      FXZL=DSQRT(FX*FX+FZL*FZL)
      IF(FXZL .GT. 1.D-10) THEN
        COS=FX/FXZL
      ELSE
        RETURN
      ENDIF
C
      IF(IER .GT. 0) THEN
C
C       --- RIGHT FACE ---
C
        CALL SUBVEC(V13,POS(1,IP3R),POS(1,IP1),3)
        CALL CROSS2(E1,V13,E3R)
        CALL CROSS2(E3R,E1,E2R)
C
        CALL VECML1(FYR,F,E2R,3)
        CALL VECML1(FZR,F,E3R,3)
C
C       --- ANGLE ---
C
        CALL CROSS(E3R,E2L,E)
        CALL VECML1(SIN,E,E1,3)
C
      ENDIF
C
C     --- CHECK ---
C
      IF(FYL .GE. 0. .AND. FZL .GT. 0. .AND. COS .GT. COS0) THEN
        NSLIP=NSLIP+1
        ISLIP(1,NSLIP)=3
        ISLIP(2,NSLIP)=IEL
        FSLIP(NSLIP)=FXZL
        CALL VECML1(CSLIP(NSLIP),EN,E2L,3)
        RETURN
      ENDIF
C
      IF(IER .EQ. 0) RETURN
C
      IF(SIN .GT. 0. .AND. FZL .LE. 0. .AND. FYR .LE. 0. .AND.
     &   FX .GT. 0.) THEN
        NSLIP=NSLIP+1
        ISLIP(1,NSLIP)=2
        ISLIP(2,NSLIP)=IEDGE
        FSLIP(NSLIP)=FX
        EA(:) = E2L(:) + E3R(:)
        CALL DIRCOS(EA,EA,3)
        CALL VECML1(CSLIP(NSLIP),EN,EA,3)
      ENDIF
C
      END
