      SUBROUTINE EDGESLIP(ISLV,F,EN,IEDGE,IEDG,IELC,POS,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IEDG(6),POS(3,*),V12(3),V13(3),E1(3),E2L(3),E2R(3)
     &         ,E3L(3),E3R(3),E(3),IELC(3,*),F(3),ISLV(2),EN(3)
      DATA TOL /75.D0/
C----&------------------------------------------------------------------
      IG1=IEDG(1)
      IG2=IEDG(2)
      IER=IEDG(3)
      IEL=IEDG(5)
C
      IF(IER .EQ. 0) THEN
        WRITE(ITO,'(/X,A)') 'STOP IN SUB. EDGESLIP.'
        CALL ERRSTP(90,ITO)
      ENDIF
C
      CALL SUBVEC(V12,POS(1,IG2),POS(1,IG1),3)
      CALL DIRCOS(E1,V12,3)
C
C     --- LEFT FACE ---
C
      IF( IEL .GT. 0 ) THEN
C
        IG3=IELC(IEDG(6),IEL)
C
        CALL SUBVEC(V13,POS(1,IG3),POS(1,IG1),3)
        CALL CROSS2(V13,E1,E2L)
        CALL CROSS2(E1,E2L,E3L)
C
        CALL VECML1(FYL,F,E2L,3)
        CALL VECML1(FZL,F,E3L,3)
C
      ENDIF
C
C     --- RIGHT FACE ---
C
      IG3=IELC(IEDG(4),IER)
C
      CALL SUBVEC(V13,POS(1,IG3),POS(1,IG1),3)
      CALL CROSS2(E1,V13,E3R)
      CALL CROSS2(E3R,E1,E2R)
C
      CALL VECML1(FYR,F,E2R,3)
      CALL VECML1(FZR,F,E3R,3)
C
C     --- ANGLE ---
C
      IF( IEL .GT. 0 ) THEN
        CALL CROSS(E3R,E2L,E)
        CALL VECML1(SIN,E,E1,3)
      ENDIF
C
C     --- CHECK ---
C
      IF( IEL .EQ. 0 ) THEN
        IF( FZR .GE. 0. .AND. FYR .GE. 0. ) THEN
          ISLV(1)=3
          ISLV(2)=IER
        ENDIF
      ELSEIF( SIN .GT. 0. ) THEN
        IF( FZL .LE. 0. .AND. FYR .LE. 0. ) THEN
          ISLV(1)=2
          ISLV(2)=IEDGE
        ELSEIF( FYL .GE. 0. .AND. FZL .GT. 0. ) THEN
          ISLV(1)=3
          ISLV(2)=IEL
        ELSEIF( FZR .GE. 0. .AND. FYR .GT. 0. ) THEN
          ISLV(1)=3
          ISLV(2)=IER
        ENDIF
      ELSEIF( FYL .GE. 0. .AND. FZR .GE. 0. ) THEN
        COST=DCOS(TOL/90.D0*DASIN(1.D0))
        CALL VECML1(COSL,EN,E2L,3)
        CALL VECML1(COSR,EN,E3R,3)
        IF( COSL .GT. COST .AND. COSR .GT. COST ) THEN
          IF( FZL .GE. FYR ) THEN
            ISLV(1)=3
            ISLV(2)=IEL
          ELSE
            ISLV(1)=3
            ISLV(2)=IER
          ENDIF
        ELSEIF( COSL .GT. COST ) THEN
          ISLV(1)=3
          ISLV(2)=IEL
        ELSEIF( COSR .GT. COST ) THEN
          ISLV(1)=3
          ISLV(2)=IER
        ELSE
          IF( COSL .GE. COSR ) THEN
            ISLV(1)=3
            ISLV(2)=IEL
          ELSE
            ISLV(1)=3
            ISLV(2)=IER
          ENDIF
        ENDIF
      ENDIF
C
      END
