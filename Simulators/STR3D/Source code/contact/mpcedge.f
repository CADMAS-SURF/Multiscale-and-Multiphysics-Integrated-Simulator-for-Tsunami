      SUBROUTINE MPCEDGE(IDEP,IDX,IRH,IS,NS,IG,IEDQ,IELQ,INDOF,POS,ETOL
     &                  ,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IG(2),V12(3),E1(3),POS(3,*),IDEP(2,2),IDX(2,2)
     &         ,IRH(2,*),ISRT(3),INDOF(3),IELQ(4,*),NDQ(4)
C----&------------------------------------------------------------------
C
C     --- E1 ---
C
      CALL SUBVEC(V12,POS(1,IG(2)),POS(1,IG(1)),3)
      CALL DIRCOS(E1,V12,3)
C
C     --- J1,J2,J3 ---
C
      ISRT(1)=1
      DO 200 I=2,3
        DO 210 J=1,I-1
          JJ=ISRT(J)
          IF( DABS(E1(I)) .GT. DABS(E1(JJ)) ) THEN
            CALL SHIFTB0(ISRT,J,I-1,1)
            ISRT(J)=I
            GOTO 200
          ENDIF
  210   CONTINUE
        ISRT(I)=I
  200 CONTINUE
C
      IF( INDOF(ISRT(2)) .EQ. 0 .AND. INDOF(ISRT(3)) .EQ. 0 ) THEN
        J1=ISRT(1)
      ELSEIF( INDOF(ISRT(1)) .EQ. 0 .AND. INDOF(ISRT(3)) .EQ. 0 
     &        .AND. DABS( E1(ISRT(2)) ) .GT. ETOL ) THEN
        J1=ISRT(2)
      ELSEIF( INDOF(ISRT(1)) .EQ. 0 .AND. INDOF(ISRT(2)) .EQ. 0
     &        .AND. DABS( E1(ISRT(3)) ) .GT. ETOL ) THEN
        J1=ISRT(3)
      ELSE
        WRITE(ITO,'(/X,A)') 'SLAVE POINT IS CONSTRAINED.'
        CALL ERRSTP(50,ITO)
      ENDIF
C
      IF(J1 .EQ. 1) THEN
        J2=2
        J3=3
      ELSEIF(J1 .EQ. 2) THEN
        J2=3
        J3=1
      ELSEIF(J1 .EQ. 3) THEN
        J2=1
        J3=2
      ENDIF
C
C     --- IDEP,CIDEP,IDX,IRH ---
C
      IDEP(1,1)=NS
      IDEP(2,1)=J2
      IDEP(1,2)=NS
      IDEP(2,2)=J3
C
      IF( IEDQ .EQ. 0 ) THEN
C
        IDX(1,1)=IS
        IDX(2,1)=IS+6
        IDX(1,2)=IS+7
        IDX(2,2)=IS+13
C
        IP=0
        DO 100 I=1,2
          IP=IP+1
          IRH(1,IP)=NS
          IRH(2,IP)=J1
          DO 110 J=1,2
            DO 120 K=1,3
              IP=IP+1
              IRH(1,IP)=IG(J)
              IRH(2,IP)=K
  120       CONTINUE
  110     CONTINUE
  100   CONTINUE
C
      ELSE
C
        IDX(1,1)=IS
        IDX(2,1)=IS+12
        IDX(1,2)=IS+13
        IDX(2,2)=IS+25
C
        NDQ(1)=IG(1)
C
        IF( IG(1) .EQ. IELQ(1,IEDQ) ) THEN
          NDQ(2)=IELQ(2,IEDQ)
          NDQ(3)=IELQ(3,IEDQ)
          NDQ(4)=IELQ(4,IEDQ)
        ELSEIF( IG(1) .EQ. IELQ(2,IEDQ) ) THEN
          NDQ(2)=IELQ(1,IEDQ)
          NDQ(3)=IELQ(3,IEDQ)
          NDQ(4)=IELQ(4,IEDQ)
        ELSEIF( IG(1) .EQ. IELQ(3,IEDQ) ) THEN
          NDQ(2)=IELQ(1,IEDQ)
          NDQ(3)=IELQ(2,IEDQ)
          NDQ(4)=IELQ(4,IEDQ)
        ELSEIF( IG(1) .EQ. IELQ(4,IEDQ) ) THEN
          NDQ(2)=IELQ(1,IEDQ)
          NDQ(3)=IELQ(2,IEDQ)
          NDQ(4)=IELQ(3,IEDQ)
        ENDIF
C
        IP=0
        DO 300 I=1,2
          IP=IP+1
          IRH(1,IP)=NS
          IRH(2,IP)=J1
          DO 310 J=1,4
            DO 320 K=1,3
              IP=IP+1
              IRH(1,IP)=NDQ(J)
              IRH(2,IP)=K
  320       CONTINUE
  310     CONTINUE
  300   CONTINUE
C
      ENDIF
C
      END
