      SUBROUTINE MPCFACE(IDEP,IDX,IRH,IS,NS,IELC,IFCQ,IELQ,INDOF,POS
     &                  ,ETOL,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION POS(3,*),IELC(3),E(3),V12(3),V13(3),IDEP(2),IDX(2)
     &         ,IRH(2,*),ISRT(3),INDOF(3),IELQ(4,*),NDQ(4)
C----&------------------------------------------------------------------
      CALL SUBVEC(V12,POS(1,IELC(2)),POS(1,IELC(1)),3)
      CALL SUBVEC(V13,POS(1,IELC(3)),POS(1,IELC(1)),3)
      CALL CROSS2(V12,V13,E)
C
      ISRT(1)=1
      DO 100 I=2,3
        DO 110 J=1,I-1
          JJ=ISRT(J)
          IF( DABS(E(I)) .GT. DABS(E(JJ)) ) THEN
            CALL SHIFTB0(ISRT,J,I-1,1)
            ISRT(J)=I
            GOTO 100
          ENDIF
  110   CONTINUE
        ISRT(I)=I
  100 CONTINUE
C
      IF( INDOF(ISRT(1)) .EQ. 0 ) THEN
        J1=ISRT(1)
        J2=ISRT(2)
        J3=ISRT(3)
      ELSEIF( INDOF(ISRT(2)) .EQ. 0 .AND. 
     &        DABS( E(ISRT(2)) ) .GT. ETOL ) THEN
        J1=ISRT(2)
        J2=ISRT(1)
        J3=ISRT(3)
      ELSEIF( INDOF(ISRT(3)) .EQ. 0 .AND. 
     &        DABS( E(ISRT(3)) ) .GT. ETOL ) THEN
        J1=ISRT(3)
        J2=ISRT(1)
        J3=ISRT(2)
      ELSE
        WRITE(ITO,'(/X,A)') 'SLAVE POINT IS CONSTRAINED.'
        CALL ERRSTP(50,ITO)
      ENDIF
C
C     --- IDEP,CIDEP,IDX,IRH ---
C
      IDEP(1)=NS
      IDEP(2)=J1
C
      IF( IFCQ .EQ. 0 ) THEN
C
        IDX(1)=IS
        IDX(2)=IS+10
C
        IRH(1,1)=NS
        IRH(2,1)=J2
C   
        IRH(1,2)=NS
        IRH(2,2)=J3
C
        IP=2
        DO 130 K1=1,3
          DO 131 K2=1,3
            IP=IP+1
            IRH(1,IP)=IELC(K1)
            IRH(2,IP)=K2
  131     CONTINUE
  130   CONTINUE
C
      ELSE
C
        IDX(1)=IS
        IDX(2)=IS+13
C
        IRH(1,1)=NS
        IRH(2,1)=J2
C   
        IRH(1,2)=NS
        IRH(2,2)=J3
C
        NDQ(1)=IELC(1)
        NDQ(2)=IELC(2)
C
        IF( IELC(1) .EQ. IELQ(1,IFCQ) ) THEN
          NDQ(3)=IELQ(3,IFCQ)
          NDQ(4)=IELQ(4,IFCQ)
        ELSEIF( IELC(1) .EQ. IELQ(2,IFCQ) ) THEN
          NDQ(3)=IELQ(1,IFCQ)
          NDQ(4)=IELQ(4,IFCQ)
        ELSEIF( IELC(1) .EQ. IELQ(3,IFCQ) ) THEN
          NDQ(3)=IELQ(1,IFCQ)
          NDQ(4)=IELQ(2,IFCQ)
        ELSEIF( IELC(1) .EQ. IELQ(4,IFCQ) ) THEN
          NDQ(3)=IELQ(2,IFCQ)
          NDQ(4)=IELQ(3,IFCQ)
        ENDIF
C
        IP=2
        DO 140 K1=1,4
          DO 141 K2=1,3
            IP=IP+1
            IRH(1,IP)=NDQ(K1)
            IRH(2,IP)=K2
  141     CONTINUE
  140   CONTINUE
C
      ENDIF
C
      END
