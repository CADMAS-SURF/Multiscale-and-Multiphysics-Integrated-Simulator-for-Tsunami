      SUBROUTINE MPCPREB(IDEP,CIDEP,IDX,IRH,IFRH,INDP,NINDP,NINDC,NIDEP
     &                  ,NIRH,ISLV,INDC,IEDG,IELC,IELQ,IVRQ,IEDQ,IFCQ
     &                  ,INDOF,POS,IFMDL,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ISLV(2,NINDC),POS(3,*),IELC(3,*),INDC(NINDC),IEDG(6,*)
     &         ,IDEP(2,NIDEP),CIDEP(NIDEP),IDX(2,NIDEP),IRH(2,NIRH)
     &         ,IFRH(NIRH),INDP(2,*),INDOF(6,*),IELQ(4,*),IVRQ(*)
     &         ,IEDQ(*),IFCQ(*)
C
      DATA ETOL / 1.D-2 /
C-----------------------------------------------------------------------
C
C     --- IDEP,IDX,IRH ---
C
      IP=1
      IP2=1
C
      DO 100 I=1,NINDC
        NS =INDC(I)
        IST=ISLV(1,I)
        MA =ISLV(2,I)
        IF( IFMDL .GT. 0 .AND. IST .GT. 0 .AND.
     &      ( INDOF(1,NS) .NE. 0 .OR. INDOF(2,NS) .NE. 0 .OR.
     &        INDOF(3,NS) .NE. 0 ) ) THEN
          WRITE(ITO,'(/X,A)') 'SLAVE POINT IS CONSTRAINED.'
          CALL ERRSTP(50,ITO)
        ENDIF
        IF( IST .EQ. 1 .OR. IST .EQ. 11 ) THEN
          CALL MPCVRTX(IDEP(1,IP),IDX(1,IP),IRH(1,IP2),IP2,NS,MA
     &                ,IVRQ(MA),IELQ,INDOF(1,NS),ITO)
          IP=IP+3
          IP2=IP2+3
          IF( IVRQ(MA) .GT. 0 ) IP2=IP2+9
        ELSEIF( IST .EQ. 2 .OR. IST .EQ. 12 ) THEN
          CALL MPCEDGE(IDEP(1,IP),IDX(1,IP),IRH(1,IP2),IP2,NS,IEDG(1,MA)
     &                ,IEDQ(MA),IELQ,INDOF(1,NS),POS,ETOL,ITO)
          IP=IP+2
          IP2=IP2+14
          IF( IEDQ(MA) .GT. 0 ) IP2=IP2+12
        ELSEIF( IST .EQ. 3 .OR. IST .EQ. 13 ) THEN
          CALL MPCFACE(IDEP(1,IP),IDX(1,IP),IRH(1,IP2),IP2,NS,IELC(1,MA)
     &                ,IFCQ(MA),IELQ,INDOF(1,NS),POS,ETOL,ITO)
          IP=IP+1
          IP2=IP2+11
          IF( IFCQ(MA) .GT. 0 ) IP2=IP2+3
        ELSEIF( IST .EQ. 4 .OR. IST .EQ. 14 ) THEN
          CALL MPCSTKE(IDEP(1,IP),IDX(1,IP),IRH(1,IP2),IP2,NS,IEDG(1,MA)
     &                ,IEDQ(MA),IELQ,INDOF(1,NS),ITO)
          IP=IP+3
          IP2=IP2+6
          IF( IEDQ(MA) .GT. 0 ) IP2=IP2+6
        ELSEIF( IST .EQ. 5 .OR. IST .EQ. 15 ) THEN
          CALL MPCSTKF(IDEP(1,IP),IDX(1,IP),IRH(1,IP2),IP2,NS,IELC(1,MA)
     &                ,IFCQ(MA),IELQ,INDOF(1,NS),ITO)
          IP=IP+3
          IP2=IP2+9
          IF( IFCQ(MA) .GT. 0 ) IP2=IP2+3
        ENDIF
  100 CONTINUE
C
C     --- CIDEP ---
C
      CIDEP(:) = 1.D0
C
C     --- INDP,NINDP ---
C
  500 IP=0
      DO 700 I=1,NIRH
        NODE=IRH(1,I)
        IFR =IRH(2,I)
        DO 710 J=1,NIDEP
          IF( NODE .EQ. IDEP(1,J) .AND. IFR .EQ. IDEP(2,J) ) GOTO 700
  710   CONTINUE
        DO 720 J=1,IP
          IF( NODE .EQ. INDP(1,J) .AND. IFR .EQ. INDP(2,J) ) GOTO 700
  720   CONTINUE
        IP=IP+1
        INDP(1,IP)=NODE
        INDP(2,IP)=IFR
  700 CONTINUE
      NINDP=IP
C
C     --- IFRH ---
C
      DO 800 I=1,NIRH
        NODE=IRH(1,I)
        IFR =IRH(2,I)
        DO 810 J=1,NIDEP
          IF( NODE .EQ. IDEP(1,J) .AND. IFR .EQ. IDEP(2,J) ) THEN
            IFRH(I)=-J
            GOTO 800
          ENDIF
  810   CONTINUE
        DO 820 J=1,NINDP
          IF( NODE .EQ. INDP(1,J) .AND. IFR .EQ. INDP(2,J) ) THEN
            IFRH(I)=J
            GOTO 800
          ENDIF
  820   CONTINUE
        WRITE(ITO,*) 'STOP IN SUB. MPCPREB!'
        CALL ERRSTP(90,ITO)
  800 CONTINUE
C
      END
