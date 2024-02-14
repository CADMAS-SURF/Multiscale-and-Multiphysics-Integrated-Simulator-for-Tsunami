      SUBROUTINE MPCSIZE(NIDEP,NIRH,NINDC,ISLV,IVRQ,IEDQ,IFCQ)
C
      DIMENSION ISLV(2,*),IVRQ(*),IEDQ(*),IFCQ(*)
C
      NIDEP=0
      NIRH=0
C
      DO 100 K=1,NINDC
        IST=ISLV(1,K)
        MA =ISLV(2,K)
        IF( IST .EQ. 1 .OR. IST .EQ. 11 ) THEN
          I=3
          J=3
          IF( IVRQ(MA) .GT. 0 ) J=J+9
        ELSEIF( IST .EQ. 2 .OR. IST .EQ. 12 ) THEN
          I=2
          J=14
          IF( IEDQ(MA) .GT. 0 ) J=J+12
        ELSEIF( IST .EQ. 3 .OR. IST .EQ. 13 ) THEN
          I=1
          J=11
          IF( IFCQ(MA) .GT. 0 ) J=J+3
        ELSEIF( IST .EQ. 4 .OR. IST .EQ. 14 ) THEN
          I=3
          J=6
          IF( IEDQ(MA) .GT. 0 ) J=J+6
        ELSEIF( IST .EQ. 5 .OR. IST .EQ. 15 ) THEN
          I=3
          J=9
          IF( IFCQ(MA) .GT. 0 ) J=J+3
        ELSE
          I=0
          J=0
        ENDIF
        NIDEP=NIDEP+I
        NIRH=NIRH+J
  100 CONTINUE
C
      END
