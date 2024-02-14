      SUBROUTINE CRH2SET(CRH,NINDC,ISLV,INDC,IEDG,IELC,IVRQ,IEDQ,IFCQ
     &                  ,POS,IRH)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ISLV(2,NINDC),POS(3,*),IELC(3,*),INDC(NINDC),IEDG(6,*)
     &         ,CRH(*),IRH(2,*),IVRQ(*),IEDQ(*),IFCQ(*)
C-----------------------------------------------------------------------
      IS=1
C
      DO 100 I=1,NINDC
        NS =INDC(I)
        IST=ISLV(1,I)
        MA =ISLV(2,I)
        IF( IST .EQ. 1 ) THEN
          CALL CRH2VRTX(CRH(IS),IVRQ(MA))
          IS=IS+3
          IF( IVRQ(MA) .GT. 0 ) IS=IS+9
        ELSEIF( IST .EQ. 2 ) THEN
          CALL CRH2EDGE(CRH(IS),NS,IRH(2,IS),IEDG(1,MA),IEDQ(MA),POS)
          IS=IS+14
          IF( IEDQ(MA) .GT. 0 ) IS=IS+12
        ELSEIF( IST .EQ. 3 ) THEN
          CALL CRH2FACE(CRH(IS),NS,IRH(2,IS),IRH(2,IS+1),IELC(1,MA)
     &                 ,IFCQ(MA),POS)
          IS=IS+11
          IF( IFCQ(MA) .GT. 0 ) IS=IS+3
        ELSEIF( IST .EQ. 4 ) THEN
          CALL CRH2STKE(CRH(IS),NS,IEDG(1,MA),IEDQ(MA),POS)
          IS=IS+6
          IF( IEDQ(MA) .GT. 0 ) IS=IS+6
        ELSEIF( IST .EQ. 5 ) THEN
          CALL CRH2STKF(CRH(IS),NS,IELC(1,MA),IFCQ(MA),POS)
          IS=IS+9
          IF( IFCQ(MA) .GT. 0 ) IS=IS+3
        ENDIF
  100 CONTINUE
C
      RETURN
      END
