      SUBROUTINE MPCCORR(UG,POS,ISLV,RSLV,NINDC,NRANK,INDOF,IELC,INDC
     &                  ,IEDG,IRANK)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ISLV(2,NINDC),RSLV(3,NINDC),POS(3,*),IELC(3,*)
     &         ,INDC(NINDC),IEDG(6,*),UG(6,*),INDOF(6,*),IRANK(NINDC)
C----&------------------------------------------------------------------
      DO 200 IR=1,NRANK
        DO 100 I=1,NINDC
          IF( IRANK(I) .NE. IR ) CYCLE
          NS =INDC(I)
          IST=ISLV(1,I)
          MA =ISLV(2,I)
          IF( IST .EQ. 11 ) THEN
            CALL MPCCORR11(UG,POS,NS,MA)
            ISLV(1,I)=IST-10
          ELSEIF( IST .EQ. 12 .OR. IST .EQ. 14 ) THEN
            CALL MPCCORR12(UG,POS,NS,RSLV(1,I),INDOF(1,NS),IEDG(1,MA))
            ISLV(1,I)=IST-10
          ELSEIF( IST .EQ. 13 .OR. IST .EQ. 15 ) THEN
            CALL MPCCORR13(UG,POS,NS,RSLV(1,I),INDOF(1,NS),IELC(1,MA))
            ISLV(1,I)=IST-10
          ELSEIF( IST .EQ. 2 ) THEN
            CALL MPCCORR2(UG,POS,NS,INDOF(1,NS),IEDG(1,MA))
          ELSEIF( IST .EQ. 3 ) THEN
            CALL MPCCORR3(UG,POS,NS,INDOF(1,NS),IELC(1,MA))
          ENDIF
  100   CONTINUE
  200 CONTINUE
C
      END
