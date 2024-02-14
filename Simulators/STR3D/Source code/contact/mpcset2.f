      SUBROUTINE MPCSET2(KK,INDOF,INDMPC,MPCF,NIDEP,IDEP,INDP,IROW_R
     &                  ,ICOL_R,IELM,NM,INDC,IELC,IEDG,IELQ,IFCQ,IEDQ
     &                  ,IVRQ,ISLV,IFRIC,IDWK)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION INDOF(6,*),INDMPC(2,6,*),MPCF(2,*),IDEP(2,NIDEP)
     &         ,INDP(2,*),IROW_R(NIDEP+1),ICOL_R(*),KK(*),IELM(NM,*)
     &         ,IDWK(*),INDC(*),IELC(3,*),IEDG(6,*),IELQ(4,*),IFCQ(*)
     &         ,IEDQ(*),IVRQ(*),ISLV(2,*),IFRIC(10,*),KN(5)
C-----------------------------------------------------------------------
C
C     --- INDOF,INDMPC,MPCF ---
C
      DO 300 I=1,NIDEP
        NODE=IDEP(1,I)
        IFR =IDEP(2,I)
        INDOF(IFR,NODE)=-2
        IS=IROW_R(I)
        IE=IROW_R(I+1)-1
        INDMPC(1,IFR,NODE)=IS          
        INDMPC(2,IFR,NODE)=IE
        DO 310 J=IS,IE
          IC=ICOL_R(J)
          MPCF(1,J)=INDP(1,IC)
          MPCF(2,J)=INDP(2,IC)
  310   CONTINUE
  300 CONTINUE
C
C     ----- NCRMAX, NRAMAX, NESTF -----
C
      NELM = KK(12)
      NELMC = KK(29)
      NELMX = KK(32)
C
      NCRMAX = 0
      NRAMAX = 0
C
      DO I = 1, NELM + NELMC + NELMX
        IF( I > NELM .AND. I <= NELM + NELMC ) CYCLE
        ITYP = IELM(2,I)
        IF( ITYP == 1 .OR. ITYP == 4 ) THEN
          NDF = 6
        ELSE
          NDF = 3
        ENDIF
        ND = IELM(3,I)
        CALL NCRSET(ND,NDF,IELM(8,I),INDOF,INDMPC,MPCF,IDWK,NCRMAX
     &             ,NRAMAX)
      ENDDO
C
      NINDC = KK(95)
      NINDCX = KK(103)
C
      DO I = 1, NINDC + NINDCX
        IF( IFRIC(1,I) == 0 ) CYCLE
        NS = INDC(I)
        IST = ISLV(1,I)
        MA  = ISLV(2,I)
        CALL MSTNOD(ND,KN(2),IST,MA,IELC,IEDG,IELQ,IFCQ,IEDQ,IVRQ)
        ND = ND + 1
        KN(1) = NS
        CALL NCRSET(ND,3,KN,INDOF,INDMPC,MPCF,IDWK,NCRMAX,NRAMAX)
      ENDDO
C
      NESTF = ( NCRMAX + 1 ) * NCRMAX / 2
C
      KK(33) = NESTF
      KK(34) = NCRMAX
      KK(35) = NRAMAX
C
      END
