      SUBROUTINE NPFLOWD(FTID,KK,RR,GRID,IELM,NM,AMAT,DPG,IDYN,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),RR(*),IELM(NM,*),EKUP(1200),EKPP(400),ECPU(1200)
     &         ,ECPP(400),EMPU(1200),GRID(3,*),AMAT(33,*),ELHM12(1200)
     &         ,DPG(*),FTID(6,*)

      NELM = KK(12)
      NELMC = KK(29)
      NELMX = KK(32)

      BETA = RR(4)

      DO I = 1, NELM + NELMC + NELMX

        IF( I > NELM .AND. I <= NELM + NELMC ) CYCLE

        ITYP = IELM(2,I)

        IF( ITYP /= 6 ) CYCLE

        ND = IELM(3,I)
        IMAT = IELM(4,I)

        ND3 = ND * 3
        N12 = ND3 * ND

        CALL GEOMTX(EKUP,EKPP,ECPU,ECPP,EMPU,ND,ND3,IELM(8,I),GRID
     &             ,AMAT(1,IMAT),ITO)

        IF( IDYN == 1 ) THEN
          ELHM12(1:N12) = -BETA * EKUP(1:N12)
        ELSEIF( IDYN == 0 ) THEN
          ELHM12(1:N12) = -EKUP(1:N12)
        ENDIF

        CALL GNPFLWD(FTID,ELHM12,DPG,IELM(8,I),ND)

      ENDDO

      END
