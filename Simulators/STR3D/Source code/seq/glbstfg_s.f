      SUBROUTINE GLBSTFG_S(STF,RHV,KK,RR,GRID,IELM,NM,AMAT,INDOP,IDSK
     &                    ,IDCG,DPG,DT1,DT2,IDYN,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),RR(*),IELM(NM,*),EKPP(210),ECPP(210),GRID(3,*)
     &         ,AMAT(33,*),ELHM(210),RHV(*),DPG(*),INDOP(*),STF(*)
     &         ,IDSK(*),IDCG(*)

      NELM = KK(12)

      ALP = RR(7)

      DT12 = .5D0 * ( DT1 + DT2 )

      STF(1:KK(20)) = 0.

      DO I = 1, NELM

        ITYP = IELM(2,I)

        IF( ITYP /= 6 ) CYCLE

        ND = IELM(3,I)
        IMAT = IELM(4,I)

        NN = ( ND + 1 ) * ND / 2

        CALL GEOMTX_S(EKPP,ECPP,ND,NN,IELM(8,I),GRID,AMAT(1,IMAT),ITO)

        IF( IDYN == 1 ) THEN
          ELHM(1:NN) = ECPP(1:NN) / DT12 * .5D0 + ALP * EKPP(1:NN)
        ELSEIF( IDYN == 0 ) THEN
          ELHM(1:NN) = EKPP(1:NN)
        ENDIF

        CALL SPCDRHVG_S(RHV,ELHM,IELM(8,I),ND,INDOP,DPG)

        CALL CGADMG_S(STF,ELHM,IELM(8,I),ND,IDSK,IDCG,INDOP,KK(21),ITO)

      ENDDO

      END
