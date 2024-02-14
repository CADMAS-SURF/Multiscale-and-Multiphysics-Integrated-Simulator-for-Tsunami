      SUBROUTINE GLBSTFG(STF,RHV,KK,NCRMAX,NRAMAX,RR,GRID,IELM,NM
     &                  ,AMAT,INDOF,INDMPC,MPCF,RMPC,INDOP,IDSK,IDCG
     &                  ,DUG,DPG,DT1,DT2,ITER,ITER2,IMPC,IDYN,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),RR(*),IELM(NM,*),EKUP(1200),EKPP(400),ECPU(1200)
     &         ,ECPP(400),EMPU(1200),GRID(3,*),AMAT(33,*)
     &         ,ELHM12(NCRMAX*20),ELHM21(20*NCRMAX),ELHM22(400)
     &         ,RAMBDA(NRAMAX),INDOF(6,*),INDMPC(2,6,*),MPCF(*),RMPC(*)
     &         ,IDCR(2,NCRMAX),RHV(*),DUG(6,*),DPG(*),INDOP(*),STF(*)
     &         ,IDSK(*),IDCG(*)

      NELM = KK(12)
      NELMC = KK(29)
      NELMX = KK(32)

      BETA = RR(4)
      ALP = RR(7)

      DT12 = .5D0 * ( DT1 + DT2 )

      DO I = 1, NELM + NELMC + NELMX

        IF( I > NELM .AND. I <= NELM + NELMC ) CYCLE

        ITYP = IELM(2,I)

        IF( ITYP /= 6 ) CYCLE

        ND = IELM(3,I)
        IMAT = IELM(4,I)

        ND3 = ND * 3
        N12 = ND3 * ND
        N21 = ND * ND3
        N22 = ND * ND

        CALL GEOMTX(EKUP,EKPP,ECPU,ECPP,EMPU,ND,ND3,IELM(8,I),GRID
     &             ,AMAT(1,IMAT),ITO)

        IF( IDYN == 1 ) THEN
          ELHM12(1:N12) = -BETA * EKUP(1:N12)
          ELHM21(1:N21) = EMPU(1:N21) / ( DT2 * DT12 )
     &                    + ECPU(1:N21) / DT12 * .5D0
          ELHM22(1:N22) = ECPP(1:N22) / DT12 * .5D0 + ALP * EKPP(1:N22)
        ELSEIF( IDYN == 0 ) THEN
          ELHM12(1:N12) = -EKUP(1:N12)
          ELHM21(1:N21) = 0.
          ELHM22(1:N22) = EKPP(1:N22)
        ENDIF

        CALL MPCTRNSG(IMPC,IELM(8,I),ND,3,ND3,RAMBDA,NRAMAX,INDOF,INDMPC
     &               ,MPCF,RMPC,ELHM12,ELHM21,IDCR,NCR)

        IF( ITER == 1 .AND. ITER2 == 1 ) 
     &    CALL SPCDRHVG(RHV,ELHM12,ELHM21,ELHM22,DUG,DPG,IELM(8,I),ND
     &                 ,IDCR,NCR,INDOF,INDOP)

        CALL CGADMG(STF,ELHM12,ELHM21,ELHM22,IELM(8,I),ND,IDCR,NCR,INDOF
     &             ,INDOP,IDSK,IDCG,ITO)

      ENDDO

      END
