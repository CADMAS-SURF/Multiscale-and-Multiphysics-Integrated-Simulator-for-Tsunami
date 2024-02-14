      SUBROUTINE CNTRFC(RFCI,NNOD,NINDC,NIDEP,NRANK,FTI,FTO,INDC,IRANK
     &                 ,INDCR)

      USE MPC_WORK
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IRANK(NINDC),RFCI(6,NNOD),FTI(6,NNOD),FTO(6,NNOD)
     &         ,INDCR(NNOD),INDC(NINDC)

      INDCR(:) = 0

      DO I = 1, NINDC
        INDCR( INDC(I) ) = I
      ENDDO

      RFCI(:,:) = 0.D0

      DO IR = NRANK, 1, -1

        DO I = 1, NIDEP

          NDS = IDEP(1,I)
          IFS = IDEP(2,I)

          IF( IRANK(INDCR(NDS)) /= IR ) CYCLE

          RFCI(IFS,NDS) = FTI(IFS,NDS) - FTO(IFS,NDS) - RFCI(IFS,NDS)

          JS = IDX(1,I)
          JE = IDX(2,I)

          DO J = JS, JE

            NDM = IRH(1,J)
            IFM = IRH(2,J)

            IF( NDM == NDS ) THEN
              RFCI(IFM,NDM) = -CRH(J) * RFCI(IFS,NDS)
            ELSE
              RFCI(IFM,NDM) = RFCI(IFM,NDM) - CRH(J) * RFCI(IFS,NDS)
            ENDIF

          ENDDO

        ENDDO

      ENDDO

      END
