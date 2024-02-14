      SUBROUTINE SF_CENT_ELEM(GELM,NELM,IELM,POS)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION GELM(3,NELM),IELM(24,NELM),POS(3,*)

      GELM(:,:) = 0.

      DO I = 1, NELM
        N = IELM(4,I)
        DO J = 1, N
          GELM(:,I) = GELM(:,I) + POS(:,IELM(4+J,I))
        ENDDO
        GELM(:,I) = GELM(:,I) / DBLE(N)
      ENDDO

      END