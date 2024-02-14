      SUBROUTINE PCNSTRI(INDOP,IELM,NNOD,NELM,NM)

      DIMENSION INDOP(NNOD),IELM(NM,NELM)

      INDOP(:) = 1

      DO I = 1, NELM
        
        ITYP = IELM(2,I)
        NNP = IELM(3,I)

        IF( ITYP == 6 ) INDOP( IELM(8:7+NNP,I) ) = 0

      ENDDO

      END
