      SUBROUTINE IST_UPDT(IST,SY,S,D,KK,MGP,NM,IELM,MAT,AMAT,EPS,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IELM(NM,*),MAT(2,*),AMAT(33,*),IST(MGP,*),S(6*MGP,*)
     &         ,D(21*MGP,*),SY(MGP,*),EPS(6*MGP,*),KK(*)

      NELM = KK(12)
      NELMC = KK(29)
      NELMX = KK(32)

      DO I = 1, NELM + NELMC + NELMX

        IF( I > NELM .AND. I <= NELM + NELMC ) CYCLE

        ITYP = IELM(2,I)
        NNP  = IELM(3,I)
        IMAT = IELM(4,I)

        IYLD = MAT(2,IMAT)

        E   = AMAT( 1,IMAT)
        ANU = AMAT( 2,IMAT)
        HD  = AMAT(12,IMAT)
        ALP = AMAT(13,IMAT)
        ST  = AMAT(14,IMAT)

        SELECT CASE( ITYP )
        CASE( 2, 6 )
          CALL IST_SOL(IST(1,I),SY(1,I),S(1,I),D(1,I),NNP,E,ANU,ST,IYLD
     &                ,HD,ALP,ITO)
        CASE( 3 )
          CALL IST_TRS(IST(1,I),SY(1,I),SY(2,I),S(1,I),D(1,I),E,ST,IYLD
     &                ,HD,EPS(1,I))
        CASE( 4 )
          CALL IST_BEAM(IST(1,I),SY(1,I),S(1,I),D(1,I),E,ANU,ST,IYLD,HD)
        END SELECT

      ENDDO

      END