      SUBROUTINE NCGSET(KK,IELM,NM)
C
      USE MPI_PARAM
C
      DIMENSION KK(*),IELM(NM,*)
C-----------------------------------------------------------------------
      NELM = KK(12)
C
      NCGMAX = 0
      NCGMAXP = 0
C
      DO I = 1, NELM
C
        ITYP = IELM(2,I)
        ND = IELM(3,I)
C
        SELECT CASE( ITYP )
        CASE( 1 )
C
          SELECT CASE( ND )
          CASE( 3 )
            NCG = 42
          CASE( 4 )
            NCG = 54
          CASE( 6 )
            NCG = 114
          CASE( 8, 9 )
            NCG = 126
          END SELECT
C
        CASE( 2, 6 )
C
          SELECT CASE( ND )
          CASE( 4 )
            NCG = 45
          CASE( 6 )
            NCG = 63
          CASE( 8 )
            NCG = 81
          CASE( 10 )
            NCG = 90
          CASE( 15 )
            NCG = 213
          CASE( 20 )
            NCG = 189
          END SELECT
C
          IF( ITYP == 6 ) THEN
            NCGP = NCG / 3
            NCGMAXP = MAX0( NCGMAXP, NCGP )
          ENDIF
C
        CASE( 3, 5 )
C
          NCG = 45
C
        CASE( 4 )
C
          NCG = 90
C
        END SELECT
C
        NCGMAX = MAX0( NCGMAX, NCG )
C
      ENDDO
C
      IF( MYRANK > 0 ) THEN
        CALL CG_MPI_ALLREDUCE_I(NCGMAX,MAX,1,1)
        NCGMAX = MAX
        CALL CG_MPI_ALLREDUCE_I(NCGMAXP,MAX,1,1)
        NCGMAXP = MAX
      ENDIF
C
      KK(24) = NCGMAX
      KK(25) = NCGMAXP
C
      IF( MYRANK == 1 ) CALL SEND_KK(KK,25)
C
      IF( KK(1) == 0 .AND. KK(25) > 0 ) KK(21) = KK(21) + 10
C
      END
