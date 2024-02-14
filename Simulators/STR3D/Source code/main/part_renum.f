      SUBROUTINE PART_RENUM( N_PART, NP_ENS, IP_ENS, KK, IELM, NM )
C
      DIMENSION NP_ENS(6), IP_ENS(*), KK(*), IELM(NM,*)
      INTEGER, POINTER :: IPART(:,:)
C
      NMAT  = KK(11)
      NELM  = KK(12)
      NTHK  = KK(13)
      NRODA = KK(15)
      NBARD = KK(17)
C
      MAXP = MAX0( NTHK, NMAT, NRODA, NBARD )
C
      ALLOCATE( IPART(MAXP,6) )
C
      IPART(:,:) = 0
C
      DO I = 1, NELM
C
        ITYP = IELM(2,I)
C
        SELECT CASE( ITYP )
        CASE( 1, 3, 4 )
          IPROP = IELM(5,I)
        CASE( 2, 6 )
          IPROP = IELM(4,I)
        CASE( 5 )
          CYCLE
        END SELECT
C
        IPART(IPROP,ITYP) = 1
C
      ENDDO
C
      IP = 0
C
      NP_ENS(:) = 0
C
      DO ITYP = 1, 6
        DO IPROP = 1, MAXP
          IF( IPART(IPROP,ITYP) == 1 ) THEN
            IP = IP + 1
            IPART(IPROP,ITYP) = IP
            NP_ENS(ITYP) = NP_ENS(ITYP) + 1
          ENDIF
        ENDDO
      ENDDO
C
      N_PART = IP
C
      IP_ENS(1:NELM) = 0
C
      DO I = 1, NELM
C
        ITYP = IELM(2,I)
C
        SELECT CASE( ITYP )
        CASE( 1, 3, 4 )
          IPROP = IELM(5,I)
        CASE( 2, 6 )
          IPROP = IELM(4,I)
        CASE( 5 )
          CYCLE
        END SELECT
C
        IP_ENS(I) = IPART(IPROP,ITYP)
C
      ENDDO
C
      DEALLOCATE( IPART )
C
      END