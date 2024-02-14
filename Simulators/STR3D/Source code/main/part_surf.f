      SUBROUTINE PART_SURF( IP_ENS, IELM, IPFC, NM, NPFC )

      DIMENSION IP_ENS(*), IPFC(10,NPFC), IELM(NM,*)

      DO I = 1, NPFC

        IE = IPFC(1,I)

        ITYP = IELM(2,IE)
        IMAT = IELM(4,IE)
        
        IF( ITYP == 2 .OR. ITYP == 6 ) IP_ENS(I) = IMAT

      ENDDO

      END