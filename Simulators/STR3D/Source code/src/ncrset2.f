      SUBROUTINE NCRSET2(KK,IELM,NM)
C
      DIMENSION KK(*),IELM(NM,*)
C
C     ----- NCRMAX, NESTF -----
C
      NELM = KK(12)
      NELMC = KK(29)
      NELMX = KK(32)
C
      NCRMAX = 0
C
      DO I = 1, NELM + NELMC + NELMX
        IF( I > NELM .AND. I <= NELM + NELMC ) CYCLE
        ITYP = IELM(2,I)
        IF( ITYP == 1 .OR. ITYP == 4 ) THEN
          NDF = 6
        ELSE
          NDF = 3
        ENDIF
        ND = IELM(3,I)
        NCRMAX = MAX0( NCRMAX, ND*NDF )
      ENDDO
C
      NESTF = ( NCRMAX + 1 ) * NCRMAX / 2
C
      KK(33) = NESTF
      KK(34) = NCRMAX
C
      END
