      SUBROUTINE CT_TEMP( N_TMP, CHAR )
C
      CHARACTER*80 CHAR
      DIMENSION IG(3)
C
      READ(CHAR,'(BN,16X,3(I8,8X))') IG(:)
C
      DO I = 1, 3
        IF( IG(I) == 0 ) EXIT
        N_TMP = N_TMP + 1
      ENDDO
C
      END
