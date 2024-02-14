      SUBROUTINE CT_SPC( N_SPC, CHAR )
C
      CHARACTER*80 CHAR
      DIMENSION IG(2)
C
C
      READ(CHAR,'(BN,16X,I8,16X,I8)') IG(:)
C
      DO I = 1, 2
        IF( IG(I) == 0 ) EXIT
        N_SPC = N_SPC + 1
      ENDDO
C
      END
