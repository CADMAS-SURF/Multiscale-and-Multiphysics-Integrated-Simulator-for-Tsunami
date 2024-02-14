      SUBROUTINE CT_TEMPD( N_TMPD, CHAR )
C
      CHARACTER*80 CHAR
      DIMENSION ID(3)
C
      READ(CHAR,'(BN,8X,3(I8,8X))') ID(:)
C
      DO I = 1, 3
        IF( ID(I) == 0 ) EXIT
        N_TMPD = N_TMPD + 1
      ENDDO
C
      END
