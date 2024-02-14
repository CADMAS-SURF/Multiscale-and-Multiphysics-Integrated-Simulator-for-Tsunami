      SUBROUTINE CT_PLOAD2( N_PL4, CHAR )
C
      LOGICAL THRU
      CHARACTER*80 CHAR
      DIMENSION IDE(6)
C
      IF( THRU( CHAR(33:40) ) ) THEN
        N_PL4 = N_PL4 + 1
      ELSE
        READ(CHAR,'(BN,24X,6I8)') IDE(:)
        DO I = 1, 6
          IF( IDE(I) == 0 ) EXIT
          N_PL4 = N_PL4 + 1
        ENDDO
      ENDIF
C
      END