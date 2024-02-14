      SUBROUTINE RESFR( IFR, CHAR )
C
      CHARACTER*8 CHAR
      DIMENSION IFR(6)
C
      IFR(:) = 0
C
      DO I = 1, 8
        READ(CHAR(I:I),'(I1)') NO
        IF( NO > 0 ) IFR(NO) = 1
      ENDDO
C
      END