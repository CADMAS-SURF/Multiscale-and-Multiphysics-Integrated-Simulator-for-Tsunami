      SUBROUTINE RD_SPC1( J_SPC1, IP, CHAR, ITI )
C
      LOGICAL THRU
      CHARACTER*80 CHAR
      CHARACTER*8 C
      CHARACTER*8 BLK /'        '/
      DIMENSION IG(8), J_SPC1(9,*)
C
      IF( THRU( CHAR(33:40) ) ) THEN
C
        READ(CHAR,'(BN,8X,I8,A8,I8,8X,I8)') IDS, C, IG(1:2)
C
        IP = IP + 1
        J_SPC1(1,IP) = IDS
        J_SPC1(2,IP) = IG(1)
        J_SPC1(3,IP) = IG(2)
        CALL RESFR( J_SPC1(4,IP), C )
C
        RETURN
C
      ELSE
C
        READ(CHAR,'(BN,8X,I8,A8,6I8)') IDS, C, IG(1:6)
C
        DO I = 1, 6
          IF( IG(I) == 0 ) RETURN
          IP = IP + 1
          J_SPC1(1,IP) = IDS
          J_SPC1(2,IP) = IG(I)
          J_SPC1(3,IP) = IG(I)
          CALL RESFR( J_SPC1(4,IP), C )
        ENDDO
C
      ENDIF
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
C
          READ(CHAR,'(BN,8X,8I8)') IG(1:8)
C
          DO I = 1, 8
            IF( IG(I) == 0 ) RETURN
            IP = IP + 1
            J_SPC1(1,IP) = IDS
            J_SPC1(2,IP) = IG(I)
            J_SPC1(3,IP) = IG(I)
            CALL RESFR( J_SPC1(4,IP), C )
          ENDDO
C
        ELSE
C
          BACKSPACE(ITI)
          RETURN
C
        ENDIF
C
      ENDDO
C
      END
