      SUBROUTINE RD_TEMPP1( J_TMP1, R_TMP1, IP, CHAR, ITI )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      LOGICAL THRU
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      CHARACTER*16 BLK2 /'                '/
      DIMENSION JE(8), J_TMP1(4,*), R_TMP1(2,*)
C
      IS = IP + 1
C
      IF( CHAR(41:56) == BLK2 ) THEN
        ITYP = 1
        READ(CHAR,'(BN,8X,2I8,2F8.0)') IDS, JE(1), T1, T2
      ELSE
        ITYP = 2
        READ(CHAR,'(BN,8X,2I8,16X,2F8.0)') IDS, JE(1), T1, T2
      ENDIF
C
      IP = IP + 1
      J_TMP1(2:3,IP) = JE(1)
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
C
          IF( THRU( CHAR(17:24) ) ) THEN
C
            READ(CHAR,'(BN,8X,I8,8X,I8)') JE(1:2)
C
            IP = IP + 1
            J_TMP1(2:3,IP) = JE(1:2)
C
            IF( THRU( CHAR(41:48) ) ) THEN
C
              READ(CHAR,'(BN,32X,I8,8X,I8)') JE(1:2)
C
              IP = IP + 1
              J_TMP1(2:3,IP) = JE(1:2)
C
            ENDIF
C
          ELSE
C
            READ(CHAR,'(BN,8X,8I8)') JE(:)
C
            DO I = 1, 8
              IF( JE(I) == 0 ) EXIT
              IP = IP + 1
              J_TMP1(2:3,IP) = JE(I)
            ENDDO
C
          ENDIF
C
        ELSE
C
          IE = IP
C
          J_TMP1(1,IS:IE) = IDS
          J_TMP1(4,IS:IE) = ITYP
          R_TMP1(1,IS:IE) = T1
          R_TMP1(2,IS:IE) = T2
C
          BACKSPACE(ITI)
          RETURN
C
        ENDIF
C
      ENDDO
C
      END
