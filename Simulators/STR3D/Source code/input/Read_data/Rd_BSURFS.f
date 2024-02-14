      SUBROUTINE RD_BSURFS( J_BSRF, IP, CHAR, ITI )
C
      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/
      DIMENSION IE(2), IG(3,2), J_BSRF(5,*)
C
      READ(CHAR,'(BN,8X,I8,24X,4I8)') ID, IE(1), IG(:,1)
C
      IP = IP + 1
      J_BSRF(1,IP) = ID
      J_BSRF(2,IP) = IE(1)
      J_BSRF(3:5,IP) = IG(:,1)
C
      DO
C
        READ(ITI,'(A80)') CHAR
C
        IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
          READ(CHAR,'(BN,8X,8I8)') ( IE(I), IG(:,I), I = 1, 2 )
          DO I = 1, 2
            IF( IE(I) == 0 ) RETURN
            IP = IP + 1
            J_BSRF(1,IP) = ID
            J_BSRF(2,IP) = IE(I)
            J_BSRF(3:5,IP) = IG(:,I)
          ENDDO
        ELSE
          BACKSPACE(ITI)
          RETURN
        ENDIF
C
      ENDDO
C
      END
