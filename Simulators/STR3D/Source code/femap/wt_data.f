      SUBROUTINE WT_DATA(CHAR1,IFL)

      CHARACTER*200 CHAR1,CHAR2
      CHARACTER*1 C /','/

      IC = 1
      IP = 0

      DO I = 1, 200
        IF( CHAR1(I:I) == ' ' ) THEN
          IF( IC == 0 ) THEN
            IP = IP + 1
            CHAR2(IP:IP) = C
            IC = 1
          ELSE
            CYCLE
          ENDIF
        ELSE
          IP = IP + 1
          CHAR2(IP:IP) = CHAR1(I:I)
          IC = 0
        ENDIF
      ENDDO

      WRITE(IFL,'(A)') CHAR2(1:IP)

      END
