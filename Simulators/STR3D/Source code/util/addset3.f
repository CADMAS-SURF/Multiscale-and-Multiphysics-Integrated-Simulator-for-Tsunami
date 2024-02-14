      SUBROUTINE ADDSET3(IS,IE,INDEX,I)

      DIMENSION INDEX(2,*)

      IF( I == 1 ) THEN
        IS = 1
      ELSE
        IS = INDEX(2,I-1) + 1
      ENDIF

      IE = INDEX(2,I)

      END