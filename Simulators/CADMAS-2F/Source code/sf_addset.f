      SUBROUTINE SF_ADDSET(IS,IE,INDEX,I)

      DIMENSION INDEX(*)

      IF( I == 1 ) THEN
        IS = 1
      ELSE
        IS = INDEX(I-1) + 1
      ENDIF

      IE = INDEX(I)

      END