      SUBROUTINE FMP_MAT(ID,IFL)

      CHARACTER*200 CHAR

      WRITE(CHAR,'(7I5)') ID,-601,124,0,0,1,0

      CALL WT_DATA(CHAR,IFL)

      WRITE(IFL,'(I0)') ID

      CALL WT_ZERO_I(10,IFL)

      CALL WT_ZERO_I(25,IFL)

      CALL WT_ZERO_R(200,IFL)

      CALL WT_ZERO_I(50,IFL)

      CALL WT_ZERO_I(70,IFL)

      END
