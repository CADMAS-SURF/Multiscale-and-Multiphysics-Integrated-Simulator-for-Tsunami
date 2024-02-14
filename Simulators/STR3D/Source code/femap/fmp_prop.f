      SUBROUTINE FMP_PROP(ID,ITYP,IFL)

      CHARACTER*200 CHAR

      WRITE(CHAR,'(6I4)') ID,124,ID,ITYP,1,0

      CALL WT_DATA(CHAR,IFL)

      WRITE(IFL,'(I0)') ID

      WRITE(IFL,'(A)') '0,0,0,0,'

      WRITE(IFL,'(A)') '1,'

      WRITE(IFL,'(A)') '0,'

      WRITE(IFL,'(A)') '1,'

      WRITE(IFL,'(A)') '0.,'

      WRITE(IFL,'(A)') '0,'

      WRITE(IFL,'(A)') '0,'

      END
