      SUBROUTINE WT_ZERO_I(N,IFL)

      CHARACTER*1 C /','/

      WRITE(IFL,'(I0,A)') N,C

      WRITE(IFL,'(10(I1,A))') (0,C,I=1,N)

      END
