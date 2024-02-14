      SUBROUTINE WT_ZERO_R(N,IFL)

      CHARACTER*1 C /','/

      WRITE(IFL,'(I0,A)') N,C

      WRITE(IFL,'(10(F2.0,A))') (0.,C,I=1,N)

      END
