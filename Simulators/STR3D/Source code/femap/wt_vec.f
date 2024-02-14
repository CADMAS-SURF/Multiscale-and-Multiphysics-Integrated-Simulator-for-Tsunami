      SUBROUTINE WT_VEC(IDX,IDY,IDZ,ITYP,ID,X,IDF,NDF,N,IFL)

      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*1 C /','/
      DIMENSION ID(N),X(NDF,N)

      WRITE(IFL,'(A)') '1.,-1.,0.,'

      WRITE(IFL,'(10(I0,A))') IDX,C,IDY,C,IDZ,C,(0,C,I=1,17)

      WRITE(IFL,'(4(I0,A))') 0,C,0,C,0,C,ITYP,C

      WRITE(IFL,'(A)') '0,1,1,'

      DO I = 1, N
        WRITE(IFL,'(I0,A,1PE11.4,A)') ID(I),C,X(IDF,I),C
      ENDDO

      WRITE(IFL,'(A)') '-1,0.,'

      END
