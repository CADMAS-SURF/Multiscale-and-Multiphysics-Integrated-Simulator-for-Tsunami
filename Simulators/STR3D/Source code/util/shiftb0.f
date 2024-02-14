      SUBROUTINE SHIFTB0(IA,IS,IE,N)
C
      DIMENSION IA(*)
C-----------------------------------------------------------------------
      DO I=IE,IS,-1
        IA(I+N)=IA(I)
      ENDDO
C
      RETURN
      END
