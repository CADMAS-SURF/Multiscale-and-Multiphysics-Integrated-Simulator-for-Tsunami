C***********************************************************************
      SUBROUTINE  CLEAR0( IT,N )
C***********************************************************************
C  0 CLEAR OF INTEGER TABLE
C
C  IT     = CLEAR TABLE                                   --- ( O )
C  N      = NUMBER OF IT TABLE                            --- ( I )
C
C-----------------------------------------------------------------------
      DIMENSION  IT(N)
C
C
      IF( N .LE. 0 )  RETURN
C
C--- 0 CLEAR ---
      DO 1000 I=1,N
         IT(I) = 0
 1000 CONTINUE
C
C
      RETURN
      END
