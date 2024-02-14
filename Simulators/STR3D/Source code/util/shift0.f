C***********************************************************************
      SUBROUTINE  SHIFT0( IA,IB,N )
C***********************************************************************
C  IA = IB  ---  DATA SHIFT
C
C  IA      = SHIFT AREA                                    --- ( O )
C  IB      = SHIFT DATA                                    --- ( I )
C  N       = NUMBER OF SHIFT DATA                          --- ( I )
C
C-----------------------------------------------------------------------
      DIMENSION  IA(N),IB(N)
C
C
C--- SHIFT ---
      DO 1000 I=1,N
         IA(I) = IB(I)
 1000 CONTINUE
C
C
      RETURN
      END
