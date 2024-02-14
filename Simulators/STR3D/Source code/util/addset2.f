      SUBROUTINE ADDSET2(I,INDEX,IS,NDATA)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION INDEX(*)
C----&------------------------------------------------------------------
      IF(I.EQ.1) THEN
        IE=0
      ELSE
        IE=INDEX(I-1)
      ENDIF
      NDATA=INDEX(I)-IE
      IS=IE+1
C
      RETURN
      END
