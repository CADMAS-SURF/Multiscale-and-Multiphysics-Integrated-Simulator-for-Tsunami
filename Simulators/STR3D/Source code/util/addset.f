      SUBROUTINE ADDSET(I,INDEX,IS,NDATA)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION INDEX(2,*)
C---------------------------------------------------------------------==
C        1         2         3         4         5         6         7**
C23456789+123456789+123456789+123456789+123456789+123456789+123456789+12
C---------------------------------------------------------------------==
C
      IF(I.EQ.1) THEN
        IE=0
      ELSE
        IE=INDEX(2,I-1)
      ENDIF
      NDATA=INDEX(2,I)-IE
      IS=IE+1
C
      RETURN
      END
