      SUBROUTINE NODSET2(KN,NODOP,NOD,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KN(10),NOD(6)
C---------------------------------------------------------------------==
C        1         2         3         4         5         6         7**
C23456789+123456789+123456789+123456789+123456789+123456789+123456789+12
C---------------------------------------------------------------------==
C
      DO 100 I=1,4
        IF(KN(I).EQ.NODOP) GOTO(201,202,203,204) I
  100 CONTINUE
C
      WRITE(ITO,*) 'STOP IN SUB. NODSET2!'
      CALL ERRSTP(90,ITO)
C
  201 NOD( 1) = KN( 2)
      NOD( 2) = KN( 4)
      NOD( 3) = KN( 3)
      NOD( 4) = KN( 9)
      NOD( 5) = KN(10)
      NOD( 6) = KN( 6)
      RETURN
C
  202 NOD( 1) = KN( 1)
      NOD( 2) = KN( 3)
      NOD( 3) = KN( 4)
      NOD( 4) = KN( 7)
      NOD( 5) = KN(10)
      NOD( 6) = KN( 8)
      RETURN
C
  203 NOD( 1) = KN( 1)
      NOD( 2) = KN( 4)
      NOD( 3) = KN( 2)
      NOD( 4) = KN( 8)
      NOD( 5) = KN( 9)
      NOD( 6) = KN( 5)
      RETURN
C
  204 NOD( 1) = KN( 1)
      NOD( 2) = KN( 2)
      NOD( 3) = KN( 3)
      NOD( 4) = KN( 5)
      NOD( 5) = KN( 6)
      NOD( 6) = KN( 7)
      RETURN
C
      END
