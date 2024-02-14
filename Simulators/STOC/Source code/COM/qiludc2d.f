      SUBROUTINE QILUDC2D(AD,AL,AU,DD,INDP)
C======================================================================
C     不完全LU分解を行う
C======================================================================
      IMPLICIT NONE
C
C ... MODE=0 NOT MODIFIED ILU DECOMPOSITION
C     MODE=1 MODIFIED-ILU DECOMPOSITION
C
      INTEGER,PARAMETER::MODE=1
C
      INCLUDE 'DOMAIN.h'
C
      REAL(8),INTENT(IN)::AD(MX,MY),AL(2,MX,MY)
      REAL(8),INTENT(IN)::AU(2,MX,MY)
      REAL(8),INTENT(OUT)::DD(MX,MY)
      INTEGER,INTENT(IN)::INDP(MX,MY)
C
      REAL(8)::PARM(6)
      DATA PARM /0.95D0,0.9D0,0.75D0,0.50D0,0.3D0,0.0D0/
C
      REAL(8)::AU1,AU2,DD1,U
      INTEGER::I,J,L,MXLOOP,NPOS
C
C
      CALL ZERCLR(DD,MXY,0.0D0)
C
      IF( MODE.EQ.0 ) THEN
         MXLOOP=1
      ELSE
         MXLOOP=6
      END IF
C
      DO 100 L=1,MXLOOP
         IF( MODE.EQ.1 ) THEN
            U = PARM(L)
         ELSE
            U = 0.0D0
         END IF
C
         NPOS=0
         DO 110 J=2,MYM
         DO 110 I=2,MXM
            IF( INDP(I,J).GT.0 ) THEN
c               AU1 = AU(1,I-1,J)+U*(AU(2,I-1,J)+AU(3,I-1,J))
c               AU2 = AU(2,I,J-1)+U*(AU(1,I,J-1)+AU(3,I,J-1))
               AU1 = AU(1,I-1,J)+U*AU(2,I-1,J)
               AU2 = AU(2,I,J-1)+U*AU(1,I,J-1)
               DD1 = AD(I,J) - AL(1,I,J)*DD(I-1,J)*AU1
     $                         - AL(2,I,J)*DD(I,J-1)*AU2
               DD(I,J)=1.0D0 / DD1
               IF( MODE.EQ.1 .AND. DD(I,J).LE.0.0D0 ) NPOS=1
               IF( ABS(DD1).LT.1.0D-20 ) DD(I,J) = 1.0D0/AD(I,J)
            END IF
  110    CONTINUE
C
         IF(NPOS.EQ.0) GO TO 120
  100 CONTINUE
  120 CONTINUE
C
      RETURN
      END
