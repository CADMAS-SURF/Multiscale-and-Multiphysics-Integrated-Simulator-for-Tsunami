      SUBROUTINE QILUDC(AD,AL,AU,DD,INDP,MZ0)
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
      REAL(8),INTENT(INOUT)::AD(MX,MY,MZ0),AL(3,MX,MY,MZ0)
      REAL(8),INTENT(INOUT)::AU(3,MX,MY,MZ0),DD(MX,MY,MZ0)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ0)
      INTEGER,INTENT(IN)::MZ0
C
      REAL(8)::PARM(6)
      DATA PARM /0.95D0,0.9D0,0.75D0,0.50D0,0.3D0,0.0D0/
C
      REAL(8)::AU1,AU2,AU3,DD1,U
      INTEGER::I,J,K,L,MXLOOP,NPOS
C
C
      CALL ZERCLR(DD,MXY*MZ0,0.0D0)
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
         DO 110 K=2,MZ0-1
         DO 110 J=2,MYM
         DO 110 I=2,MXM
            IF( INDP(I,J,K).GT.0 ) THEN
               AU1 = AU(1,I-1,J,K)+U*(AU(2,I-1,J,K)+AU(3,I-1,J,K))
               AU2 = AU(2,I,J-1,K)+U*(AU(1,I,J-1,K)+AU(3,I,J-1,K))
               AU3 = AU(3,I,J,K-1)+U*(AU(1,I,J,K-1)+AU(2,I,J,K-1))
               DD1 = AD(I,J,K) - AL(1,I,J,K)*DD(I-1,J,K)*AU1
     $                         - AL(2,I,J,K)*DD(I,J-1,K)*AU2
     $                         - AL(3,I,J,K)*DD(I,J,K-1)*AU3
               DD(I,J,K)=1.0D0 / DD1
               IF( MODE.EQ.1 .AND. DD(I,J,K).LE.0.0D0 ) NPOS=1
               IF( ABS(DD1).LT.1.0D-20 ) DD(I,J,K) = 1.0D0/AD(I,J,K)
            END IF
  110    CONTINUE
C
         IF(NPOS.EQ.0) GO TO 120
  100 CONTINUE
  120 CONTINUE
C
      RETURN
      END
