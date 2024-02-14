      SUBROUTINE RUNUPY(VP,FF,HH,HDEP,GV,GX,GY,XC,YC,ZC,
     $                  INDV,LLWALB,KF,KG)
C======================================================================
C     遡上・防潮堤による流量制限を行う(Y方向)
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'VVMAX.h'
C
      REAL(8),INTENT(INOUT)::VP(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ),HH(MX,MY),HDEP(MX,MY)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ),GY(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
C
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),LLWALB(3,MLWALB)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KG(MX,MY)
C
      REAL(8)::DH1,DH2,GY1
      INTEGER::I,J,K,N
C
C
C ... 遡上モデル
      DO 100 J=1,MYM
      DO 100 I=2,MXM
C
       K = MAX(KG(I,J),KG(I,J+1))
       IF( INDV(I,J,K).GT.0 ) THEN
C
         DH1 = HH(I,J  )-HDEP(I,J  )
         DH2 = HH(I,J+1)-HDEP(I,J+1)
         IF( DH1.LE.GXB .AND. DH2.LE.GXB ) THEN
            VP(I,J,K) = 0.0D0
         ELSE IF( K.EQ.KG(I,J) .AND.
     $            K.EQ.KF(I,J) .AND.
     $            DH1.LE.GXB   .AND.
     $            HDEP(I,J).GE.HH(I,J+1) .AND.
     $            HH(I,J+1).GT.HDEP(I,J+1)+GXB ) THEN
            VP(I,J,K) = 0.0D0
         ELSE IF( K.EQ.KG(I,J+1) .AND.
     $            K.EQ.KF(I,J+1) .AND.
     $            DH2.LE.GXB     .AND.
     $            HDEP(I,J+1).GE.HH(I,J) .AND.
     $            HH(I,J) .GT. HDEP(I,J)+GXB ) THEN
            VP(I,J,K) = 0.0D0
         ELSE IF( DH1.LE.GLH .AND. DH2.LE.GLH ) THEN
            IF(ABS(VP(I,J,K)).GT.VVMAX) VP(I,J,K)=SIGN(VVMAX,VP(I,J,K))
         END IF
C
       END IF
  100 CONTINUE
C
C
C ... 水位が防潮堤よりも低い場合は流速0とする
!CDIR NODEP
      DO 110 N=MLWALBX+1,MLWALB
         I = LLWALB(1,N)
         J = LLWALB(2,N)
         K = LLWALB(3,N)
C
         GY1 = 1.0D0-GY(I,J,K)
         IF( FF(I,J,K).LT.GY1 .AND. FF(I,J+1,K).LT.GY1 )
     $      VP(I,J,K) = 0.0D0
  110 CONTINUE
C
      RETURN
      END
