      SUBROUTINE RUNUPX(UP,FF,HH,HDEP,GV,GX,GY,XC,YC,ZC,
     $                  INDU,LLWALB,KF,KG)
C======================================================================
C     遡上・防潮堤による流量制限を行う(X方向)
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'VVMAX.h'
C
      REAL(8),INTENT(INOUT)::UP(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ),HH(MX,MY),HDEP(MX,MY)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ),GY(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
C
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),LLWALB(3,MLWALB)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KG(MX,MY)
C
      REAL(8)::DH1,DH2,GX1
      INTEGER::I,J,K,N
C
C
C ... 遡上モデル
      DO 100 J=2,MYM
      DO 100 I=1,MXM
C
       K = MAX(KG(I,J),KG(I+1,J))
       IF( INDU(I,J,K).GT.0 ) THEN
C
         DH1 = HH(I  ,J)-HDEP(I  ,J)
         DH2 = HH(I+1,J)-HDEP(I+1,J)
         IF( DH1.LE.GXB .AND. DH2.LE.GXB ) THEN
            UP(I,J,K) = 0.0D0
         ELSE IF( K.EQ.KG(I,J) .AND.
     $            K.EQ.KF(I,J) .AND.
     $            DH1.LE.GXB   .AND.
     $            HDEP(I,J).GE.HH(I+1,J) .AND.
     $            HH(I+1,J).GT.HDEP(I+1,J)+GXB ) THEN
            UP(I,J,K) = 0.0D0
         ELSE IF( K.EQ.KG(I+1,J) .AND.
     $            K.EQ.KF(I+1,J) .AND.
     $            DH2.LE.GXB     .AND.
     $            HDEP(I+1,J).GE.HH(I,J) .AND.
     $            HH(I,J) .GT. HDEP(I,J)+GXB ) THEN
            UP(I,J,K) = 0.0D0
         ELSE IF( DH1.LE.GLH .AND. DH2.LE.GLH ) THEN
            IF(ABS(UP(I,J,K)).GT.VVMAX) UP(I,J,K)=SIGN(VVMAX,UP(I,J,K))
         END IF
C
       END IF
  100 CONTINUE
C
C
C ... 水位が防潮堤よりも低い場合は流速0とする
!CDIR NODEP
      DO 110 N=1,MLWALBX
         I = LLWALB(1,N)
         J = LLWALB(2,N)
         K = LLWALB(3,N)
C
         GX1 = 1.0D0-GX(I,J,K)
         IF( FF(I,J,K).LT.GX1 .AND. FF(I+1,J,K).LT.GX1 )
     $      UP(I,J,K) = 0.0D0
  110 CONTINUE
C
      RETURN
      END
