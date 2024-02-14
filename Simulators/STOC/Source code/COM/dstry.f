      SUBROUTINE DSTRY(XC,YC,ZC,
     &                 HDEP,HH,KF,KG,KP,KH,GV,GX,GY,GZ,
     $                 GV0,GX0,GY0,GZ0,GVD,GXD,GYD,GZD,FF,AMNG,
     &                 INDP,INDU,INDV,INDW,
     &                 HTDST1,HTDST2,IDST,TMDST,
     &                 LLWALL)
C======================================================================
C     破壊処理に関するメインルーチン
C
C
C      HTDST1(I,J)   :  破壊時における修正地形の標高 [m]
C      HTDST2(I,J)   :  破壊判定時における地盤の標高 [m]
C      IDST(I,J)     :  破壊処理におけるフラグ
C                     = -92  :  破壊の最中(漂流物による破壊)
C                     = -91  :  破壊の最中(流体による破壊)
C                     = -12  :  破壊後(漂流物による破壊)
C                     = -11  :  破壊後(流体による破壊)
C                     =  -1  :  非木造(非破壊)
C                     =   0  :  建物なし
C                     =  +1  :  木造(破壊の可能性あり)
C                     =  他の正の整数値を導入することで
C                        木造(破壊の可能性あり)に他の破壊基準を採用可能
C            (漂流物による破壊は実行済み:既にIDST(I,J)=-92となっている)
C
C
C
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'DESTROY.h'
C
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
C
      REAL(8),INTENT(INOUT)::
     $               GV(MX,MY,MZ),GX(MX,MY,MZ),GY(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::
     $           GV0(MX,MY,MZ),GX0(MX,MY,MZ),GY0(MX,MY,MZ),GZ0(MX,MY,MZ)
      REAL(8),INTENT(IN)::
     $           GVD(MX,MY,MZ),GXD(MX,MY,MZ),GYD(MX,MY,MZ),GZD(MX,MY,MZ)
C
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY)
      REAL(8),INTENT(INOUT)::HDEP(MX,MY),AMNG(MX,MY)
C
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KG(MX,MY),KP(MX,MY),KH(MX,MY)
C
      REAL(8),INTENT(INOUT)::HTDST1(MX,MY),HTDST2(MX,MY)
      INTEGER,INTENT(INOUT)::IDST(MX,MY)
      REAL(8),INTENT(INOUT)::TMDST(MX,MY)
C
      INTEGER,INTENT(INOUT)::LLWALL(8,MLWALL)
C
C
      REAL(8)::HTWK
      REAL(8)::AWK1,AWK2,AWK3
C
      INTEGER::I,J,K
      INTEGER::NN,IDIR
C
C----------------------------------------------------------------------
C     (1) 破壊判定および地形の修正・水位の設定
C----------------------------------------------------------------------
C
      DO 200 I=2+NBFRSZ,MXM-NBFRSZ
      DO 200 J=2+NBFRSZ,MYM-NBFRSZ
C
C-------------------------------------
C     (1a) 浸水深による破壊判定
C        (漂流物による破壊は実行済み:既にIDST(I,J)=-92となっている場合)
C-------------------------------------
         IF (IDST(I,J).LT.-90) GO TO 210
         IF (IDST(I,J).GE.1) THEN
            NN = IDST(I,J)
C
            HTWK = MAX(HDEP(I-1,J  ),HTDST2(I,J))
            IF(HH(I-1,J  )-HTWK.GT.DSTLMT(NN)) THEN
                  IDST(I,J) = -91
                  GO TO 210
            END IF
            HTWK = MAX(HDEP(I+1,J  ),HTDST2(I,J))
            IF(HH(I+1,J  )-HTWK.GT.DSTLMT(NN)) THEN
                  IDST(I,J) = -91
                  GO TO 210
            END IF
            HTWK = MAX(HDEP(I  ,J-1),HTDST2(I,J))
            IF(HH(I  ,J-1)-HTWK.GT.DSTLMT(NN)) THEN
                  IDST(I,J) = -91
                  GO TO 210
            END IF
            HTWK = MAX(HDEP(I  ,J+1),HTDST2(I,J))
            IF(HH(I  ,J+1)-HTWK.GT.DSTLMT(NN)) THEN
                  IDST(I,J) = -91
                  GO TO 210
            END IF
C
         END IF
C
         GO TO 200
C
C-------------------------------------
C     (1b) 建物破壊による地形の修正・水位の設定
C-------------------------------------
  210    HDEP(I,J) = HTDST1(I,J)
         HH(I,J) = HDEP(I,J) + EPSH
         AMNG(I,J) = AMNGDST
C
C-------------------------------------
C     (1c) 破壊時刻の出力
C-------------------------------------
         TMDST(I,J)  = TIME
C
  200 CONTINUE
C
C
C----------------------------------------------------------------------
C     (2) 水位・地盤レベルおよび体積多孔率等の設定
C----------------------------------------------------------------------
      DO 300 I=2+NBFRSZ,MXM-NBFRSZ
      DO 300 J=2+NBFRSZ,MYM-NBFRSZ
         IF (IDST(I,J).GT.-90) GO TO 300
C
C--------------------------
C--------  KG   -----------
C--------------------------
         DO 320 K=2,MZM
            IF (ZC(1,K).GT.HDEP(I,J)) THEN
               KG(I,J) = K
               GO TO 325
            END IF
  320    CONTINUE
         KG(I,J) = MZ
C--------------------------
C------  KF,KP,KH  --------
C--------------------------
  325    DO 330 K=KG(I,J),MZM
            IF (ZC(1,K).GT.HH(I,J)) THEN
               KF(I,J) = K
               KP(I,J) = K
               KH(I,J) = K
               GO TO 335
            END IF
  330    CONTINUE
         KF(I,J) = MZ
         KP(I,J) = MZ
         KH(I,J) = MZ
C
C
  335    DO 340 K=2,MZM
C--------------------------
C------  GV,INDP  ---------
C--------------------------
            AWK1 = (ZC(1,K) - HDEP(I,J)) * ZC(6,K)
            AWK1 = MIN(1.0D0,AWK1)
            IF (AWK1.GT.0.0D0) THEN
            INDP(I  ,J  ,K) = 1
              GV0(I  ,J  ,K) = AWK1
              GV(I,J,K) = GV0(I,J,K)*GVD(I,J,K)
            ELSE
              INDP(I  ,J  ,K) = 0
                GV0(I  ,J  ,K) = 1.0D0
                GV(I,J,K) = GV0(I,J,K)*GVD(I,J,K)
            END IF
C--------------------------
C--------  FF   -----------
C--------------------------
            IF (K.LT.KF(I,J)) THEN
               FF(I,J,K) = 1.0
            ELSE IF (K.EQ.KF(I,J)) THEN
               FF(I,J,K) = (HH(I,J) - ZC(1,K-1)) * ZC(6,K)
            ELSE
               FF(I,J,K) = 0.0
            END IF
  340    CONTINUE
C
  300 CONTINUE
C
C----------------------------------------------------------------------
C     (3) 面透過率等の設定
C----------------------------------------------------------------------
      DO 400 I=2+NBFRSZ,MXM-NBFRSZ
      DO 400 J=2+NBFRSZ,MYM-NBFRSZ
         IF (IDST(I,J).GT.-90) GO TO 400
         IDST(I,J) = IDST(I,J) + 80  !!! IDST=-9? ==> IDST=-1? に変更
C
         DO 410 K=2,MZM
            AWK1 = GV0(I,J,K)
C--------------------------
C--------  GX-  -----------
C--------------------------
            IF (INDP(I,J,K).EQ.1 .AND. INDP(I-1,J,K).EQ.1) THEN
               AWK2 = (ZC(1,K) - HDEP(I-1,J)) * ZC(6,K)
               AWK2 = MIN(1.0D0,AWK2)
               AWK3 = MIN(AWK1,AWK2)
               INDU(I-1,J,K) = 1
               GX0(I-1,J,K) = AWK3
               GX(I-1,J,K) = GX0(I-1,J,K)*GXD(I-1,J,K)
            ELSE IF (INDP(I,J,K).EQ.1 .OR. INDP(I-1,J,K).EQ.1) THEN
               INDU(I-1,J,K) = -222  !!! (4)で INDU = -2 に変更
               GX0(I-1,J,K) = 1.0D0
               GX(I-1,J,K) = GX0(I-1,J,K)*GXD(I-1,J,K)
            ELSE
               INDU(I-1,J,K) = -4
               GX0(I-1,J,K) = 1.0D0
               GX(I-1,J,K) = GX0(I-1,J,K)*GXD(I-1,J,K)
            END IF
C--------------------------
C--------  GX+  -----------
C--------------------------
            IF (INDP(I,J,K).EQ.1 .AND. INDP(I+1,J,K).EQ.1) THEN
               AWK2 = (ZC(1,K) - HDEP(I+1,J)) * ZC(6,K)
               AWK2 = MIN(1.0D0,AWK2)
               AWK3 = MIN(AWK1,AWK2)
               INDU(I,J,K) = 1
               GX0(I,J,K) = AWK3
               GX(I,J,K) = GX0(I,J,K)*GXD(I,J,K)
            ELSE IF (INDP(I,J,K).EQ.1 .OR. INDP(I+1,J,K).EQ.1) THEN
               INDU(I,J,K) = -222  !!! (4)で INDU = -2 に変更
               GX0(I,J,K) = 1.0D0
               GX(I,J,K) = GX0(I,J,K)*GXD(I,J,K)
            ELSE
               INDU(I,J,K) = -4
               GX0(I,J,K) = 1.0D0
               GX(I,J,K) = GX0(I,J,K)*GXD(I,J,K)
            END IF
C--------------------------
C--------  GY-  -----------
C--------------------------
            IF (INDP(I,J,K).EQ.1 .AND. INDP(I,J-1,K).EQ.1) THEN
               AWK2 = (ZC(1,K) - HDEP(I,J-1)) * ZC(6,K)
               AWK2 = MIN(1.0D0,AWK2)
               AWK3 = MIN(AWK1,AWK2)
               INDV(I,J-1,K) = 1
               GY0(I,J-1,K) = AWK3
               GY(I,J-1,K) = GY0(I,J-1,K)*GYD(I,J-1,K)
            ELSE IF (INDP(I,J,K).EQ.1 .OR. INDP(I,J-1,K).EQ.1) THEN
               INDV(I,J-1,K) = -222  !!! (4)で INDV = -2 に変更
               GY0(I,J-1,K) = 1.0D0
               GY(I,J-1,K) = GY0(I,J-1,K)*GYD(I,J-1,K)
            ELSE
               INDV(I,J-1,K) = -4
               GY0(I,J-1,K) = 1.0D0
               GY(I,J-1,K) = GY0(I,J-1,K)*GYD(I,J-1,K)
            END IF
C--------------------------
C--------  GY+  -----------
C--------------------------
            IF (INDP(I,J,K).EQ.1 .AND. INDP(I,J+1,K).EQ.1) THEN
               AWK2 = (ZC(1,K) - HDEP(I,J+1)) * ZC(6,K)
               AWK2 = MIN(1.0D0,AWK2)
               AWK3 = MIN(AWK1,AWK2)
               INDV(I,J,K) = 1
               GY0(I,J,K) = AWK3
               GY(I,J,K) = GY0(I,J,K)*GYD(I,J,K)
            ELSE IF (INDP(I,J,K).EQ.1 .OR. INDP(I,J+1,K).EQ.1) THEN
               INDV(I,J,K) = -222  !!! (4)で INDV = -2 に変更
               GY0(I,J,K) = 1.0D0
               GY(I,J,K) = GY0(I,J,K)*GYD(I,J,K)
            ELSE
               INDV(I,J,K) = -4
               GY0(I,J,K) = 1.0D0
               GY(I,J,K) = GY0(I,J,K)*GYD(I,J,K)
            END IF
C
C
  410    CONTINUE
C
C--------------------------
C--------  GZ+  -----------
C--------------------------
         DO 420 K=1,MZM
            IF (INDP(I,J,K).EQ.1 .AND. INDP(I,J,K+1).EQ.1) THEN
               INDW(I,J,K) = 1
               GZ0(I,J,K) = 1.0D0
               GZ(I,J,K) = GZ0(I,J,K)*GZD(I,J,K)
            ELSE IF (INDP(I,J,K).EQ.1 .OR. INDP(I,J,K+1).EQ.1) THEN
               INDW(I,J,K) = -222  !!! (4)で INDW = -2 に変更
               GZ0(I,J,K) = 1.0D0
               GZ(I,J,K) = GZ0(I,J,K)*GZD(I,J,K)
            ELSE
               INDW(I,J,K) = -4
               GZ0(I,J,K) = 1.0D0
               GZ(I,J,K) = GZ0(I,J,K)*GZD(I,J,K)
            END IF
  420    CONTINUE
C
C
  400 CONTINUE
C
C----------------------------------------------------------------------
C     (4) 破壊に伴う MLWALL および LLWALL の追加
C----------------------------------------------------------------------
      NN = MLWALL1
C
      DO 500 K=2,MZM
      DO 500 I=2+NBFRSZ-1,MXM-NBFRSZ
      DO 500 J=2+NBFRSZ-1,MYM-NBFRSZ
         IF (INDU(I,J,K).NE.-222) GO TO 500
         INDU(I,J,K) = -2
         NN = NN + 1
         IF (INDP(I,J,K).EQ.0) THEN
            IDIR = 1
         ELSE
            IDIR = 0
         END IF
         LLWALL(1,NN) = I
         LLWALL(2,NN) = J
         LLWALL(3,NN) = K
         LLWALL(4,NN) = IDIR
         LLWALL(5,NN) = 0
         LLWALL(6,NN) = 1 ! NO-SLIP
         LLWALL(7,NN) = 0 ! ADIABATIC
         LLWALL(8,NN) = 0
  500 CONTINUE
C
      DO 510 K=2,MZM
      DO 510 I=2+NBFRSZ-1,MXM-NBFRSZ
      DO 510 J=2+NBFRSZ-1,MYM-NBFRSZ
         IF (INDV(I,J,K).NE.-222) GO TO 510
         INDV(I,J,K) = -2
         NN = NN + 1
         IF (INDP(I,J,K).EQ.0) THEN
            IDIR = 3
         ELSE
            IDIR = 2
         END IF
         LLWALL(1,NN) = I
         LLWALL(2,NN) = J
         LLWALL(3,NN) = K
         LLWALL(4,NN) = IDIR
         LLWALL(5,NN) = 0
         LLWALL(6,NN) = 1 ! NO-SLIP
         LLWALL(7,NN) = 0 ! ADIABATIC
         LLWALL(8,NN) = 0
  510 CONTINUE
C
      DO 520 K=1,MZM
      DO 520 I=2+NBFRSZ-1,MXM-NBFRSZ
      DO 520 J=2+NBFRSZ-1,MYM-NBFRSZ
         IF (INDW(I,J,K).NE.-222) GO TO 520
         INDW(I,J,K) = -2
         NN = NN + 1
         IF (INDP(I,J,K).EQ.0) THEN
            IDIR = 5
         ELSE
            IDIR = 4
         END IF
         LLWALL(1,NN) = I
         LLWALL(2,NN) = J
         LLWALL(3,NN) = K
         LLWALL(4,NN) = IDIR
         LLWALL(5,NN) = 0
         LLWALL(6,NN) = 1 ! NO-SLIP
         LLWALL(7,NN) = 0 ! ADIABATIC
         LLWALL(8,NN) = 0
  520 CONTINUE
C
C
C
      IF (MLWALL1.NE.NN) THEN
C
      WRITE (*,'(A26,F10.3)')      ' ## DESTROY ## ==> TIME = ',TIME
      WRITE (*,'(A16,I10,A5,I10)') 'MLWALL1 CHANGE: ',MLWALL1,' ==> ',NN
C
      MLWALL1 = NN
      END IF
C
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      RETURN
      END
