      SUBROUTINE BCSURF(PP,UU,VV,WW,HH,TT,QQ,QW,PATM,DPS,WX,WY,CD,
     $                  XC,YC,ZC,YCOSP,GV,GX,GY,GZ,HDEP,RWWB,RWWF,
     $                  INDU,INDV,INDP,KF,KP,KG,KH,IFL)
C======================================================================
C     水面に関するPP,UU,VV,WWの境界条件を設定する
C
C     PP: 圧力(Pa)、このうち圧力境界セルの圧力値を更新する
C     KP: 圧力境界条件が適用されはじめるセルのz方向セルインデックス
C======================================================================
C
C====================================================FOR WIND DATA START
      USE MOD_WIND
C====================================================FOR WIND DATA END
      IMPLICIT NONE
C
      INCLUDE 'BOUNDI.h'
      INCLUDE 'BOUNDR.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'TABLER.h'
      INCLUDE 'FILE.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'OIL.h'
C
      REAL(8),INTENT(INOUT)::PP(MX,MY,MZ),TT(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
C
      REAL(8),INTENT(INOUT)::HH(MX,MY),QQ(MX,MY,MZ),QW(MX,MY)
      REAL(8),INTENT(INOUT)::PATM(MX,MY),DPS(MX,MY)
      REAL(8),INTENT(INOUT)::WX(MX,MY),WY(MX,MY),CD(MX,MY)
      REAL(8),INTENT(INOUT)::RWWB(MX,MY,9),RWWF(MX,MY,9)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::YCOSP(MY)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GY(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HDEP(MX,MY)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KP(MX,MY),KG(MX,MY),KH(MX,MY)
      INTEGER,INTENT(INOUT)::IFL
C.... (局所配列)
      REAL(8)::WU(MX,MY),WV(MX,MY),WP(MX,MY)
C
      REAL(8)::PP1,UU1,VV1
      INTEGER::I,II,IXX,J,K
C====================================================FOR WIND DATA START
      LOGICAL,SAVE :: IWIND_SET = .TRUE.
      INTEGER :: IERR
C====================================================FOR WIND DATA END
C
C
C----------------------------------------------------------------------
C     (1) 水面境界値データ PATM, WX, WY を設定する
C----------------------------------------------------------------------
      DO 500 J=1,MY
      DO 500 I=1,MX
        DPS(I,J) = PATM(I,J)
 500  CONTINUE
C
C ... 台風モデルを使用する場合
      IF(LTYPH.EQ.1) THEN
        CALL WINCL3(WX,WY,PATM,WU,WV,WP,XC,YC)
        OIL_WIN=1
C
C ... ユーザー関数で設定する場合
      ELSE IF( ISURF(1).EQ.-1 ) THEN
        CALL UBCSRF(PATM,WX,WY,CD,QQ,QW,RWWB,RWWF)
        OIL_WIN=1
C
C ... 関数で設定する場合
      ELSE IF( ISURF(1).EQ.-2 ) THEN
        CALL UBCSR2(PATM,WX,WY,QQ,QW,TT,ZC,HH,HDEP,RWWB,RWWF,
     $              INDP,KH,KF,KG)
        OIL_WIN=1
C
C ... 一定値または時系列値を設定する場合
      ELSE
         IF( ISURF(1).EQ.0 ) THEN
            PP1 = RSURF(1)
         ELSE
            PP1 = TABLE(ISURF(1))
         END IF
         IF( ISURF(2).EQ.0 ) THEN
            UU1 = RSURF(2)
            IF( UU1.NE.0.D0 ) OIL_WIN=1
         ELSE
            UU1 = TABLE(ISURF(2))
            OIL_WIN=1
         END IF
         IF( ISURF(3).EQ.0 ) THEN
            VV1 = RSURF(3)
            IF( VV1.NE.0.D0 ) OIL_WIN=1
         ELSE
            VV1 = TABLE(ISURF(3))
            OIL_WIN=1
         END IF
C         PPP = 1.0D4
         DO 510 J=2,MYM
         DO 510 I=2,MXM
            PP1 = 0.0D0
            WX(I,J) = UU1
            WY(I,J) = VV1
            CD(I,J) = GM2S
C            IF(I.GE.8.AND.I.LE.9) PP1=PPP*MIN(1.0D0,DT*ISTEP*10)
C            IF(I.GE.10.AND.I.LE.11) PP1=-PPP*MIN(1.0D0,DT*ISTEP*10)
  510    CONTINUE
      END IF
C
C====================================================FOR WIND DATA START
      IF( NP_OIL.GE.0 ) THEN
      IF(IWIND_SET) THEN
        IWIND_SET = .FALSE.
        CALL WIND_INIT(1,IERR)
      END IF
      CALL WIND_TIME(TIME)
      CALL WIND_INTERP(WX,WY,XC,YC,MX,MY,1)
      OIL_WIN=1
      ENDIF
C====================================================FOR WIND DATA END
C
C ... 表面圧力の時間変化量
      DO 520 J=2,MYM
      DO 520 I=2,MXM
        DPS(I,J) = PATM(I,J)-DPS(I,J)
 520  CONTINUE
C
      IF(IFL.EQ.0) GO TO 530
C----------------------------------------------------------------------
C     (2) 圧力の水面境界条件を設定
C----------------------------------------------------------------------
      DO 110 J=2,MYM
      DO 110 I=2,MXM
         KP(I,J) = KF(I,J)
  110 CONTINUE
C
      DO 100 K=2,MZM
      DO 100 J=2,MYM
      DO 100 I=2,MXM
C ...... 水面のある領域のみ計算
         IF( INDP(I,J,K).GT.0 .AND. K.LT.KF(I,J) ) THEN
C ............ 隣り合ったセルの水位が2メッシュ以上異なる場合
               IF( ( INDU(I-1,J,K).GT.0 .AND. K.GT.KF(I-1,J) ) .OR.
     $             ( INDU(I  ,J,K).GT.0 .AND. K.GT.KF(I+1,J) ) .OR.
     $             ( INDV(I,J-1,K).GT.0 .AND. K.GT.KF(I,J-1) ) .OR.
     $             ( INDV(I,J  ,K).GT.0 .AND. K.GT.KF(I,J+1) ) ) THEN
                   KP(I,J) = K
               END IF
            END IF
  100 CONTINUE
C
C
C FOR WAVE AND ORIGINAL
      IXX = 0
C FOR JET
C     IXX = 1
C
      IF(IXX.EQ.0) THEN
C
      DO 120 J=2,MYM
      DO 120 I=2,MXM
         IF( KF(I,J).LT.MZ ) THEN
            K = KF(I,J)
            IF( INDP(I,J,K-1).GT.0 ) THEN
C ......... 表面セルの圧力を気圧と下部セルの圧力から内外挿する
               PP(I,J,K) = ( PATM(I,J)*ZC(3,K-1)
     $                   +   PP(I,J,K-1)*(HH(I,J)-ZC(2,K)) )
     $                   / ( HH(I,J)-ZC(2,K-1) )
            ELSE IF( INDP(I,J,K-1).EQ.0 ) THEN
C ......... 表面セルの圧力を気圧と静水圧で設定する
               PP(I,J,K) = PATM(I,J)-(HH(I,J)-ZC(2,K))*RHO*GRAV
            END IF
         END IF
  120 CONTINUE
C
      END IF
C
C
C----------------------------------------------------------------------
C     (3) 流速の水面境界条件を設定
C----------------------------------------------------------------------
C
C// 2005.02.19 ( BCSUR2をCALLするように変更)
C
CDEBUG      CALL BCSUR2(UU,VV,WW,GV,GX,GY,GZ,HH,KF,KP,KG,KH,
CDEBUG     $            INDU,INDV,INDP,XC,YC,ZC,YCOSP,1)
C
  530 CONTINUE
      RETURN
      END
