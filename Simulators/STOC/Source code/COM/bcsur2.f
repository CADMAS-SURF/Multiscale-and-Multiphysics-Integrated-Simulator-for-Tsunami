      SUBROUTINE BCSUR2(UU,VV,WW,HU,HV,HW,GV,GX,GY,GZ,HH,KF,KP,KG,KH,
     $                  INDU,INDV,INDP,XC,YC,ZC,YCOS,IFL)
C======================================================================
C     水面に関するUU,VV,WWの境界条件を設定する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'FILE.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
C
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HU(MX,MY,MZ),HV(MX,MY,MZ),HW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GY(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY)
C
      INTEGER,INTENT(INOUT)::KF(MX,MY),KP(MX,MY),KG(MX,MY),KH(MX,MY)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::YCOS(MY)
      INTEGER,INTENT(INOUT)::IFL
C
      REAL(8)::DHI,DHJ,DHM,DHP,DUDZ,DVDZ,GV1,UIN,VIN,WPD
      INTEGER::I,J,K,KFMAX1,KPMAX1
      INTEGER::KFD(MX+1,MY+1)
C
      CALL FTIMER(75,0)
      CALL CP_KFEXPND(KFD,KF)
      CALL FTIMER(75,1)
C
C ... (1) Uの境界値を設定
      DO 200 J=2,MYM
C     DO 200 I=2,MXM-1
      DO 200 I=2,MXM
C ...... 水面のある領域のみ計算
         IF( KF(I,J).LT.MZ .AND. KF(I+1,J).LT.MZ ) THEN
            KPMAX1 = MAX(KP(I,J),KP(I+1,J))
            KFMAX1 = MAX(KF(I,J),KF(I+1,J))
C
C ......... 両側が圧力計算セルでない場合のUは不定となるため、
C           下側のセルからコピーする。
C
            K=KFMAX1+1
C            DO 210 K=KFMAX1+1,KFMAX1+1
              IF( INDU(I,J,K).GT.0 ) THEN
                IF( INDU(I,J,K-1).GT.0 ) THEN
                  DUDZ = (UU(I,J,K-1)-UU(I,J,K-2))*ZC(5,K-2)
                  IF( INDU(I,J,K-2).LT.0 ) DUDZ=0.0D0
                  DUDZ = 0.0D0
                  UU(I,J,K) = UU(I,J,K-1)+DUDZ*ZC(3,K-1)
                END IF
              END IF
C  210       CONTINUE
C
             K=KFMAX1+2
C            DO 220 K=KFMAX1+2,MZM
             IF( K.LE.MZ )  UU(I,J,K) = 0.0D0
C  220       CONTINUE
         END IF
  200 CONTINUE
C
C ... (2) Vの境界値を設定
C     DO 300 J=2,MYM-1
      DO 300 J=2,MYM
      DO 300 I=2,MXM
C ...... 水面のある領域のみ計算
         IF( KF(I,J).LT.MZ .AND. KF(I,J+1).LT.MZ ) THEN
            KPMAX1 = MAX(KP(I,J),KP(I,J+1))
            KFMAX1 = MAX(KF(I,J),KF(I,J+1))
C
C ......... 両側が圧力計算セルでない場合のVは非計算だが他の不定となるため、
C           下側のセルからコピーする。
             K=KFMAX1+1
C            DO 310 K=KFMAX1+1,KFMAX1+1
              IF( INDV(I,J,K).GT.0 ) THEN
                IF( INDV(I,J,K-1).GT.0 ) THEN
                  DVDZ = (VV(I,J,K-1)-VV(I,J,K-2))*ZC(5,K-2)
                  IF( INDV(I,J,K-2).LT.0 ) DVDZ=0.0D0
                  DVDZ = 0.0D0
                  VV(I,J,K) = VV(I,J,K-1)+DVDZ*ZC(3,K-1)
                END IF
              END IF
C  310       CONTINUE
C
C            DO 320 K=KFMAX1+2,MZM
             K=KFMAX1+2
             IF( K.LE.MZ ) VV(I,J,K) = 0.0D0
C  320       CONTINUE
         END IF
  300 CONTINUE
C
C
C ... (3) Wの境界値を設定
      DO 400 J=2,MYM
      DO 400 I=2,MXM
C ...... 水面のある領域のみ計算
         IF( KF(I,J).LT.MZ ) THEN
            K=KF(I,J)
C            DO 410 K=KF(I,J),KF(I,J)+1
               WW(I,J,K  ) = WW(I,J,K-1)
               WW(I,J,K+1) = WW(I,J,K-1)
               IF( K+2.LE.MZ ) WW(I,J,K+2) = 0.0D0
C  410       CONTINUE
C
C            DO 420 K=KF(I,J)+2,MZM
C               WW(I,J,K) = 0.0D0
C  420       CONTINUE
         END IF
  400 CONTINUE
C
      IF(IFL.EQ.0) RETURN
C
C ... 計算開始時の流速の初期値(U,V,W)をセット
C
C     DO 500 J=2,MYM
C     DO 510 I=2,MXM
      DO 500 J=2,MY
      DO 510 I=2,MX
        IF(KF(I,J).LT.MZ.AND.
     $    (KF(I,J).GT.KH(I,J).OR.KH(I,J).EQ.KG(I,J))) THEN
C
          IF(KF(I,J).GT.KH(I,J)+1) THEN
            WRITE(LP,600) ISTEP,TIME,I,J,KF(I,J),KH(I,J)
  600       FORMAT('## KF.GT.KH+1 ##   ISTEP =',I10,
     $             '   TIME =',1P,D12.5,'   (I,J)=',2I5,
     $             '   KF,KH=',2I5)
          END IF
C
C ....... <流速U+>
          IF(I.NE.MX) THEN
          IF(KF(I-1,J).GE.KF(I,J)) THEN
            DO 520 K=KH(I,J),KF(I,J)
              IF(K.GE.KF(I+1,J)) THEN
                IF(INDU(I,J,K).GT.0) THEN
                  IF(INDU(I,J,K-1).LE.-1) THEN
                    GV1 = GV(I,J,K)*XC(7,I,J)+GV(I+1,J,K)*XC(8,I,J)
                    IF(GX(I,J,K).LE.GV1) THEN
C ................. 上流側流速コピー(防潮提なし）
                      DHI = HH(I  ,J)-(ZC(1,K)-GV(I  ,J,K)*ZC(4,K))
                      DHP = HH(I+1,J)-(ZC(1,K)-GV(I+1,J,K)*ZC(4,K))
                      IF(HH(I,J).GT.HH(I+1,J).AND.DHI.GT.GXB
     $                                       .AND.DHP.LE.EPSH) THEN
                        IF(INDU(I-1,J,K).GT.0) UU(I,J,K) = UU(I-1,J,K)
                      END IF
                    END IF
                  ELSE
C ................. 下側流速コピー
                    IF(K.GT.KF(I+1,J).AND.UU(I,J,K).EQ.0.0D0) THEN
                      UU(I,J,K) = UU(I,J,K-1)
                    END IF
                  END IF
                END IF
              END IF
  520       CONTINUE
          END IF
          END IF
C
C ....... <流速U->
          IF(KFD(I+1,J).GE.KF(I,J)) THEN
            DO 530 K=KH(I,J),KF(I,J)
              IF(K.GE.KF(I-1,J)) THEN
                IF(INDU(I-1,J,K).GT.0) THEN
                  IF(INDU(I-1,J,K-1).LE.-1) THEN
                    GV1 = GV(I-1,J,K)*XC(7,I-1,J)+GV(I,J,K)*XC(8,I-1,J)
                    IF(GX(I-1,J,K).LE.GV1) THEN
C ................. 上流側流速コピー(防潮提なし）
                      DHI = HH(I  ,J)-(ZC(1,K)-GV(I  ,J,K)*ZC(4,K))
                      DHM = HH(I-1,J)-(ZC(1,K)-GV(I-1,J,K)*ZC(4,K))
                      IF(HH(I,J).GT.HH(I-1,J).AND.DHI.GT.GXB
     $                                       .AND.DHM.LE.EPSH) THEN
                        IF(INDU(I,J,K).GT.0) UU(I-1,J,K) = UU(I,J,K)
                      END IF
                    END IF
                  ELSE
C ................. 下側流速コピー
                    IF(K.GT.KF(I-1,J).AND.UU(I-1,J,K).EQ.0.0D0) THEN
                      UU(I-1,J,K) = UU(I-1,J,K-1)
                    END IF
                  END IF
                END IF
              END IF
  530       CONTINUE
          END IF
C
          IF(J.NE.MY) THEN
C ....... <流速V+>
          IF(KF(I,J-1).GE.KF(I,J)) THEN
            DO 540 K=KH(I,J),KF(I,J)
              IF(K.GE.KF(I,J+1)) THEN
                IF(INDV(I,J,K).GT.0) THEN
                  IF(INDV(I,J,K-1).LE.-2) THEN
                    GV1 = GV(I,J,K)*YC(7,J)+GV(I,J+1,K)*YC(8,J)
                    IF(GY(I,J,K).LE.GV1) THEN
C ................. 上流側流速コピー(防潮提なし）
                      DHJ = HH(I,J  )-(ZC(1,K)-GV(I,J  ,K)*ZC(4,K))
                      DHP = HH(I,J+1)-(ZC(1,K)-GV(I,J+1,K)*ZC(4,K))
                      IF(HH(I,J).GT.HH(I,J+1).AND.DHJ.GT.GXB
     $                                       .AND.DHP.LE.EPSH) THEN
                        IF(INDV(I,J-1,K).GT.0) VV(I,J,K) = VV(I,J-1,K)
                      END IF
                    END IF
                   ELSE
C ................. 下側流速コピー
                    IF(K.GT.KF(I,J+1).AND.VV(I,J,K).EQ.0.0D0) THEN
                      VV(I,J,K) = VV(I,J,K-1)
                    END IF
                  END IF
                END IF
              END IF
  540       CONTINUE
          END IF
          END IF
C
C ....... <流速V->
          IF(KFD(I,J+1).GE.KF(I,J)) THEN
            DO 550 K=KH(I,J),KF(I,J)
              IF(K.GE.KF(I,J-1)) THEN
                IF(INDV(I,J-1,K).GT.0) THEN
                  IF(INDV(I,J-1,K-1).LE.-1) THEN
                    GV1 = GV(I,J-1,K)*YC(7,J-1)+GV(I,J,K)*YC(8,J-1)
                    IF(GY(I,J-1,K).LE.GV1) THEN
C ................. 上流側流速コピー(防潮提なし）
                      DHJ = HH(I,  J)-(ZC(1,K)-GV(I,J  ,K)*ZC(4,K))
                      DHM = HH(I,J-1)-(ZC(1,K)-GV(I,J-1,K)*ZC(4,K))
                      IF(HH(I,J).GT.HH(I,J-1).AND.DHJ.GT.GXB
     $                                       .AND.DHM.LE.EPSH) THEN
                        IF(INDV(I,J,K).GT.0) VV(I,J-1,K) = VV(I,J,K)
                      END IF
                    END IF
                  ELSE
C ................. 下側流速コピー
                    IF(K.GT.KF(I,J-1).AND.VV(I,J-1,K).EQ.0.0D0) THEN
                      VV(I,J-1,K) = VV(I,J-1,K-1)
                    END IF
                  END IF
                END IF
              END IF
  550       CONTINUE
          END IF
        END IF
  510 CONTINUE
  500 CONTINUE
C
C .... <流速W>
      DO 560 J=2,MYM
      DO 560 I=2,MXM
        IF(KF(I,J).LT.MZ) THEN
C ....... 連続の式から計算
          DO 570 K=KH(I,J),KF(I,J)-1
            UIN = (HU(I,J,K)-HU(I-1,J,K))*XC(6,I,J)
            VIN = (HV(I,J,K)-HV(I,J-1,K))*YC(6,J)/YCOS(J)
            WPD = GZ(I,J,K-1)*WW(I,J,K-1)-ZC(4,K)*(UIN+VIN)
            WW(I,J,K) = WPD/GZ(I,J,K)
  570     CONTINUE
C ....... 表層は勾配ゼロでセット
           WW(I,J,KF(I,J)) = WW(I,J,KF(I,J)-1)
           WW(I,J,KF(I,J)+1) = WW(I,J,KF(I,J))
        END IF
  560 CONTINUE
C
      RETURN
      END
