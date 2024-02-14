      SUBROUTINE CLBED(ZBED,ZBEDN,QBX,QBY,SHLSD,WEXSD,UU,VV,HU,HV,
     $                 XC,YC,ZC,HH,HDEP,KF,KG,GX,GY,INDU,INDV,
     $                 MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
     $                 I_ML,J_ML,I_NS,J_NS,XC_ML,YC_ML,XC_NS,YC_NS,
     $                 KF_ML,KG_ML,KF_NS,KG_NS,XC_REF,YC_REF,
     $                 IEAS,IWES,JSOU,JNOR,
     $                 IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,NESML,BUF,
     $                 GXBDH,GYBDH,KIBDH,KJBDH)
C======================================================================
C     掃流砂量および掃流砂高さを計算する
C       GRAV  ：重力加速度(m/s2)
C       SSAND ：砂の水中比重(m2/s)
C       DSAND ：砂の粒径(m)
C       GVSAND：砂の空隙率
C       PSIC  ：限界シールズ数
C       PHIS  ：掃流砂の静止摩擦角(rad)
C       KCMIN ：掃流砂量に海底勾配効果を反映する修正関数KCの下限値
C======================================================================
C       MWEXSD=0：高橋のモデル,1999
C       MWEXSD=1：池野らのモデル,2009
C       MBDSLP=0：掃流砂量に海底勾配を考慮しない
C       MBDSLP=1：掃流砂量に海底勾配を考慮する(MWEXSD=1でのみ有効)
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'mpif.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'SEDIMENT.h'
      INCLUDE 'TIMER.h'
C
      REAL(8),INTENT(INOUT)::ZBED(MX,MY),ZBEDN(MX,MY)
      REAL(8),INTENT(OUT)  ::QBX(MX,MY),QBY(MX,MY)
      REAL(8),INTENT(IN)   ::SHLSD(MX,MY),WEXSD(MX,MY)
      REAL(8),INTENT(IN)   ::UU(MX,MY,MZ),VV(MX,MY,MZ)
      REAL(8),INTENT(IN)   ::HU(MX,MY,MZ),HV(MX,MY,MZ)
      REAL(8),INTENT(IN)   ::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)   ::HH(MX,MY),HDEP(MX,MY)
      REAL(8),INTENT(IN)   ::GX(MX,MY,MZ),GY(MX,MY,MZ)
      INTEGER,INTENT(IN)   ::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(IN)   ::KF(MX,MY),KG(MX,MY)
C
      INTEGER,INTENT(IN)::IEAS,IWES,JSOU,JNOR
      INTEGER,INTENT(IN)::IEAS_ML,IWES_ML,JNOR_ML,JSOU_ML
      INTEGER,INTENT(IN)::MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS
      INTEGER,INTENT(IN)::I_ML(2,MX_ML),J_ML(2,MY_ML),
     $                    I_NS(2,MX_NS),J_NS(2,MY_NS)
      INTEGER,INTENT(IN)::KF_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $                    KG_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1),
     $                    KF_NS(MX_NS,MY_NS),KG_NS(MX_NS,MY_NS)
      REAL(8),INTENT(IN)::XC_ML(8,MX_ML),YC_ML(8,MY_ML),
     $                    XC_NS(8,MX_NS),YC_NS(8,MY_NS)
      REAL(8),INTENT(IN)::XC_REF(8,MX),YC_REF(8,MY)
      INTEGER,INTENT(IN)::NESML(4)
      REAL(8),INTENT(INOUT)::BUF(*)
      REAL(8),INTENT(INOUT)::GXBDH(MX,MY),GYBDH(MX,MY)
      INTEGER,INTENT(IN)   ::KIBDH(MX,MY),KJBDH(MX,MY)
C
C ... ローカル変数
      INTEGER::I,J,K
      REAL(8)::D,RSGD,DZB
      REAL(8)::UC,VC,UVC                  !セル中心での流速
      REAL(8)::ZX,ZY,TANS,TANB,KC         !勾配考慮の場合に使う変数
      REAL(8)::QB,QBXC(MX,MY),QBYC(MX,MY) !セル中心での掃流砂量
      REAL(8),PARAMETER::EPS=1.0D-10      !流速が小さいとき掃流砂量ゼロ
      INTEGER::IPARNT,ICHILD,IERR,IFLAG,ICODE
C
      REAL(8)::QBXC_NS(MX_NS,MY_NS),QBYC_NS(MX_NS,MY_NS)
      REAL(8)::QBXC_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1),
     $         QBYC_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1)
      REAL(8)::QBXCBCN(MXY,4),QBYCBCN(MXY,4)
      REAL(8)::ZBED_NS(MX_NS,MY_NS)
      REAL(8)::ZBED_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1)
      REAL(8)::ZBEDBCN(MXY,4)
      REAL(8)::DH1,DH2,HBX
C
      IPARNT = IPECON(2,NRANK+1)
      ICHILD = IPECON(3,NRANK+1)
      QBXC_ML=0.0D0
      QBYC_ML=0.0D0
C
C
C======================================================================
C     掃流砂量(フラックス)を計算する
C======================================================================
C
      RSGD=SQRT(SSAND*ABS(GRAV)*DSAND**3)
C
C ... いったんセル中心で掃流砂量を求める
      QBXC=0.0D0
      QBYC=0.0D0
C ... 高橋のモデルを用いる
      IF(MWEXSD.EQ.0)THEN
         DO 100 J=2,MYM
         DO 100 I=2,MXM
            IF(KF(I,J).EQ.MZ)CYCLE
C ......... 鉛直平均流速を求める
            D=MAX(HH(I,J)-HDEP(I,J),EPSH)
            UC=(HU(I-1,J,MZ)+HU(I,J,MZ))*0.5D0/D
            VC=(HV(I,J-1,MZ)+HV(I,J,MZ))*0.5D0/D
            UVC=SQRT(UC**2+VC**2)
C ......... 掃流砂量を求める
            IF(UVC.LT.EPS)CYCLE
            QB=21.0D0*RSGD*SHLSD(I,J)**1.5D0
            QBXC(I,J)=QB*UC/UVC
            QBYC(I,J)=QB*VC/UVC
  100    CONTINUE
C ... 池野らのモデルを用いる
      ELSEIF(MWEXSD.EQ.1)THEN
         DO 200 J=2,MYM
         DO 200 I=2,MXM
            IF(KF(I,J).EQ.MZ)CYCLE
C ......... 海底近傍セルの流速を求める
            UC=(UU(I-1,J,KG(I,J))+UU(I,J,KG(I,J)))*0.5D0
            VC=(VV(I,J-1,KG(I,J))+VV(I,J,KG(I,J)))*0.5D0
            UVC=SQRT(UC**2+VC**2)
C ......... 掃流砂量を求める
            IF(UVC.LT.EPS)CYCLE
C ......... 掃流砂量に海底勾配を考慮しない
            IF(MBDSLP.EQ.0)THEN
               IF(SHLSD(I,J).LE.PSIC)CYCLE
               QB=17.0D0*RSGD*SHLSD(I,J)**1.5D0
     $                  *(1.0D0-PSIC/SHLSD(I,J))
     $                  *(1.0D0-SQRT(PSIC/SHLSD(I,J)))
C ......... 掃流砂量に海底勾配を考慮する
            ELSEIF(MBDSLP.EQ.1)THEN
               IF(UC.GE.0.0D0)THEN
                  ZX=ZC(1,1)
                  DO 210 K=2,MZM
                     IF(INDU(I,J,K).LE.-2)THEN
                        ZX=ZX+ZC(4,K)
                     ELSE
                        ZX=ZX+(1.0D0-GX(I,J,K))*ZC(4,K)
                     ENDIF
  210             CONTINUE
               ELSE
                  ZX=ZC(1,1)
                  DO 220 K=2,MZM
                     IF(INDU(I-1,J,K).LE.-2)THEN
                        ZX=ZX+ZC(4,K)
                     ELSE
                        ZX=ZX+(1.0D0-GX(I-1,J,K))*ZC(4,K)
                     ENDIF
  220             CONTINUE
               ENDIF
               IF(VC.GE.0.0D0)THEN
                  ZY=ZC(1,1)
                  DO 230 K=2,MZM
                     IF(INDV(I,J,K).LE.-2)THEN
                        ZY=ZY+ZC(4,K)
                     ELSE
                        ZY=ZY+(1.0D0-GY(I,J,K))*ZC(4,K)
                     ENDIF
  230             CONTINUE
               ELSE
                  ZY=ZC(1,1)
                  DO 240 K=2,MZM
                     IF(INDV(I,J-1,K).LE.-2)THEN
                        ZY=ZY+ZC(4,K)
                     ELSE
                        ZY=ZY+(1.0D0-GY(I,J-1,K))*ZC(4,K)
                     ENDIF
  240             CONTINUE
               ENDIF
               IF(ABS(UC).GT.ABS(VC))THEN
                  TANS=ABS(VC/UC)
                  TANB=((ZX-HDEP(I,J))+TANS*(ZY-HDEP(I,J)))
     $                 /SQRT(TANS*TANS+1.0D0)
               ELSE
                  TANS=ABS(UC/VC)
                  TANB=(TANS*(ZX-HDEP(I,J))+(ZY-HDEP(I,J)))
     $                 /SQRT(TANS*TANS+1.0D0)
               ENDIF
               KC=1.0D0+(1.0D0/SSAND+1.0D0)*TANB/TAN(PHIS)
               KC=MAX(KC,KCMIN)
               IF(SHLSD(I,J).LE.PSIC*KC)CYCLE
               QB=17.0D0/KC*RSGD*SHLSD(I,J)**1.5D0
     $                  *(1.0D0-KC*PSIC/SHLSD(I,J))
     $                  *(1.0D0-SQRT(KC*PSIC/SHLSD(I,J)))
            ENDIF
C
            QBXC(I,J)=QB*UC/UVC
            QBYC(I,J)=QB*VC/UVC
  200    CONTINUE
      ENDIF
C
C ... 仮想セルのセル中心の掃流砂量を受け取る(領域分割の通信)
      CALL CP_DSR_DC2(MX,MY,1,0,1,ZBED)
      CALL CP_DSR_DC2(MX,MY,1,0,1,QBXC)
      CALL CP_DSR_DC2(MX,MY,1,0,1,QBYC)
C
C ... 仮想セルのセル中心の掃流砂量を設定(ネスティングの通信：親⇒子)
CDEBUG      IF(ICHILD.GE.0) THEN
CDEBUG        CALL CP_SNDQBCML2NS(MX,MY,IEAS,IWES,JSOU,JNOR,
CDEBUG     $                      BUF,ZBED,QBXC,QBYC)
CDEBUG      END IF
CDEBUGC
CDEBUG      IF(IPARNT.GE.0.OR.IPFLG.NE.0) THEN
CDEBUG        CALL CP_RCVQBCML2NS(MX_ML,MY_ML,IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
CDEBUG     $                      BUF,ZBED_ML,QBXC_ML,QBYC_ML)
CDEBUG        CALL CP_BCQBCML2NS(IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
CDEBUG     $                     MX_ML,MY_ML,MZ_ML,MX_NS,MY_NS,MZ_NS,
CDEBUG     $                     I_ML,J_ML,I_NS,J_NS,XC_ML,YC_ML,XC_NS,YC_NS,
CDEBUG     $                     KF_ML,KG_ML,KF_NS,KG_NS,ZBED_ML,ZBEDBCN,
CDEBUG     $                     QBXC_ML,QBYC_ML,QBXCBCN,QBYCBCN)
        DO 300 I=2,MXM
           ZBED(I, 1)=ZBED(I,  2)
           ZBED(I,MY)=ZBED(I,MYM)
           QBXC(I, 1)=QBXC(I,  2)
           QBXC(I,MY)=QBXC(I,MYM)
           QBYC(I, 1)=QBYC(I,  2)
           QBYC(I,MY)=QBYC(I,MYM)
CDEBUG           ZBED(I, 1)=ZBEDBCN(I,1)
CDEBUG           ZBED(I,MY)=ZBEDBCN(I,4)
CDEBUG           QBXC(I, 1)=QBXCBCN(I,1)
CDEBUG           QBXC(I,MY)=QBXCBCN(I,4)
CDEBUG           QBYC(I, 1)=QBYCBCN(I,1)
CDEBUG           QBYC(I,MY)=QBYCBCN(I,4)
  300   CONTINUE
        DO 350 J=2,MYM
           ZBED( 1,J)=ZBED(  2,J)
           ZBED(MX,J)=ZBED(MXM,J)
           QBXC( 1,J)=QBXC(  2,J)
           QBXC(MX,J)=QBXC(MXM,J)
           QBYC( 1,J)=QBYC(  2,J)
           QBYC(MX,J)=QBYC(MXM,J)
CDEBUG           ZBED( 1,J)=ZBEDBCN(J,2)
CDEBUG           ZBED(MX,J)=ZBEDBCN(J,3)
CDEBUG           QBXC( 1,J)=QBXCBCN(J,2)
CDEBUG           QBXC(MX,J)=QBXCBCN(J,3)
CDEBUG           QBYC( 1,J)=QBYCBCN(J,2)
CDEBUG           QBYC(MX,J)=QBYCBCN(J,3)
  350   CONTINUE
CDEBUG      END IF
C
C ... 子領域セルのセル中心の掃流砂量を設定(ネスティングの通信：子⇒親)
      IF(IPARNT.GE.0) THEN
        CALL CP_BCQBCNS2ML(IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
     $                     MX_ML,MY_ML,MZ_ML,MX,MY,MZ,
     $                     I_ML,J_ML,I_NS,J_NS,KF_ML,KF,XC_REF,YC_REF,
     $                     QBXC,QBYC,QBXC_ML,QBYC_ML)
        CALL CP_SNDQBCNS2ML(QBXC_ML,QBYC_ML,BUF,MX_ML,MY_ML,MZ_ML,
     $                      IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
     $                      NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4))
      END IF
C
      IF(ICHILD.GE.0) THEN
        CALL CP_RCVQBCNS2ML(QBXC,QBYC,BUF,MX,MY,MZ,IEAS,IWES,JSOU,JNOR,
     $                      NESML(2),NESML(3),NESML(1),NESML(4))
      END IF
C
C ... 補間してセル境界の掃流砂量を計算する
      QBX=0.0D0
      QBY=0.0D0
      DO 400 J=2,MYM
      DO 400 I=1,MXM
         IF(KF(I,J).EQ.MZ.AND.KF(I+1,J).EQ.MZ) CYCLE
C
         DH1=HH(I  ,J)-HDEP(I  ,J)
         DH2=HH(I+1,J)-HDEP(I+1,J)
         IF(DH1.LT.ZLIMSD.OR.DH2.LT.ZLIMSD) THEN
            QBX(I,J)=0.0D0
         ELSEIF(KF(I,J).NE.MZ .AND. KF(I+1,J).NE.MZ)THEN
            QBX(I,J)=QBXC(I,J)*XC(7,I,J)+QBXC(I+1,J)*XC(8,I,J)
         ELSEIF(IPARNT.GE.0 .OR. IPFLG.NE.0)THEN
            IF(    (I.EQ.1  .AND. KF(2  ,J).NE.MZ
     $                      .AND. IPECON(5,NRANK+1).LT.0)
     $         .OR.(I.EQ.MX .AND. KF(MXM,J).NE.MZ
     $                      .AND. IPECON(6,NRANK+1).LT.0)    )THEN
               QBX(I,J)=QBXC(I,J)*XC(7,I,J)+QBXC(I+1,J)*XC(8,I,J)
            ENDIF
         ENDIF
C
C ...... 天端による制限
         IF( KIBDH(I,J).GT.0 ) THEN
            K=KIBDH(I,J)
            HBX=ZC(1,K)-GXBDH(I,J)*ZC(4,K)
            IF( QBX(I,J).GT.0.0D0.AND.HDEP(I  ,J).LT.HBX )QBX(I,J)=0.0D0
            IF( QBX(I,J).LT.0.0D0.AND.HDEP(I+1,J).LT.HBX )QBX(I,J)=0.0D0
         ENDIF
  400 CONTINUE
C
      DO 500 J=1,MYM
      DO 500 I=2,MXM
         IF(KF(I,J).EQ.MZ.AND.KF(I,J+1).EQ.MZ) CYCLE
C
         DH1=HH(I,J  )-HDEP(I,J  )
         DH2=HH(I,J+1)-HDEP(I,J+1)
         IF(DH1.LT.ZLIMSD.OR.DH2.LT.ZLIMSD) THEN
            QBY(I,J)=0.0D0
         ELSEIF(KF(I,J).NE.MZ .AND. KF(I,J+1).NE.MZ)THEN
            QBY(I,J)=QBYC(I,J)*YC(7,J)+QBYC(I,J+1)*YC(8,J)
         ELSEIF(IPARNT.GE.0 .OR. IPFLG.NE.0)THEN
            IF(    (J.EQ.1  .AND. KF(I,2  ).NE.MZ
     $                      .AND. IPECON(4,NRANK+1).LT.0)
     $         .OR.(J.EQ.MY .AND. KF(I,MYM).NE.MZ
     $                      .AND. IPECON(7,NRANK+1).LT.0)    )THEN
               QBY(I,J)=QBYC(I,J)*YC(7,J)+QBYC(I,J+1)*YC(8,J)
            ENDIF
         ENDIF
C
C ...... 天端による制限
         IF( KJBDH(I,J).GT.0 ) THEN
            K=KJBDH(I,J)
            HBX=ZC(1,K)-GYBDH(I,J)*ZC(4,K)
            IF( QBY(I,J).GT.0.0D0.AND.HDEP(I,J  ).LT.HBX )QBY(I,J)=0.0D0
            IF( QBY(I,J).LT.0.0D0.AND.HDEP(I,J+1).LT.HBX )QBY(I,J)=0.0D0
         ENDIF
  500 CONTINUE
C
C======================================================================
C     掃流砂高さを更新する
C======================================================================
      DO 600 J=2,MYM
      DO 600 I=2,MXM
         ZBEDN(I,J)=ZBED(I,J)
C ...... ①交換砂量の効果のみを掃流砂高さに反映する
         ZBED(I,J)=ZBED(I,J)-WEXSD(I,J)/(1.0D0-GVSAND)*DT
         IF(ZBED(I,J).LT.0.0D0) ZBED(I,J)=0.0D0
C ...... ②残存掃流砂に応じて掃流砂量(フラックス)を補正する
         DZB=( (MAX(QBX(I,J),0.0D0)-MIN(QBX(I-1,J),0.0D0))*XC(6,I,J)
     $        +(MAX(QBY(I,J),0.0D0)-MIN(QBY(I,J-1),0.0D0))*YC(6,J)  )
     $       /(1.0D0-GVSAND)*DT
         IF(ZBED(I,J).LT.DZB)THEN
            DZB=MAX(DZB,EPSH)
            IF(QBX(I-1,J).LT.0.0D0) QBX(I-1,J)=QBX(I-1,J)*ZBED(I,J)/DZB
            IF(QBX(I  ,J).GT.0.0D0) QBX(I  ,J)=QBX(I  ,J)*ZBED(I,J)/DZB
            IF(QBY(I,J-1).LT.0.0D0) QBY(I,J-1)=QBY(I,J-1)*ZBED(I,J)/DZB
            IF(QBY(I,J  ).GT.0.0D0) QBY(I,J  )=QBY(I,J  )*ZBED(I,J)/DZB
         ENDIF
  600 CONTINUE
C ...... ②’仮想セルとのやりとり部分も掃流砂量(フラックス)を補正する
      DO 610 I=2,MXM
         DZB=( (MAX(QBX(I,1),0.0D0)-MIN(QBX(I-1,1),0.0D0))*XC(6,I,1)
     $        + MAX(QBY(I,1),0.0D0)*YC(6,1) )/(1.0D0-GVSAND)*DT
         DZB=MAX(DZB,EPSH)
         IF(ZBED(I,1).LT.DZB .AND. QBY(I,1).GT.0.0D0)
     $      QBY(I,1)=QBY(I,1)*ZBED(I,1)/DZB
         DZB=( (MAX(QBX(I,MY),0.0D0)-MIN(QBX(I-1,MY),0.0D0))*XC(6,I,MY)
     $        - MIN(QBY(I,MYM),0.0D0)*YC(6,MY) )/(1.0D0-GVSAND)*DT
         IF(ZBED(I,MY).LT.DZB .AND. QBY(I,MYM).LT.0.0D0)
     $      QBY(I,MYM)=QBY(I,MYM)*ZBED(I,MY)/DZB
  610 CONTINUE
      DO 620 J=2,MYM
         DZB=( (MAX(QBY(1,J),0.0D0)-MIN(QBY(1,J-1),0.0D0))*YC(6,J)
     $        + MAX(QBX(1,J),0.0D0)*XC(6,1,J) )/(1.0D0-GVSAND)*DT
         DZB=MAX(DZB,EPSH)
         IF(ZBED(1,J).LT.DZB .AND. QBX(1,J).GT.0.0D0)
     $      QBX(1,J)=QBX(1,J)*ZBED(1,J)/DZB
         DZB=( (MAX(QBY(MX,J),0.0D0)-MIN(QBY(MX,J-1),0.0D0))*YC(6,J)
     $        - MIN(QBX(MXM,J),0.0D0)*XC(6,MX,J) )/(1.0D0-GVSAND)*DT
         IF(ZBED(MX,J).LT.DZB .AND. QBX(MXM,J).LT.0.0D0)
     $      QBX(MXM,J)=QBX(MXM,J)*ZBED(MX,J)/DZB
  620 CONTINUE
C
      DO 700 J=2,MYM
      DO 700 I=2,MXM
C ...... ③掃流砂量の効果も掃流砂高さに反映する
         ZBED(I,J)=ZBED(I,J)-( (QBX(I,J)-QBX(I-1,J))*XC(6,I,J)
     $                        +(QBY(I,J)-QBY(I,J-1))*YC(6,J)  )
     $                       /(1.0D0-GVSAND)*DT
         IF(ZBED(I,J).LT.0.0D0) ZBED(I,J)=0.0D0
  700 CONTINUE
C
C
      IPARNT = IPECON(2,NRANK+1)
      ICHILD = IPECON(3,NRANK+1)
C
      IF(ICHILD.GE.0) THEN
         CALL CP_SNDZBD(ZBED,BUF,MX,MY,
     $                  IEAS,IWES,JSOU,JNOR,
     $                  NESML(2),NESML(3),NESML(1),NESML(4),ICHILD)
      ENDIF
C
      IF(IPARNT.GE.0) THEN
         CALL CP_RCVZBD(ZBED_ML,BUF,MX_ML,MY_ML,
     $                  IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
     $                  NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4),IPARNT)
         CALL CP_MODZBD(ZBED,ZBED_ML,
     $                  I_ML,J_ML,MX_ML,MY_ML,MX,MY,
     $                  NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4),
     $                  IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML)
      ENDIF
C
      RETURN
      END
