      SUBROUTINE CLWEX(WEXSD,EXSDE,EXSDD,SHLSD,USSD,CSEDI,ZBED,
     $                 ZC,GV,HH,HX,HDEP,INDP,KF,KH,KG)
C======================================================================
C     交換砂量を計算する
C       GRAV  ：重力加速度(m/s2)
C       ZLIMSD：土砂移動を考慮しうる限界水深(m)
C       SSAND ：砂の水中比重(m2/s)
C       DSAND ：砂の粒径(m)
C       GVSAND：砂の空隙率
C       AEXSD ：池野らのモデルで用いられる実験定数a
C       CMAXSD：浮遊砂濃度の上限(0<, ≦1)
C       WSEDI ：沈降速度(m/s)
C       PSIC  ：限界シールズ数
C       SDNU  ：流体の動粘性係数
C======================================================================
C       MWEXSD=0：高橋のモデル,1999
C       MWEXSD=1：池野らのモデル,2009
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'SEDIMENT.h'
      INCLUDE 'TIMER.h'
C
      REAL(8),INTENT(OUT)  ::WEXSD(MX,MY)
      REAL(8),INTENT(INOUT)::EXSDE(MX,MY),EXSDD(MX,MY)
      REAL(8),INTENT(IN)   ::SHLSD(MX,MY),USSD(MX,MY)
      REAL(8),INTENT(IN)   ::CSEDI(MX,MY,MZ)
      REAL(8),INTENT(IN)   ::ZBED(MX,MY)
      REAL(8),INTENT(IN)   ::ZC(8,MZ)
      REAL(8),INTENT(IN)   ::GV(MX,MY,MZ)
      REAL(8),INTENT(IN)   ::HH(MX,MY),HX(MX,MY),HDEP(MX,MY)
      INTEGER,INTENT(IN)   ::INDP(MX,MY,MZ)
      INTEGER,INTENT(IN)   ::KF(MX,MY),KH(MX,MY),KG(MX,MY)
C
C ... ローカル変数
      INTEGER::I,J,K
      REAL(8)::D,DH,SDAMNT,DZB,RSGD,ANSGD,WSSGD
      REAL(8)::CBSEDI    !交換砂量計算に用いる浮遊砂濃度
      REAL(8)::WEXZB     !残存掃流砂の量による巻上の上限交換砂量
      REAL(8)::WEXSDE    !交換砂量WEXSDの巻上(侵食)成分
      REAL(8)::WEXSDD    !交換砂量WEXSDの沈降(堆積)成分
C
C
C======================================================================
C     交換砂量を計算する
C======================================================================
      RSGD=SQRT(SSAND*ABS(GRAV)*DSAND)
      ANSGD=AEXSD*(SDNU**2/(SSAND*ABS(GRAV)*DSAND**3))**0.2D0*RSGD
      WSSGD=(WSEDI/RSGD)**0.8D0
C
      DO 100 J=2,MYM
      DO 100 I=2,MXM
         IF(KF(I,J).EQ.MZ) CYCLE
C
C ...... 交換砂量計算に用いる浮遊砂濃度を決める
C ...... 鉛直平均浮遊砂濃度
         IF(MCONCSD.EQ.0)THEN
            D=HX(I,J)-HDEP(I,J)
            IF(KH(I,J).EQ.KG(I,J))THEN
               CBSEDI=CSEDI(I,J,KG(I,J))
            ELSEIF(D.GE.ZLIMSD)THEN
               SDAMNT=0.0D0
               DO 110 K=2,MZM
                  IF(INDP(I,J,K).EQ.0 .OR. K.GT.KF(I,J)) CYCLE
                  IF(K.NE.KF(I,J))THEN
                     DH=ZC(4,K)*GV(I,J,K)
                  ELSEIF(KF(I,J).NE.KG(I,J))THEN
                     DH=HH(I,J)-ZC(1,K-1)
                  ELSE
                     DH=HH(I,J)-HDEP(I,J)
                  ENDIF
                  SDAMNT=SDAMNT+CSEDI(I,J,K)*DH
  110          CONTINUE
               CBSEDI=SDAMNT/D
            ELSE
               CBSEDI=0.0D0
            ENDIF
C ...... 海底近傍セル浮遊砂濃度
         ELSEIF(MCONCSD.EQ.1)THEN
            CBSEDI=CSEDI(I,J,KG(I,J))
C ...... 藤井ら（2009）の方法による海底浮遊砂濃度
         ELSEIF(MCONCSD.EQ.2)THEN
            IF(KH(I,J).EQ.KG(I,J))THEN
               DZB=HX(I,J)-HDEP(I,J)
            ELSE
               DZB=ZC(4,KG(I,J))*GV(I,J,KG(I,J))
            ENDIF
            CBSEDI=DZB*WSEDI*CSEDI(I,J,KG(I,J))
     $             /(1.0D0-EXP(-WSEDI/(SDKP*USSD(I,J)*SDCK)))
            IF(CBSEDI.GT.CMAXSD) CBSEDI=CMAXSD
         ENDIF
C
C ...... 交換砂量を求める
C ...... 高橋のモデルを用いる
         IF(MWEXSD.EQ.0)THEN
            WEXSDE=0.012D0*SHLSD(I,J)**2*RSGD
C ...... 池野らのモデルを用いる
         ELSEIF(MWEXSD.EQ.1)THEN
            WEXSDE=ANSGD*(WSSGD*MAX(SHLSD(I,J)-PSIC,0.0D0))**2
         ENDIF
         WEXSDD=WSEDI*CBSEDI
         WEXSD(I,J)=WEXSDE-WEXSDD
C
C ...... 残存掃流砂の量による巻上量制限を考慮
         WEXZB=ZBED(I,J)*(1.0D0-GVSAND)/DT
         IF(WEXSD(I,J).GT.WEXZB)THEN
            WEXSD(I,J)=WEXZB
            WEXSDE=WEXSD(I,J)+WEXSDD
         ENDIF
C
C ...... 累積交換砂量
         EXSDE(I,J)=EXSDE(I,J)+WEXSDE*DT
         EXSDD(I,J)=EXSDD(I,J)+WEXSDD*DT
C
  100 CONTINUE
C
      RETURN
      END
