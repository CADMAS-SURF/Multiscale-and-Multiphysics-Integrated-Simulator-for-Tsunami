      SUBROUTINE CLSHL(SHLSD,USSD,UU,VV,HU,HV,XC,YC,ZC,GV0,HH,HDEP,AMNG,
c                                             ~~~~~~
     $                 KF,KG,GXBDH,GYBDH,KIBDH,KJBDH)
C======================================================================
C     シールズ数および海底摩擦速度を計算する
C       GRAV  ：重力加速度(m/s2)
C       ZLIMSD：土砂移動を考慮しうる限界水深(m)
C       SSAND ：砂の水中比重(m2/s)
C       DSAND ：砂の粒径(m)
C       SDNU  ：流体の動粘性係数
C       SDKP  ：カルマン定数
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'SEDIMENT.h'
C
      REAL(8),INTENT(INOUT)::SHLSD(MX,MY)
      REAL(8),INTENT(OUT)  ::USSD(MX,MY)
      REAL(8),INTENT(IN)   ::UU(MX,MY,MZ),VV(MX,MY,MZ)
      REAL(8),INTENT(IN)   ::HU(MX,MY,MZ),HV(MX,MY,MZ)
      REAL(8),INTENT(IN)   ::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)   ::GV0(MX,MY,MZ)
      REAL(8),INTENT(IN)   ::HH(MX,MY),HDEP(MX,MY)
      REAL(8),INTENT(IN)   ::AMNG(MX,MY)
      REAL(8),INTENT(INOUT)::GXBDH(MX,MY),GYBDH(MX,MY)
      INTEGER,INTENT(IN)   ::KF(MX,MY),KG(MX,MY)
      INTEGER,INTENT(IN)   ::KIBDH(MX,MY),KJBDH(MX,MY)
C
C ... ローカル変数
      REAL(8)::KS(MX,MY) !相当粗度
      REAL(8)::USX(MX,MY)!海底摩擦速度(at x-dir cell-interface)
      REAL(8)::USY(MX,MY)!海底摩擦速度(at y-dir cell-interface)
      REAL(8)::UC,VC,VEL !セル中心での最下層格子流速or線流量
      REAL(8)::UC1,VC1   !セル中心での最下層格子流速or線流量
      REAL(8)::UC2,VC2   !セル中心での最下層格子流速or線流量
      REAL(8)::UU1,UU2,VV1,VV2
      REAL(8)::DZB       !海底面から最下層セル中心までの高さ
      REAL(8)::DZB1,DZB2 !
      REAL(8)::Z0        !粗度高さ
      REAL(8)::D,D1,D2   !全水深
      REAL(8)::HH1,HDEP1,C1,C2,C3,C4,KS1
      INTEGER::I,J,K
      INTEGER,PARAMETER:: LOGFORM=0 ! =0:Integral,=1:normal
C
C
C======================================================================
C     ①海底摩擦速度を計算する
C       MUSTSD=0：平均流れの対数分布則
C       MUSTSD=1：関根,2005
C======================================================================
      USSD=0.0D0
C ... 海底摩擦速度に対数分布則を用いる
      IF(MUSTSD.EQ.0)THEN
C======================================================================
C     ②海底摩擦速度に対数分布則を用いる場合は相当粗度を計算する
C       MRGHSD=0：粒径(CONST.)
C       MRGHSD=1：小林ら,1996
C       MRGHSD=2：Herrmann,2007
C       MRGHSD=3：Manning-Strickler
C======================================================================
         KS=0.0D0
         USX=-1.0D0
         USY=-1.0D0
         IF(MRGHSD.EQ.0)THEN
            KS=DSAND
         ELSEIF(MRGHSD.EQ.1)THEN
            DO 100 J=2,MYM
            DO 100 I=2,MXM
               IF(KF(I,J).EQ.MZ)CYCLE
               KS(I,J)=5.0D0*SHLSD(I,J)*DSAND
  100       CONTINUE
         ELSEIF(MRGHSD.EQ.2)THEN
            DO 110 J=2,MYM
            DO 110 I=2,MXM
               IF(KF(I,J).EQ.MZ)CYCLE
               KS(I,J)=(2.0D0+4.5D0*MAX(SHLSD(I,J)-PSIC,0.0D0))*DSAND
  110       CONTINUE
         ELSEIF(MRGHSD.EQ.3)THEN
            DO 120 J=2,MYM
            DO 120 I=2,MXM
               IF(KF(I,J).EQ.MZ)CYCLE
               KS(I,J)=2.0201D5*(GRAV*AMNG(I,J)**2)**3
  120       CONTINUE
         ENDIF
C======================================================================
C     ②ここまでが粗度計算
C======================================================================
         DO 130 J=2,MYM
         DO 130 I=2,MXM
            IF(KF(I,J).EQ.MZ) CYCLE
            D=HH(I,J)-HDEP(I,J)
            IF(D.LT.ZLIMSD)CYCLE
C
            Z0=KS(I,J)/30.0D0
C
C  === KG ===
            K=KG(I,J)
C
            UU1=UU(I-1,J,K)
            UU2=UU(I  ,J,K)
            VV1=VV(I,J-1,K)
            VV2=VV(I,J  ,K)
C ......... 構造物の天端が設定されている場所で、地形よりも天端が高い場合に面の流速を0にする
            IF( K.EQ.KIBDH(I-1,J) .AND.
     $                      GV0(I,J,K).GT.GXBDH(I-1,J) ) UU1=0.0D0
            IF( K.EQ.KIBDH(I  ,J) .AND.
     $                      GV0(I,J,K).GT.GXBDH(I  ,J) ) UU2=0.0D0
            IF( K.EQ.KJBDH(I,J-1) .AND.
     $                      GV0(I,J,K).GT.GYBDH(I,J-1) ) VV1=0.0D0
            IF( K.EQ.KJBDH(I,J  ) .AND.
     $                      GV0(I,J,K).GT.GYBDH(I,J  ) ) VV2=0.0D0
C
            DZB=MIN(HH(I,J),ZC(1,K))-HDEP(I,J)
C
            UC=(UU1+UU2)*0.5D0
            VC=(VV1+VV2)*0.5D0
            VEL=SQRT(UC**2+VC**2)
C
            IF( LOGFORM.EQ.0 ) THEN
               USSD(I,J)=SDKP*VEL/(LOG(DZB/Z0)-1.0D0+Z0/DZB)
            ELSE
               USSD(I,J)=SDKP*VEL/LOG(0.5D0*DZB/Z0)
            ENDIF
C
C  === KG+1 ===
            K=KG(I,J)+1
            IF(K.EQ.MZ.OR.K.GT.KF(I,J)) CYCLE
C
            UU1=UU(I-1,J,K)
            UU2=UU(I  ,J,K)
            VV1=VV(I,J-1,K)
            VV2=VV(I,J  ,K)
            UC1=UC
            VC1=VC
            UC2=(UU1+UU2)*0.5D0
            VC2=(VV1+VV2)*0.5D0
C ......... 構造物の天端が設定されている場所で、地形よりも天端が高い場合に面の流速を0にする
            IF( K.EQ.KIBDH(I-1,J) ) UU1=0.0D0
            IF( K.EQ.KIBDH(I  ,J) ) UU2=0.0D0
            IF( K.EQ.KJBDH(I,J-1) ) VV1=0.0D0
            IF( K.EQ.KJBDH(I,J  ) ) VV2=0.0D0
C
            DZB1=DZB
            DZB2=MIN(HH(I,J),ZC(1,K))-ZC(1,K-1)
C
            IF( LOGFORM.EQ.0 ) THEN
               UC=(UC1*DZB1+UC2*DZB2)/(DZB1+DZB2)
               VC=(VC1*DZB1+VC2*DZB2)/(DZB1+DZB2)
               VEL=SQRT(UC**2+VC**2)
               DZB=DZB1+DZB2
               USSD(I,J)=MAX(USSD(I,J),
     $                       SDKP*VEL/(LOG(DZB/Z0)-1.0D0+Z0/DZB))
            ELSE
               VEL=SQRT(UC2**2+VC2**2)
               DZB=DZB1+0.5D0*DZB2
               USSD(I,J)=MAX(USSD(I,J),
     $                       SDKP*VEL/(LOG(DZB/Z0)))
            ENDIF
  130    CONTINUE
C
C ... 海底摩擦速度を関根の方法で計算する
      ELSEIF(MUSTSD.EQ.1)THEN
         DO 140 J=2,MYM
         DO 140 I=2,MXM
            IF(KF(I,J).EQ.MZ)CYCLE
            D=HH(I,J)-HDEP(I,J)
            IF(D.LT.ZLIMSD)CYCLE
            UC=(HU(I-1,J,MZ)+HU(I,J,MZ))*0.5D0/D
            VC=(HV(I,J-1,MZ)+HV(I,J,MZ))*0.5D0/D
            USSD(I,J)=SQRT(ABS(GRAV))*AMNG(I,J)/D**(1.0D0/6.0D0)
     $                               *SQRT(UC**2+VC**2)
  140    CONTINUE
      ENDIF
C
C======================================================================
C     ③シールズ数を更新する
C======================================================================
      DO 150 J=2,MYM
      DO 150 I=2,MXM
         IF(KF(I,J).EQ.MZ)CYCLE
         SHLSD(I,J)=USSD(I,J)**2/(SSAND*ABS(GRAV)*DSAND)
  150 CONTINUE
C
      RETURN
      END
