C-----------------------------------------------------------
      SUBROUTINE WINCL3 (WX,WY,PATM,WU,WV,WP,XC,YC)
c     by Kawai (2002.06.13)
C     MODIFIED FOR STOC
C     PATM = 気圧-1013.0hPa
C-----------------------------------------------------------
      IMPLICIT NONE
C
      REAL(8),PARAMETER::GZ=100.D0
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'TYPHOI.h'
      INCLUDE 'TYPHOR.h'
C
CCC      COMMON /PM03/ PP,GG,CH,CR,ER,RO,RW,C1,C2,TE,AV,AH,DLT,FM
C     CR=2ω, RO=ρa, PP=π
CCC      COMMON /RG01/ DLS(50),XOG(50),YOG(50),XLN,YLT
CCC      COMMON /TY01/ NTYH
CCC      COMMON /TY02/ TX(300),TY(300),TP(300),TR(300),TU(300),TV(300)
CCC      COMMON /TY09/ TR1(300),TR2(300),TH1(300),TH2(300)
CCC      common /SELECT/IHNC1,IHNCM,IHNDS
C     IHNC1= 風速係数　0:なし, 1:SY, 2:SF1, 3:SF2, 4:SF3, 5:Δpの関数
C     IHNCM= 合成　　　1:ベクトル, 2:藤井(β=30), 3:藤井(β=rの関数)
C     IHNDS= ひずみ　　0:なし, 1:あり
C
      REAL(8),INTENT(INOUT)::WX(MX,MY),WY(MX,MY),PATM(MX,MY)
      REAL(8),INTENT(INOUT)::WU(MX,MY),WV(MX,MY),WP(MX,MY)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY)
C
      REAL(8)::EPSV=1.0D-10
C
      REAL(8)::BT,C1F,C1XP,C6,CR,CRR,CS1,CS2,CST,DD1,DD2
      REAL(8)::PKSG,PKSG1,PKSG2,PP,PPP,PS1,RO,RR,RRR,SN1
      REAL(8)::SN2,SNT,TE,TEE,TEW,THA,THH,THP,THPW,TPP,TRR
      REAL(8)::TSP,TUU,TVV,TXX,TYY,UU0,UU1,UU2,VGR,VGRS,W1
      REAL(8)::W2,WK,XO,XPSG,XT,XX,XXP,XXX,YO,YT,YY,YYY
      INTEGER::I,IDR,J,KT,MM,NEW,NN
C
      REAL(8)::FUG,X,Y
C
C-----モデルの選択-------------------------------------------------
c
c     *****MEYERS' MODEL *****
      FUG(X,Y)=C1*(SQRT(DD1*(1.D0/X)*EXP(-1.D0/X)+Y**2)-Y)
C     *****FUZITA'S MODEL*****
C     FUG(X,Y)=C1*(SQRT(DD1*(X**2)*(1.+(X**2))**(-1.5)+Y**2)-Y)
C     IHNC1= 風速係数　0:なし, 1:SY, 2:SF1, 3:SF2, 4:SF3, 5:Δpの関数
C     IHNCM= 合成　　　1:ベクトル, 2:藤井(β=30), 3:藤井(β=rの関数)
c     IHNDS= ひずみ　　0:なし, 1:あり
C
C-----台風諸元の設定-----------------------------------------------
C
C.... TE ここで設定
      TE=30.0D0
      PP=3.14159265358979D0
      PPP=PP/180.0D0
      CR =CORI
      RO =RHOA
      KT =ISTEP+1
C
      RRR=3600.D0/DT
      IDR=INT(RRR+0.1D0)
      NN=KT/IDR+1
      MM=MOD(KT,IDR)
      W1=DFLOAT(MM)/DFLOAT(IDR)
      W2=1.0D0-W1
      TXX=W1*TX(NN+1)+W2*TX(NN)
      TYY=W1*TY(NN+1)+W2*TY(NN)
      TPP=W1*TP(NN+1)+W2*TP(NN)
      TRR=W1*TR(NN+1)+W2*TR(NN)
      TUU=W1*TU(NN+1)+W2*TU(NN)
      TVV=W1*TV(NN+1)+W2*TV(NN)
      TSP=SQRT(TUU**2+TVV**2)
C     TXX,TYY：台風の座標，TPP:気圧深度，
C     TRR：半径
C     TUU,TVV：台風の進行速度成分，TSP：台風の進行速度
C
C     台風の進行方向THHの計算
C      IF(ABS(TUU).GT.0.001)THEN
C
      NEW = 1
      IF(NEW.EQ.1) THEN
C.... 新しい処理
        IF (TUU.EQ.0.0D0.AND.TVV.EQ.0.0D0) THEN
          THH = 0.0D0
        ELSE
          THH = ATAN2(TVV,TUU)
          THH = THH / PPP
        END IF
      ELSE
        IF(ABS(TUU).GT.EPSV)THEN
          THH=ATAN(ABS(TVV/TUU))
          THH=THH/PPP
          IF(TUU.GT.0.D0.AND.TVV.LT.0.D0)THH=360.D0-THH
          IF(TUU.LT.0.D0.AND.TVV.GT.0.D0)THH=180.D0-THH
          IF(TUU.LT.0.D0.AND.TVV.LT.0.D0)THH=180.D0+THH
        ELSE
          THH=90.D0
          IF(TVV.LT.0.)THH=270.D0
        ENDIF
      END IF
C
C     傾度風・場の風の計算の共通定数の設定
      TRR =1000.D0*TRR
      CRR=CR*SIN(TYY*PPP)
      CRR=CRR/2.D0
      DD1=100.D0*TPP/RO
C      TEE=TE*PPP+PP/2.
C      CS1=COS(TEE)
C      SN1=SIN(TEE)
      CST=COS(THH*PPP)
      SNT=SIN(THH*PPP)
      YYY=CRR*TRR
      UU0=FUG(1.D0,YYY)
      DD2=C2*(TSP/3.6D0)/UU0
C     DD2：C2/Vg(ro)*Tv
C
c     台風中心から領域原点までの距離
      CALL CISTN (TXX,TYY,XLN,YLT,XT,YT)
      XO=XT
      YO=YT
c     (注)XTとYTの符号はXISTNで反転されている
c
c    -----風速係数の設定-------
      IF(IHNC1.EQ.0) THEN
C       定数
        C1F =C1
        C1XP=C1
        XPSG=0.5D0
        PKSG=2.5D0
      ELSE IF(IHNC1.EQ.1) THEN
C       山口SY
        C1F=0.6D0
        C1XP=0.9D0
        XPSG=0.5D0
        PKSG=2.5D0
      ELSE IF(IHNC1.EQ.2) THEN
C       藤井SF1
        C1F=0.56D0
        C1XP=1.1D0
        XPSG=0.5D0
        PKSG=2.5D0
      ELSE IF(IHNC1.EQ.3) THEN
C       藤井SF2
        C1F=0.47D0
        C1XP=1.05D0
        XPSG=0.43D0
        PKSG=2.5D0
      ELSE IF(IHNC1.EQ.4) THEN
C       藤井SF3
        C1F=0.667D0
        C1XP=1.2D0
        XPSG=0.5D0
        PKSG=2.5D0
      ELSE IF(IHNC1.EQ.5) THEN
c       気圧変数型
        C1F=0.66666667D0
        C1XP=(1.0D0+10.0D0**(0.0231D0*TPP-1.95D0))*C1F
        XPSG=0.5D0
        PKSG=2.5D0
      ELSE
c       無風
         C1F =0.0D0
         C1XP=0.0D0
         XPSG=0.5D0
         PKSG=2.5D0
      ENDIF
c     共通
      PKSG1=PKSG-1.0D0
      PKSG2=1.0D0-1.0D0/PKSG
C.... 前に移動
      TEW=TE
      TEE=TEW*PPP+PP/2.D0
      CS1=COS(TEE)
      SN1=SIN(TEE)
C
c----- 各格子点(i,j)での気圧と風の計算 --------------
c
      DO 2200 J=2,MY
      DO 2200 I=2,MX
C
          XX=XO+XC(1,I-1,J)
          YY=YO+YC(1,J-1)
          RR=SQRT(XX**2+YY**2)
C
C         対象地点の方角のTHPの計算
C
          IF(NEW.EQ.1) THEN
            IF (XX.EQ.0.0D0.AND.YY.EQ.0.0D0) THEN
              THP = 0.0D0
            ELSE
              THP = ATAN2(YY,XX)
              THP = THP / PPP
            END IF
          ELSE
            IF(ABS(XX).GT.EPSV)THEN
              THP=ATAN(ABS(YY/XX))
              THP=THP/PPP
              IF(XX.GT.0.D0.AND.YY.LT.0.D0)THP=360.D0-THP
              IF(XX.LT.0.D0.AND.YY.GT.0.D0)THP=180.D0-THP
              IF(XX.LT.0.D0.AND.YY.LT.0.D0)THP=180.D0+THP
            ELSE
              THP=90.D0
              IF(YY.LT.0.D0)THP=270.D0
            ENDIF
          END IF
C
C         台風の進行方向からの角度THAの計算
          THA=THP-THH
C
c         ----気圧の計算
          XXX=RR/TRR
          YYY=CRR*RR
          IF(XXX.LT.0.0015D0) XXX= 0.0015D0
          WP(I,J)=(1013.D0-TPP)+TPP*EXP(-1.D0/XXX)
          IF(RR.LT.GZ) THEN
            WU(I,J)=0.0D0
            WV(I,J)=0.0D0
            GO TO 2200
          END IF
C
c         ----風速係数の設定
          XXP=XXX/XPSG
          IF(XXP.GT.2.0D0) THEN
            C6=C1F
          ELSE
            C6=C1F+(C1XP-C1F)*XXP**PKSG1*EXP(PKSG2*(1.0D0-XXP**PKSG))
          ENDIF
c
c         -----風の合成
          IF(IHNCM.EQ.1) THEN
c           ----ベクトル合成
            UU1=FUG(XXX,YYY)
            UU2=DD2*UU1
            UU1=UU1/C1*C6
            CS2=XX/RR
            SN2=YY/RR
            WU(I,J)=UU1*(CS1*CS2-SN1*SN2)+UU2*CST
            WV(I,J)=UU1*(SN1*CS2+CS1*SN2)+UU2*SNT
          ELSE
C         ----藤井合成
            WK=CRR*2.0D0*RR-(TSP/3.6D0)*SIN(-THA*PPP)
            VGRS=0.5D0*(-WK+SQRT(WK*WK+4.0D0*DD1/XXX*EXP(-1.0D0/XXX)))
            VGR=VGRS*C6
            IF(IHNCM.EQ.2) THEN
              BT=30.0D0
            ELSE
              IF(XXX.LT.0.5D0) THEN
                BT=0.0D0
              ELSE IF(XXX.LT.1.0D0) THEN
                BT=30.0D0*(XXX-0.5D0)
              ELSE
                BT=15.0D0
              ENDIF
            ENDIF
            THPW=(THP+BT+90.0D0)*PPP
            WU(I,J)=VGR*COS(THPW)
            WV(I,J)=VGR*SIN(THPW)
          ENDIF
C
 2200 CONTINUE
C
C.... 格子点で計算した値をセル中心に補間
      DO 2300 J=2,MYM
      DO 2300 I=2,MXM
        WX(I,J)=(WU(I,J)+WU(I+1,J)+WU(I,J+1)+WU(I+1,J+1))/4.D0
        WY(I,J)=(WV(I,J)+WV(I+1,J)+WV(I,J+1)+WV(I+1,J+1))/4.D0
        PS1    =(WP(I,J)+WP(I+1,J)+WP(I,J+1)+WP(I+1,J+1))/4.D0
        PATM(I,J)=(PS1-1013.0D0)*100.0D0
 2300 CONTINUE
C
      RETURN
      END
