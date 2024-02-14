      SUBROUTINE FLUXSX(FU,RR,HU,TMU,GX,HDEP,HH,XC,ZC,INDP,INDU,LLWALL,
     $                  KG,KP,KF,RRBCN,NN,ANUD,DTMU,PARAMN)
C======================================================================
C     X方向の壁面、流速境界条件（固定、自由流入出）境界値を設定し流束を計算する
C     FU: X方向格子点、Y方向セル中心点、Z方向セル中心点で定義
C       KF,HH : (N)時刻の値
C       境界条件の場合分け
C         (1) 境界条件なし
C         (2) 自由流入出境界,速度固定境界
C         (3) 板境界(流束はゼロの条件のみ)
C       処理変数フラグ(NN)
C         NN=1 : 温度 
C         NN=2 : 塩分
C         NN=3 : 乱流量1(k:q2/2:E)
C         NN=4 : 乱流量2(ε:q2l:-)
C         NN>4 : 生態系変数
C         NN=-1: 浮遊砂
C       実効拡散係数計算用(ANUD,DTMU):ENUH=ANUD+TMU/DDMU
C         NN=1 : ALPH,Prt
C         NN=2 : DIFH,Sct
C         NN=3 : 乱流量1(ANUH,SGK:0.0,1.0:0.0,1.0)
C         NN=4 : 乱流量2(ANUH,SGE:0.0,1.0: - , - )
C         NN>4 : 生態系変数
C         NN=-1: DIFHSD,SCTHSD
C       対流項差分スキーム係数(PARAMN)
C         NN=1 : PARAMT
C         NN=2 : PARAMC
C         NN=3 : PARAMK
C         NN=4 : PARAMK
C         NN>4 : 生態系用
C         NN=-1: PARAMSD
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'AREA.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'BOUNDR.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'TABLER.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'TIMEI.h'
C
      REAL(8),INTENT(INOUT)::FU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::RR(MX,MY,MZ),HU(MX,MY,MZ),TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GX(MX,MY,MZ),XC(8,MX,MY),ZC(8,MZ)
      REAL(8),INTENT(INOUT)::RRBCN(NXY,MZ,4)
      REAL(8),INTENT(INOUT)::HH(MX,MY),HDEP(MX,MY),ANUD,DTMU,PARAMN
C
      INTEGER,INTENT(INOUT)::LLWALL(8,MLWALL)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KG(MX,MY),KP(MX,MY),KF(MX,MY)
      INTEGER,INTENT(INOUT)::NN
C
      REAL(8)::ADV1,CON1,ENUH,RR1,RRD,XC1,PARAMN2,DH1,DH2,DHH,DHX
      INTEGER::I,IDIR,IE,INB1,IS,ITYP,J,JE,JS,K,KE,KF1,KG1,KS,M,N,NESTD
      INTEGER::IOUTF
C
      INTEGER::IX(MX,MY,MZ)
c@@
      INTEGER::NN2
      NN2=NN
      IF(NN.GE.5) NN2=2
c@@
C
      IX = 0
      PARAMN2 = 1.0D0-PARAMN
C
C----------------------------------------------------------------------
C     (1) 壁面境界(NN>2の場合は勾配ゼロ）
C----------------------------------------------------------------------
!CDIR NODEP
      DO 100 N=1,MLWALL1
         I = LLWALL(1,N)
         J = LLWALL(2,N)
         K = LLWALL(3,N)
         IDIR = LLWALL(4,N)
         M    = LLWALL(5,N)
         ITYP = 0
         IF(LSEDI.NE.1 .OR. NN.NE.-1)THEN
            IF(NN2.LE.2) ITYP=LLWALL(6+NN2,N)
         ENDIF
C
C ...... 法線方向がX-方向の面
         IF( IDIR.EQ.0 ) THEN
C
C ......... 勾配ゼロ
            IF( ITYP.EQ.0 ) THEN
               RR(I+1,J,K) = RR(I,J,K)
               IX(I,J,K) = -1
C
C ......... 壁面の値を固定C
            ELSE IF( ITYP.EQ.1 ) THEN
               IF( IWALL(3+NN2,M).EQ. 0 ) THEN
                  RR(I+1,J,K) = RWALL(3+NN2,M)
               ELSE
                  RR(I+1,J,K) = TABLE(IWALL(3+NN2,M))
               END IF
               IF(HU(I,J,K).GT.0.0D0) RR(I+1,J,K)=RR(I,J,K)
            END IF
C
C ...... 法線方向がX+方向の面
         ELSE IF( IDIR.EQ.1 ) THEN
C
C ......... 勾配ゼロ
            IF( ITYP.EQ.0 ) THEN
               RR(I,J,K) = RR(I+1,J,K)
               IX(I,J,K) = -1
C
C ......... 壁面の値を固定
            ELSE IF( ITYP.EQ.1 ) THEN
               IF( IWALL(3+NN2,M).EQ. 0 ) THEN
                  RR(I,J,K) = RWALL(3+NN2,M)
               ELSE
                  RR(I,J,K) = TABLE(IWALL(3+NN2,M))
               END IF
               IF(HU(I,J,K).LT.0.0D0) RR(I,J,K)=RR(I+1,J,K)
            END IF
C
         END IF
  100 CONTINUE
C
C----------------------------------------------------------------------(START)
C     (2-0) 親領域の境界温度を設定
C----------------------------------------------------------------------
      IF(NESTFL.GT.0) THEN
        IF(IPECON(5,NRANK+1).LT.0) THEN
        I = 1
        DO 110 K=2,MZM
        DO 110 J=2,MYM
          KG1 = KG(I+1,J)
          IF(K.GE.KG1) THEN
            IF(INDU(I,J,K).EQ.-1) THEN
              RR1 = RRBCN(J,K,2)
              IF(INDP(I+1,J,K).EQ.1) THEN
                RR(I,J,K) = RR1+PARAMN*(RR1-RR(2,J,K))
              END IF
            END IF
          END IF
  110   CONTINUE
        END IF
C
        IF(IPECON(6,NRANK+1).LT.0) THEN
        I = MXM
        DO 130 K=2,MZM
        DO 130 J=2,MYM
          KG1 = KG(I,J)
          IF(K.GE.KG1) THEN
            IF(INDU(I,J,K).EQ.-1) THEN
              RR1 = RRBCN(J,K,3)
              IF(INDP(I,J,K).EQ.1) THEN
                RR(I+1,J,K) = RR1+PARAMN*(RR1-RR(I,J,K))
              END IF
            END IF
          END IF
  130   CONTINUE
        END IF
C
      END IF
C
C----------------------------------------------------------------------
C     (2) 流速固定境界(固定流速が流入時固定、流出時勾配ゼロ）
C----------------------------------------------------------------------
      DO 200 N=1,NINLT
         M  = MINLT(N)
         IS = IAREA(1,M)
         IE = IAREA(2,M)
         JS = IAREA(3,M)
         JE = IAREA(4,M)
         KS = IAREA(5,M)
         KE = IAREA(6,M)
         IDIR = IAREA(7,M)
C
         IF(LSEDI.EQ.1 .AND. NN.EQ.-1)THEN
            RR1 = 0.0D0
         ELSE
            IF( IINLT(4+NN2,N).EQ. 0 ) THEN
               RR1 = RINLT(4+NN2,N)
            ELSE
               RR1 = TABLE(IINLT(4+NN2,N))
            END IF
         ENDIF
C
C ...... 法線方向がX方向の面
         IF( IDIR.EQ.1 ) THEN
            I = IS
            DO 210 K=KS,KE
            DO 210 J=JS,JE
               IF( INDU(I,J,K).EQ.-1 ) THEN
                  IX(I,J,K) = -1
                  RRD = RR1
                  IF(INDP(I,J,K).GT.0) THEN
                     IF(HU(I,J,K).GT.0.0D0) RRD=RR(I,J,K)
                     RR(I+1,J,K) = RRD
                  ELSE
                     IF(HU(I,J,K).LT.0.0D0) RRD=RR(I+1,J,K)
                     RR(I,J,K) = RRD
                  END IF
               END IF
  210       CONTINUE
         END IF
C
  200 CONTINUE
C
C----------------------------------------------------------------------
C     (3) 自由流入出境界(開境界の場合は流入時固定、流出時勾配ゼロ）
C----------------------------------------------------------------------
      DO 300 N=1,NOUTLT
         M  = MOUTLT(N)
         IS = IAREA(1,M)
         IE = IAREA(2,M)
         JS = IAREA(3,M)
         JE = IAREA(4,M)
         KS = IAREA(5,M)
         KE = IAREA(6,M)
         IDIR = IAREA(7,M)
C
         IF(LSEDI.EQ.1 .AND. NN.EQ.-1)THEN
            RR1 = 0.0D0
            IOUTF=0
         ELSE
            IF(NN2.EQ.3.OR.NN2.EQ.4) THEN
               IOUTF=-1
            ELSE
               IOUTF = IOUTLT(NN2,N)
            END IF
            IF( IOUTF.EQ. 0 ) THEN
               RR1 = ROUTLT(NN2,N)
            ELSE IF( IOUTF.GT.0 ) THEN
               RR1 = TABLE(IOUTF)
            END IF
         ENDIF
C
C ...... 法線方向がX方向の面
         IF( IDIR.EQ.1 ) THEN
            I    = IS
            DO 310 K=KS,KE
            DO 310 J=JS,JE
               IF( INDU(I,J,K).EQ.0 ) THEN
                  IX(I,J,K) = -1
                  RRD = RR1
                  IF(INDP(I,J,K).GT.0) THEN
                     IF(HU(I,J,K).GT.0.0D0) RRD=RR(I,J,K)
                     IF(IOUTF.LT.0) RRD=RR(I,J,K)
                     RR(I+1,J,K) = RRD
                  ELSE
                     IF(HU(I,J,K).LT.0.0D0) RRD=RR(I+1,J,K)
                     IF(IOUTF.LT.0) RRD=RR(I+1,J,K)
                     RR(I,J,K) = RRD
                  END IF
               END IF
  310       CONTINUE
         END IF
C
  300 CONTINUE
C
      CALL CP_DSR_DC2(MX,MY,MZ,0,1,RR)
C
C----------------------------------------------------------------------
C     流束計算
C----------------------------------------------------------------------
      CALL ZERCLR(FU,MXYZ,0.0D0)
C
      DO 400 K=2,MZM
      DO 400 J=2,MYM
      DO 400 I=1,MXM
C
        IF(LSURF.EQ.1) THEN
          IF(KF(I,J).EQ.MZ.AND.KF(I+1,J).EQ.MZ) GO TO 400
        END IF
C
        NESTD = 0
        IF(NESTFL.GT.0.AND.(I.EQ.1.OR.I.EQ.MXM)) NESTD=1
        KG1 = MIN(KG(I,J),KG(I+1,J))
        KF1 = MIN(MAX(KF(I,J),KF(I+1,J)),MZM)
        IF( K.GE.KG1.AND.K.LE.KF1 ) THEN
C
C ....... ±Xのどちらかの側がエネルギー式のX方向流束成分の計算点である
          IF( INDP(I,J,K).GT.0 .OR. INDP(I+1,J,K).GT.0 ) THEN
C ......... 境界条件処理をする場合のために計算点側のインデックスを設定
            IF( INDP(I,J,K).GT.0 ) THEN
               INB1 = I+1
               XC1  = 2.0D0*XC(6,I,J)
            ELSE
               INB1 = I
               XC1  = 2.0D0*XC(6,I+1,J)
            END IF
C
            ENUH = ANUD
     $           +(TMU(I,J,K)*XC(7,I,J)+TMU(I+1,J,K)*XC(8,I,J))/DTMU
            IF(IX(I,J,K).NE.0) ENUH=0.0D0
C
            DHH = 1.0D0
            IF(LSURF.EQ.1) THEN
              DH1 = MIN(ZC(1,K),HH(I  ,J))-MAX(ZC(1,K-1),HDEP(I  ,J))
              DH2 = MIN(ZC(1,K),HH(I+1,J))-MAX(ZC(1,K-1),HDEP(I+1,J))
              IF(KF(I  ,J).EQ.MZ.OR.INDP(I  ,J,K).EQ.0) DH1=DH2
              IF(KF(I+1,J).EQ.MZ.OR.INDP(I+1,J,K).EQ.0) DH2=DH1
              DHH = (DH1*XC(8,I,J)+DH2*XC(7,I,J))*ZC(6,K)
              IF(DH1.LE.0.0D0.OR.DH2.LE.0.0) ENUH=0.0D0 
            END IF
            DHX = DHH*GX(I,J,K)
C
C ......... (1) 境界条件なし
            IF( INDU(I,J,K).GT.0 ) THEN
               RR1  = RR(I,J,K)*XC(7,I,J)+RR(I+1,J,K)*XC(8,I,J)
               ADV1 = PARAMN2*HU(I,J,K)*RR1
     $              + PARAMN *(RR(I  ,J,K)*MAX(HU(I,J,K),0.0D0)
     $              +          RR(I+1,J,K)*MIN(HU(I,J,K),0.0D0))
               CON1 = DHX*ENUH*(RR(I+1,J,K)-RR(I,J,K))*XC(5,I,J)
C
C ......... (2) 自由流入出境界、速度固定境界、壁境界
            ELSE IF( INDU(I,J,K).LE.0 .AND. INDU(I,J,K).GE.-2 ) THEN
               IF(NESTD.EQ.0) THEN
                 ADV1 = HU(I,J,K)*RR(INB1,J,K)
                 CON1 = DHX*ENUH*(RR(I+1,J,K)-RR(I,J,K))*XC1
               ELSE
                 RR1  = RR(I,J,K)*XC(7,I,J)+RR(I+1,J,K)*XC(8,I,J)
                 ADV1 = PARAMN2*HU(I,J,K)*RR1
     $                + PARAMN *(RR(I  ,J,K)*MAX(HU(I,J,K),0.0D0)
     $                +          RR(I+1,J,K)*MIN(HU(I,J,K),0.0D0))
                 CON1 = DHX*ENUH*(RR(I+1,J,K)-RR(I,J,K))*XC(5,I,J)
               END IF
C
C ......... (3) 板境界
            ELSE IF( INDU(I,J,K).EQ.-3 ) THEN
               ADV1 = 0.0D0
               CON1 = 0.0D0
            END IF
C
            FU(I,J,K) = CON1 - ADV1
          END IF
        END IF
C
  400 CONTINUE
C
      RETURN
      END
