      SUBROUTINE FLUXSZ(FW,RR,HW,TMU,GZ,ZC,INDP,INDW,LLWALL,
     $                  KG,KP,KF,NN,ANUD,DTMU,PARAMN,IZCAL)
C======================================================================
C     Z方向の壁面、流速境界条件（固定、自由流入出）境界値を設定し流束を計算する
C     FW: Z方向格子点、X方向セル中心点、Y方向セル中心点で定義
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
C         NN=3 : 乱流量1(ANUV,SGK:0.0,1.0:0.0,1.0)
C         NN=4 : 乱流量2(ANUV,SGE:0.0,1.0: - , - )
C         NN>4 : 生態系変数
C         NN=-1: DIFVSD,SCTVSD
C       対流項差分スキーム係数(PARAMN)
C         NN=1 : PARAMT
C         NN=2 : PARAMC
C         NN=3 : PARAMK
C         NN=4 : PARAMK
C         NN>4 : 生態系用
C         NN=-1: PARAMSD
C       鉛直方向拡散計算フラグ
C         IZCAL: (=0 計算する 、=1 計算しない )
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
      INCLUDE 'TIMEI.h'
C
      REAL(8),INTENT(INOUT)::FW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::RR(MX,MY,MZ),HW(MX,MY,MZ),TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GZ(MX,MY,MZ),ZC(8,MZ)
      REAL(8),INTENT(INOUT)::ANUD,DTMU,PARAMN
C
      INTEGER,INTENT(INOUT)::LLWALL(8,MLWALL)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KG(MX,MY),KP(MX,MY),KF(MX,MY)
      INTEGER,INTENT(INOUT)::NN,IZCAL
C
      REAL(8)::ADV1,CON1,ENUV,RR1,RRD,ZC1,PARAMN2
      INTEGER::I,IDIR,IE,IS,ITYP,J,JE,JS,K,KE,KF1,KG1,KNB1,KS,M,N
      INTEGER::IOUTF
C
      INTEGER::IZ(MX,MY,MZ)
c@@
      INTEGER::NN2
      NN2=NN
      IF(NN.GE.5) NN2=2
c@@
C
      IZ = 0
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
C ...... 法線方向がZ-方向の面
         IF( IDIR.EQ.4 ) THEN
C
C ......... 勾配ゼロ
            IF( ITYP.EQ.0 ) THEN
               RR(I,J,K+1) = RR(I,J,K)
               IZ(I,J,K) = -1
C
C ......... 壁面の値を固定
            ELSE IF( ITYP.EQ.1 ) THEN
               IF( IWALL(3+NN2,M).EQ. 0 ) THEN
                  RR(I,J,K+1) = RWALL(3+NN2,M)
               ELSE
                  RR(I,J,K+1) = TABLE(IWALL(3+NN2,M))
               END IF
               IF(HW(I,J,K).GT.0.0D0) RR(I,J,K+1)=RR(I,J,K)
            END IF
C
C ...... 法線方向がZ+方向の面
         ELSE IF( IDIR.EQ.5 ) THEN
C
C ......... 勾配ゼロ
            IF( ITYP.EQ.0 ) THEN
               RR(I,J,K) = RR(I,J,K+1)
               IZ(I,J,K) = -1
C
C ......... 壁面の値を固定
            ELSE IF( ITYP.EQ.1 ) THEN
               IF( IWALL(3+NN2,M).EQ. 0 ) THEN
                  RR(I,J,K) = RWALL(3+NN2,M)
               ELSE
                  RR(I,J,K) = TABLE(IWALL(3+NN2,M))
               END IF
               IF(HW(I,J,K).LT.0.0D0) RR(I,J,K)=RR(I,J,K+1)
            END IF
C
         END IF
  100 CONTINUE
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
C ...... 法線方向がZ方向の面
         IF( IDIR.EQ.3 ) THEN
            K = KS
            DO 230 J=JS,JE
            DO 230 I=IS,IE
               IF( INDW(I,J,K).EQ.-1 ) THEN
                  IZ(I,J,K) = -1
                  RRD = RR1
                  IF(INDP(I,J,K).GT.0) THEN
                     IF(HW(I,J,K).GT.0.0D0) RRD=RR(I,J,K)
                     RR(I,J,K+1) = RRD
                  ELSE
                     IF(HW(I,J,K).LT.0.0D0) RRD=RR(I,J,K+1)
                     RR(I,J,K) = RRD
                  END IF
               END IF
  230       CONTINUE
         END IF
C3
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
C ...... 法線方向がZ方向の面
         IF( IDIR.EQ.3 ) THEN
            K = KS
            DO 330 J=JS,JE
            DO 330 I=IS,IE
               IF( INDW(I,J,K).EQ.0 ) THEN
                  IZ(I,J,K) = -1
                  RRD = RR1
                  IF(INDP(I,J,K).GT.0) THEN
                     IF(HW(I,J,K).GT.0.0D0) RRD=RR(I,J,K)
                     IF(IOUTF.LT.0) RRD=RR(I,J,K)
                     RR(I,J,K+1) = RRD
                  ELSE
                     IF(HW(I,J,K).LT.0.0D0) RRD=RR(I,J,K+1)
                     IF(IOUTF.LT.0) RRD=RR(I,J,K+1)
                     RR(I,J,K) = RRD
                  END IF
               END IF
  330       CONTINUE
         END IF
C
  300 CONTINUE
C
C----------------------------------------------------------------------
C     流束計算
C----------------------------------------------------------------------
C
      CALL ZERCLR(FW,MXYZ,0.0D0)
C
      DO 400 K=1,MZM
      DO 400 J=2,MYM
      DO 400 I=2,MXM
C
        IF(LSURF.EQ.1.AND.KF(I,J).EQ.MZ) GO TO 400
C
        KG1 = KG(I,J)-1
        KF1 = MIN(KF(I,J),MZM)
        IF( K.GE.KG1.AND.K.LE.KF1 ) THEN
C
C ....... ±Zのどちらかの側がエネルギー式のZ方向流束成分の計算点である
          IF( INDP(I,J,K).GT.0 .OR. INDP(I,J,K+1).GT.0 ) THEN
C ......... 境界条件処理をする場合のために計算点側のインデックスを設定
            IF( INDP(I,J,K).GT.0 ) THEN
               KNB1 = K+1
               ZC1  = 2.0D0*ZC(6,K)
            ELSE
               KNB1 = K
               ZC1  = 2.0D0*ZC(6,K+1)
            END IF
C
            ENUV = ANUD+(TMU(I,J,K)*ZC(7,K)+TMU(I,J,K+1)*ZC(8,K))/DTMU
            IF(IZ(I,J,K).NE.0) ENUV=0.0D0
            IF(IZCAL.NE.0) ENUV=0.0D0
            IF(K.EQ.KF1) ENUV=0.0D0
C
C ......... (1) 境界条件なし
            IF( INDW(I,J,K).GT.0 ) THEN
               RR1  = RR(I,J,K)*ZC(7,K)+RR(I,J,K+1)*ZC(8,K)
               ADV1 = PARAMN2*HW(I,J,K)*RR1
     $              + PARAMN *(RR(I,J,K  )*MAX(HW(I,J,K),0.0D0)
     $              +          RR(I,J,K+1)*MIN(HW(I,J,K),0.0D0))
               CON1 = GZ(I,J,K)*ENUV*(RR(I,J,K+1)-RR(I,J,K))*ZC(5,K)
C
C ......... (2) 自由流入出境界、速度固定境界、壁境界
            ELSE IF( INDW(I,J,K).LE.0 .AND. INDW(I,J,K).GE.-2 ) THEN
               ADV1 = GZ(I,J,K)*HW(I,J,K)*RR(I,J,KNB1)
               CON1 = GZ(I,J,K)*ENUV*(RR(I,J,K+1)-RR(I,J,K))*ZC1
C
C ......... (3) 板境界
            ELSE IF( INDW(I,J,K).EQ.-3 ) THEN
               ADV1 = 0.0D0
               CON1 = 0.0D0
            END IF
C
            IF(IMVERT.NE.0) CON1=0.0D0
            FW(I,J,K) = CON1 - ADV1
          END IF
        END IF
C
  400 CONTINUE
C
      RETURN
      END
