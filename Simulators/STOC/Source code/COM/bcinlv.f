      SUBROUTINE BCINLV(UU,VV,WW,FF,GX0,GY0,INDU,INDV,INDW,XC,YC,ZC,
     $                  UUBCN,VVBCN,MZ_ML,INDU_ML,INDV_ML,
     $                  I_NS,J_NS,K_NS,IEAS_ML,IWES_ML,JSOU_ML,
     $                  JNOR_ML,KBOT_ML,KTOP_ML,IFL)
C======================================================================
C     流速固定境界の流速の法線方向成分を設定する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'AREA.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'BOUNDR.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'TABLER.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'CP_NESTBC.h'
C// 2006.06.23
      INCLUDE 'CONNEC.h'
C END//
      INCLUDE 'CADMAS.h'
C// 2004.01.16
C      COMMON  / ADD0116 / AMP,TTT,ALL,HHH,AXX
      INCLUDE 'RGWAVE.h'
C END//
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(IN)::FF(MX,MY,MZ)
      REAL(8),INTENT(IN)::GX0(MX,MY,MZ),GY0(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDW(MX,MY,MZ)
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(INOUT)::UUBCN(NXY,MZ,4),VVBCN(NXY,MZ,4)
      INTEGER,INTENT(IN)::MZ_ML
      INTEGER,INTENT(IN)::
     $   INDU_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $           KBOT_ML-1:KTOP_ML+1),
     $   INDV_ML(IWES_ML-1:IEAS_ML+1,JSOU_ML-1:JNOR_ML+1,
     $           KBOT_ML-1:KTOP_ML+1)
      INTEGER,INTENT(IN)::I_NS(2,MX),J_NS(2,MY),K_NS(2,MZ)
      INTEGER,INTENT(IN)::IEAS_ML,IWES_ML,JSOU_ML,
     $                    JNOR_ML,KBOT_ML,KTOP_ML
      INTEGER,INTENT(IN)::IFL
C
      REAL(8)::AKK,DDD,HH1,PAI,SIGMA,UU1,UUU,VV1,WW1
      INTEGER::I,IDIR,IE,IS,J,JE,JS,K,KE,KS,M,N
      INTEGER::INB,JNB,II,JJ
C
C
      IF( NESTFL.GT.0 ) THEN
C----------------------------------------------------------------------(START)
C    親領域の境界流速を設定
C
C// 2006.06.23
      IF( IPECON(5,NRANK+1).LT.0 ) THEN
        I = 1
        DO 50 J=2,MYM
           DO K=2,MZM
              IF( INDU(I,J,K).EQ.-1 ) UU(I,J,K) = UUBCN(J,K,2)
           ENDDO
C
           IF( LVPNAB.EQ.1 .AND. MZ_ML.EQ.3 ) THEN
              INB = I+IVPNAB
              JNB = J
              II  = I_NS(1,I)
              JJ  = J_NS(1,J)
              CALL ZDNEST(UU,FF,GX0,XC(1,1,J),ZC,INDU,I,J,INB,JNB,MX,
     $                    INDU_ML,II,JJ,K_NS,IEAS_ML,IWES_ML,JSOU_ML,
     $                    JNOR_ML,KBOT_ML,KTOP_ML)
           ENDIF
   50   CONTINUE
      END IF
C
      IF( IPECON(6,NRANK+1).LT.0 ) THEN
        I = MXM
        DO 60 J=2,MYM
           DO K=2,MZM
              IF( INDU(I,J,K).EQ.-1 ) UU(I,J,K) = UUBCN(J,K,3)
           ENDDO
C
           IF( LVPNAB.EQ.1 .AND. MZ_ML.EQ.3 ) THEN
              INB = I-IVPNAB
              JNB = J
              II  = I_NS(1,I)
              JJ  = J_NS(1,J)
              CALL ZDNEST(UU,FF,GX0,XC(1,1,J),ZC,INDU,I,J,INB,JNB,MX,
     $                    INDU_ML,II,JJ,K_NS,IEAS_ML,IWES_ML,JSOU_ML,
     $                    JNOR_ML,KBOT_ML,KTOP_ML)
           ENDIF
   60   CONTINUE
      END IF
C
      IF( IPECON(4,NRANK+1).LT.0 ) THEN
        J = 1
        DO 70 I=2,MXM
           DO K=2,MZM
              IF( INDV(I,J,K).EQ.-1 ) VV(I,J,K) = VVBCN(I,K,1)
           ENDDO
C
           IF( LVPNAB.EQ.1 .AND. MZ_ML.EQ.3 ) THEN
              INB = I
              JNB = J+IVPNAB
              II  = I_NS(1,I)
              JJ  = J_NS(1,J)
              CALL ZDNEST(VV,FF,GY0,YC,ZC,INDV,I,J,INB,JNB,MY,
     $                 INDV_ML,II,JJ,K_NS,IEAS_ML,IWES_ML,JSOU_ML,
     $                    JNOR_ML,KBOT_ML,KTOP_ML)
           ENDIF
   70   CONTINUE
      END IF
C
      IF( IPECON(7,NRANK+1).LT.0 ) THEN
        J = MYM
        DO 80 I=2,MXM
           DO K=2,MZM
              IF( INDV(I,J,K).EQ.-1 ) VV(I,J,K) = VVBCN(I,K,4)
           ENDDO
C
           IF( LVPNAB.EQ.1 .AND. MZ_ML.EQ.3 ) THEN
              INB = I
              JNB = J-IVPNAB
              II  = I_NS(1,I)
              JJ  = J_NS(1,J)
              CALL ZDNEST(VV,FF,GY0,YC,ZC,INDV,I,J,INB,JNB,MY,
     $                 INDV_ML,II,JJ,K_NS,IEAS_ML,IWES_ML,JSOU_ML,
     $                    JNOR_ML,KBOT_ML,KTOP_ML)
           ENDIF
   80   CONTINUE
      END IF
C END//
C
      END IF
C----------------------------------------------------------------------(END)
C
      DO 100 N=1,NINLT
C ...... CADMASとの連成時、時間積分前は連成境界に関しては何もしない
         IF( NB_SC.GT.0 .AND. N.LE.4 .AND. IFL.EQ.-1 ) GOTO 100
         M  = MINLT(N)
         IS = IAREA(1,M)
         IE = IAREA(2,M)
         JS = IAREA(3,M)
         JE = IAREA(4,M)
         KS = IAREA(5,M)
         KE = IAREA(6,M)
         IDIR = IAREA(7,M)
C
         IF( IINLT(1,N).EQ. 0 ) THEN
            UU1 = RINLT(1,N)
         ELSE
            UU1 = TABLE(IINLT(1,N))
         END IF
C
C// 2004.01.16
C (模型実験対応)......................................................
C        流速を時間の関数にする場合、上のinclude文を追加しUU1を計算する
C           AMP:片振幅(m) , ALL:波長(m) , TTT :周期(s) , TIME:時刻(s)
C           AKK:角波数  , SIGMA:角周波数(1/s) , DDD:位相差(rad.)
C           HHH:水深(m)
C        境界条件としては、便宜的に一定値入力としておく
C
ckt       IF(AMP.GT.0.0D0) THEN
       IF(AMP.GT.0.0D0.or.AMP.LT.0.0D0) THEN
         PAI = 3.141592653897932D0
C         AMP = 0.005D0
C         ALL = 125.21D0
C         TTT = 40.D0
         DDD = PAI*0.5D0
         SIGMA=2.0D0*PAI/TTT
         AKK  = 2.0D0*PAI/ALL
         UUU = AMP*SIGMA
         HH1 = AMP*COS(-SIGMA*TIME+DDD)
       END IF
C END//.....................................................
C
         IF( IINLT(2,N).EQ. 0 ) THEN
            VV1 = RINLT(2,N)
         ELSE
            VV1 = TABLE(IINLT(2,N))
         END IF
         IF( IINLT(3,N).EQ. 0 ) THEN
            WW1 = RINLT(3,N)
         ELSE
            WW1 = TABLE(IINLT(3,N))
         END IF
C
C ...... 法線方向がX方向の面
         IF( IDIR.EQ.1 ) THEN
            I = IS
C
C        鉛直方向に分布を付けるのであれば、BCINLFの潮位HH1
C        の値を考慮してK方向にUU1の分布を与える
C
            DO 110 K=KS,KE
            DO 110 J=JS,JE
C// 2004.01.16
ckt       IF(AMP.GT.0.0D0) THEN
       IF(AMP.GT.0.0D0.or.AMP.LT.0.0D0) THEN
              UU1 = COSH(AKK*(ZC(2,K)+HHH))/SINH(AKK*HHH)
              UU1 = UUU*UU1*COS(-SIGMA*TIME+DDD)
              IF(HH1.LT.ZC(1,K-1)) UU1=0.0D0
              IF(I.EQ.MXM) UU1=-UU1
       END IF
C END//
              IF( NB_SC.GT.0.AND.N.EQ.1 ) UU1 = UWCAD(J-JS+1,K-KS+1,1)
              IF( NB_SC.GT.0.AND.N.EQ.2 ) UU1 = UECAD(J-JS+1,K-KS+1,1)
C
              IF( INDU(I,J,K).EQ.-1 ) UU(I,J,K) = UU1
  110       CONTINUE
C
C ...... 法線方向がY方向の面
         ELSE IF( IDIR.EQ.2 ) THEN
            J = JS
            DO 120 K=KS,KE
            DO 120 I=IS,IE
C// 2004.01.16
ckt       IF(AMP.GT.0.0D0) THEN
       IF(AMP.GT.0.0D0.or.AMP.LT.0.0D0) THEN
              VV1 = COSH(AKK*(ZC(2,K)+HHH))/SINH(AKK*HHH)
              VV1 = UUU*VV1*COS(-SIGMA*TIME+DDD)
              IF(HH1.LT.ZC(1,K-1)) VV1=0.0D0
              IF(J.EQ.MYM) VV1=-VV1
       END IF
C END//
              IF( NB_SC.GT.0.AND.N.EQ.3 ) VV1 = VSCAD(I-IS+1,K-KS+1,2)
              IF( NB_SC.GT.0.AND.N.EQ.4 ) VV1 = VNCAD(I-IS+1,K-KS+1,2)
C
              IF( INDV(I,J,K).EQ.-1 ) VV(I,J,K) = VV1
  120       CONTINUE
C
C ...... 法線方向がZ方向の面
         ELSE
            K = KS
            DO 130 J=JS,JE
            DO 130 I=IS,IE
               IF( INDW(I,J,K).EQ.-1 ) WW(I,J,K) = WW1
  130       CONTINUE
         END IF
  100 CONTINUE
C
C
      RETURN
      END
