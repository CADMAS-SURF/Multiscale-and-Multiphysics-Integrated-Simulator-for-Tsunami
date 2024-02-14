      SUBROUTINE WALBND(AK,EP,UU,VV,WW,TMU,INDP,INDK,XC,YC,ZC,
     $                  LLWALL,LLWALP,KF)
C======================================================================
C     壁面、板に接するkの値を計算する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'TURBR.h'
C
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::AK(MX,MY,MZ),EP(MX,MY,MZ),TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      INTEGER,INTENT(INOUT)::LLWALL(8,MLWALL),LLWALP(8,MLWALP)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDK(MX,MY,MZ),KF(MX,MY)
C
      INTEGER::NFL=0
C
      REAL(8)::ANU,DCMU,DD1,DKK,ENU,UU1,VEL1,VTAU,VV1,WW1
      INTEGER::I,IDIR,INB1,ITYP,J,JNB1,K,KNB1,M,N
C
C----------------------------------------------------------------------
C     壁関数セルをゼロクリアする
C----------------------------------------------------------------------
      DO 100 K=2,MZM
      DO 100 J=2,MYM
      DO 100 I=2,MXM
        IF(INDK(I,J,K).NE.0) THEN
          AK(I,J,K)=0.0D0
          EP(I,J,K)=0.0D0
        END IF
  100 CONTINUE
C
C----------------------------------------------------------------------
C     壁面境界
C----------------------------------------------------------------------
      DCMU = 0.0D0
      IF(CMU.GT.0.0D0) DCMU=1.0D0/SQRT(CMU)
      DO 200 N=1,MLWALL1
         I = LLWALL(1,N)
         J = LLWALL(2,N)
         K = LLWALL(3,N)
         IDIR = LLWALL(4,N)
         M    = LLWALL(5,N)
         ITYP = LLWALL(6,N)
C
C ...... 法線方向がX方向の面
         IF( IDIR.EQ.0 .OR. IDIR.EQ.1 ) THEN
            INB1 = I
            IF( IDIR.EQ.1 ) INB1 = I+1
C ......... 壁関数
            IF( ITYP.EQ.2 ) THEN
               VV1  = 0.5D0*(VV(INB1,J-1,K)+VV(INB1,J,K))
               WW1  = 0.5D0*(WW(INB1,J,K-1)+WW(INB1,J,K))
               VEL1 = SQRT(VV1*VV1+WW1*WW1)
               DD1  = 0.5D0*XC(4,I,J)
               ENU  = TMU(INB1,J,K)+ANUH
               CALL LOGLAW(VTAU,VEL1,DD1,ANUH)
               AK(INB1,J,K) = AK(INB1,J,K)+VTAU*VTAU*DCMU
               EP(INB1,J,K) = EP(INB1,J,K)+VTAU**3/(AKAR*DD1)
             END IF
C
C ...... 法線方向がY方向の面
         ELSE IF( IDIR.EQ.2 .OR. IDIR.EQ.3 ) THEN
            JNB1 = J
            IF( IDIR.EQ.3 ) JNB1 = J+1
C
C ......... 壁関数
            IF( ITYP.EQ.2 ) THEN
               UU1  = 0.5D0*(UU(I-1,JNB1,K)+UU(I,JNB1,K))
               WW1  = 0.5D0*(WW(I,JNB1,K-1)+WW(I,JNB1,K))
               VEL1 = SQRT(UU1*UU1+WW1*WW1)
               DD1  = 0.5D0*YC(4,J)
               ENU  = TMU(I,JNB1,K)+ANUH
               CALL LOGLAW(VTAU,VEL1,DD1,ANUH)
               AK(I,JNB1,K) = AK(I,JNB1,K)+VTAU*VTAU*DCMU
               EP(I,JNB1,K) = EP(I,JNB1,K)+VTAU**3/(AKAR*DD1)
            END IF
C
C ...... 法線方向がZ方向の面
         ELSE IF( IDIR.EQ.4 .OR. IDIR.EQ.5 ) THEN
            KNB1 = K
            IF( IDIR.EQ.5 ) KNB1 = K+1
C ......... 壁関数
            IF( ITYP.EQ.2 ) THEN
               UU1  = 0.5D0*(UU(I-1,J,KNB1)+UU(I,J,KNB1))
               VV1  = 0.5D0*(VV(I,J-1,KNB1)+VV(I,J,KNB1))
               VEL1 = SQRT(UU1*UU1+VV1*VV1)
               DD1  = 0.5D0*ZC(4,K)
               ENU  = TMU(I,J,KNB1)+ANUV
               CALL LOGLAW(VTAU,VEL1,DD1,ANUV)
               AK(I,J,KNB1) = AK(I,J,KNB1)+VTAU*VTAU*DCMU
               EP(I,J,KNB1) = EP(I,J,KNB1)+VTAU**3/(AKAR*DD1)
            END IF
         END IF
  200 CONTINUE
C----------------------------------------------------------------------
C     板境界
C----------------------------------------------------------------------
      DO 300 N=1,MLWALP
         I = LLWALP(1,N)
         J = LLWALP(2,N)
         K = LLWALP(3,N)
         IDIR = LLWALP(4,N)
         M    = LLWALP(5,N)
         ITYP = LLWALP(6,N)
C
C ..... 板の法線方向が±X方向の場合
         IF( IDIR.EQ.1 ) THEN
C ......... 壁関数
            IF( ITYP.EQ.2 ) THEN
               VV1  = 0.5D0*(VV(I,J-1,K)+VV(I,J,K))
               WW1  = 0.5D0*(WW(I,J,K-1)+WW(I,J,K))
               VEL1 = SQRT(VV1*VV1+WW1*WW1)
               DD1  = 0.5D0*XC(4,I,J)
               ENU  = TMU(I,J,K)+ANUH
               CALL LOGLAW(VTAU,VEL1,DD1,ANUH)
               AK(I,J,K) = AK(I,J,K)+VTAU*VTAU*DCMU
               EP(I,J,K) = EP(I,J,K)+VTAU**2/(AKAR*DD1)
C
               VV1  = 0.5D0*(VV(I+1,J-1,K)+VV(I+1,J,K))
               WW1  = 0.5D0*(WW(I+1,J,K-1)+WW(I+1,J,K))
               VEL1 = SQRT(VV1*VV1+WW1*WW1)
               DD1  = 0.5D0*XC(4,I+1,J)
               ENU  = TMU(I+1,J,K)+ANUH
               CALL LOGLAW(VTAU,VEL1,DD1,ANUH)
               AK(I+1,J,K) = AK(I+1,J,K)+VTAU*VTAU*DCMU
               EP(I+1,J,K) = EP(I+1,J,K)+VTAU**2/(AKAR*DD1)
            END IF
C
C ...... 板の法線方向が±Y方向の場合
         ELSE IF( IDIR.EQ.2 ) THEN
C ......... 壁関数
            IF( ITYP.EQ.2 ) THEN
               UU1  = 0.5D0*(UU(I-1,J,K)+UU(I,J,K))
               WW1  = 0.5D0*(WW(I,J,K-1)+WW(I,J,K))
               VEL1 = SQRT(UU1*UU1+WW1*WW1)
               DD1  = 0.5D0*YC(4,J)
               ENU  = TMU(I,J,K)+ANUH
               CALL LOGLAW(VTAU,VEL1,DD1,ANU)
               AK(I,J,K) = AK(I,J,K)+VTAU*VTAU*DCMU
               EP(I,J,K) = EP(I,J,K)+VTAU**2/(AKAR*DD1)
C
               UU1  = 0.5D0*(UU(I-1,J+1,K)+UU(I,J+1,K))
               WW1  = 0.5D0*(WW(I,J+1,K-1)+WW(I,J+1,K))
               VEL1 = SQRT(UU1*UU1+WW1*WW1)
               DD1  = 0.5D0*YC(4,J+1)
               ENU  = TMU(I,J+1,K)+ANUH
               CALL LOGLAW(VTAU,VEL1,DD1,ANU)
               AK(I,J+1,K) = AK(I,J+1,K)+VTAU*VTAU*DCMU
               EP(I,J+1,K) = EP(I,J+1,K)+VTAU**2/(AKAR*DD1)
            END IF
C ...... 板の法線方向が±Z方向の場合
         ELSE IF( IDIR.EQ.3 ) THEN
C ......... 壁関数
            IF( ITYP.EQ.2 ) THEN
               UU1  = 0.5D0*(UU(I-1,J,K)+UU(I,J,K))
               VV1  = 0.5D0*(VV(I,J-1,K)+VV(I,J,K))
               VEL1 = SQRT(UU1*UU1+VV1*VV1)
               DD1  = 0.5D0*ZC(4,K)
               ENU  = TMU(I,J,K)+ANUV
               CALL LOGLAW(VTAU,VEL1,DD1,ANUV)
               AK(I,J,K) = AK(I,J,K)+VTAU*VTAU*DCMU
               EP(I,J,K) = EP(I,J,K)+VTAU**2/(AKAR*DD1)
C
               UU1  = 0.5D0*(UU(I-1,J,K+1)+UU(I,J,K+1))
               VV1  = 0.5D0*(VV(I,J-1,K+1)+VV(I,J,K+1))
               VEL1 = SQRT(UU1*UU1+VV1*VV1)
               DD1  = 0.5D0*ZC(4,K+1)
               ENU  = TMU(I,J,K+1)+ANUV
               CALL LOGLAW(VTAU,VEL1,DD1,ANUV)
               AK(I,J,K+1) = AK(I,J,K+1)+VTAU*VTAU*DCMU
               EP(I,J,K+1) = EP(I,J,K+1)+VTAU**2/(AKAR*DD1)
            END IF
         END IF
C
  300 CONTINUE
C
C----------------------------------------------------------------------
C     平均処理
C----------------------------------------------------------------------
      DO 400 K=2,MZM
      DO 400 J=2,MYM
      DO 400 I=2,MXM
        IF(INDK(I,J,K).GT.0) THEN
          DKK = 1.0D0/FLOAT(INDK(I,J,K))
          AK(I,J,K) = AK(I,J,K)*DKK
          EP(I,J,K) = EP(I,J,K)*DKK
        END IF
  400 CONTINUE
C
C----------------------------------------------------------------------
C     計算値をチェックする
C----------------------------------------------------------------------
      DO 500 K=2,MZM
      DO 500 J=2,MYM
      DO 500 I=2,MXM
        IF(INDP(I,J,K).GT.0) THEN
          IF(K.GT.KF(I,J)) THEN
            AK(I,J,K) = 0.0D0
            EP(I,J,K) = 0.0D0
          ELSE
            IF(EP(I,J,K).LT.EPMIN) THEN
              EP(I,J,K) = EPMIN
              AK(I,J,K) = AKMIN
            ELSE IF(AK(I,J,K).LT.AKMIN) THEN
              AK(I,J,K) = AKMIN
            END IF
          END IF
        END IF
  500 CONTINUE
C
      RETURN
      END
