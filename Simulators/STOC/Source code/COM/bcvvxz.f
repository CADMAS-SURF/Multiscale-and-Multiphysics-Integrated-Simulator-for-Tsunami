      SUBROUTINE BCVVXZ(VVX,VVZ,UU,VV,WW,TMU,XCP,YC,ZC,LLWALL,INDP,
     $                  INDU,INDV,INDW,KG,GV,HH,UUBCN,VVBCN)
C======================================================================
C     壁面、流速固定境界および自由流出境界の接線方向流速を設定する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'AREA.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'BOUNDR.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'TABLER.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'CADMAS.h'
C
      REAL(8),INTENT(INOUT)::VVX(MX,MY,MZ),VVZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XCP(8,MX,MY),YC(8,MY),ZC(8,MZ)
C
      INTEGER,INTENT(INOUT)::LLWALL(8,MLWALL)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
C
      INTEGER,INTENT(INOUT)::KG(MX,MY)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY)
      REAL(8),INTENT(INOUT)::UUBCN(NXY,MZ,4),VVBCN(NXY,MZ,4)
C
      INTEGER::NFL=0
C
      REAL(8)::DD1,ENU,UU1,VEL1,VTAU,VV1,VVD,WW1
      REAL(8),PARAMETER::VSLIP=1.0D5
      INTEGER::I,IDIR,IE,INB1,IS,ITYP,J,JE,JS,K,KE,KNB1,KS,M,N
C
C
      CALL ZERCLR(VVX,MXYZ,0.0D0)
      CALL ZERCLR(VVZ,MXYZ,0.0D0)
C
      IF(NESTFL.GT.0) THEN
C----------------------------------------------------------------------
C     親域の境界流速を設定(接線方向流速)
C     (I=1:I=MXM),J=2-MYM
C----------------------------------------------------------------------
      IF( IPECON(5,NRANK+1).LT.0 ) THEN
        I = 1
        DO 55 K=2,MZM
        DO 50 J=2,MYM
           IF( INDU(I,J,K).EQ.-1 ) THEN
              IF(LNTANG.EQ.1.OR.
     $          (LNTANG.EQ.2.AND.UUBCN(J,K,2).LT.0.0D0)) THEN
                 VVX(I,J,K) = 0.5D0*(VV(I+1,J-1,K)+VV(I+1,J,K))
              ELSE
                 VVX(I,J,K) = VVBCN(J,K,2)
              ENDIF
           ENDIF
   50   CONTINUE
   55   CONTINUE
      END IF
C
      IF( IPECON(6,NRANK+1).LT.0 ) THEN
        I = MXM
        DO 65 K=2,MZM
        DO 60 J=2,MYM
           IF( INDU(I,J,K).EQ.-1 ) THEN
              IF(LNTANG.EQ.1.OR.
     $          (LNTANG.EQ.2.AND.UUBCN(J,K,3).GT.0.0D0)) THEN
                 VVX(I,J,K) = 0.5D0*(VV(I  ,J-1,K)+VV(I  ,J,K))
              ELSE
                 VVX(I,J,K) = VVBCN(J,K,3)
              ENDIF
           ENDIF
   60   CONTINUE
   65   CONTINUE
      END IF
C
      END IF
C
C
C----------------------------------------------------------------------
C     (1) 壁面境界(LOG-LAW以外)
C----------------------------------------------------------------------
!CDIR NODEP
      DO 100 N=1,MLWALL1
         I = LLWALL(1,N)
         J = LLWALL(2,N)
         K = LLWALL(3,N)
         IDIR = LLWALL(4,N)
         M    = LLWALL(5,N)
         ITYP = LLWALL(6,N)
C
C
C ...... 法線方向がX方向の面
         IF( IDIR.EQ.0 .OR. IDIR.EQ.1 ) THEN
            INB1 = I
            IF( IDIR.EQ.1 ) INB1 = I+1
C
C ......... ノースリップ
            IF( ITYP.EQ.1 ) THEN
               VVX(I,J,K) = 0.0D0
C
C ......... 接線方向速度固定
            ELSE IF( ITYP.EQ.3 ) THEN
               IF( IWALL(2,M).EQ. 0 ) THEN
                  VVX(I,J,K) = RWALL(2,M)
               ELSE
                  VVX(I,J,K) = TABLE(IWALL(2,M))
               END IF
C
C ......... スリップ
            ELSE IF( ITYP.EQ.0 ) THEN
               VVX(I,J,K) = VSLIP
            END IF
C
C
C ...... 法線方向がZ方向の面
         ELSE IF( IDIR.EQ.4 .OR. IDIR.EQ.5 ) THEN
            KNB1 = K
            IF( IDIR.EQ.5 ) KNB1 = K+1
C
C ......... ノースリップ
            IF( ITYP.EQ.1 ) THEN
               VVZ(I,J,K) = 0.0D0
C
C ......... 接線方向速度固定
            ELSE IF( ITYP.EQ.3 ) THEN
               IF( IWALL(2,M).EQ. 0 ) THEN
                  VVZ(I,J,K) = RWALL(2,M)
               ELSE
                  VVZ(I,J,K) = TABLE(IWALL(2,M))
               END IF
C
C ......... スリップ
            ELSE IF( ITYP.EQ.0 ) THEN
               VVZ(I,J,K) = VSLIP
            END IF
         END IF
  100 CONTINUE
C
C
C----------------------------------------------------------------------
C     (1-2) 壁面境界(LOG-LAW)
C           CALL LOGLAWによりベクトル化できないため分離
C----------------------------------------------------------------------
      IF( ILGLWL.EQ.1 ) THEN
      DO 110 N=1,MLWALL1
         I = LLWALL(1,N)
         J = LLWALL(2,N)
         K = LLWALL(3,N)
         IDIR = LLWALL(4,N)
         M    = LLWALL(5,N)
         ITYP = LLWALL(6,N)
C
         IF( ITYP.EQ.2 ) THEN
C
C ......... 法線方向がX方向の面
            IF( IDIR.EQ.0 .OR. IDIR.EQ.1 ) THEN
               INB1 = I
               IF( IDIR.EQ.1 ) INB1 = I+1
C
               VV1  = 0.5D0*(VV(INB1,J-1,K)+VV(INB1,J,K))
               WW1  = 0.5D0*(WW(INB1,J,K-1)+WW(INB1,J,K))
               VEL1 = SQRT(VV1*VV1+WW1*WW1)
               DD1  = 0.5D0*XCP(4,I,J)
               ENU  = TMU(INB1,J,K)+ANUH
               CALL LOGLAW(VTAU,VEL1,DD1,ANUH)
               VVX(I,J,K) = VV1 - VV1/MAX(VEL1,1.0D-20)
     $                    * VTAU*VTAU/ENU*DD1
C
C ......... 法線方向がZ方向の面
            ELSE IF( IDIR.EQ.4 .OR. IDIR.EQ.5 ) THEN
               KNB1 = K
               IF( IDIR.EQ.5 ) KNB1 = K+1
C
               UU1  = 0.5D0*(UU(I-1,J,KNB1)+UU(I,J,KNB1))
               VV1  = 0.5D0*(VV(I,J-1,KNB1)+VV(I,J,KNB1))
               VEL1 = SQRT(UU1*UU1+VV1*VV1)
               DD1  = 0.5D0*ZC(4,K)
               ENU  = TMU(I,J,KNB1)+ANUV
               CALL LOGLAW(VTAU,VEL1,DD1,ANUV)
               VVZ(I,J,K) = VV1 - VV1/MAX(VEL1,1.0D-20)
     $                    * VTAU*VTAU/ENU*DD1
            END IF
         END IF
  110 CONTINUE
      END IF
C
C
C----------------------------------------------------------------------
C     (2) 流速固定境界
C----------------------------------------------------------------------
C
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
         IF( IINLT(2,N).EQ. 0 ) THEN
            VV1 = RINLT(2,N)
         ELSE
            VV1 = TABLE(IINLT(2,N))
         END IF
C
C ...... 法線方向がX方向の面
         IF( IDIR.EQ.1 ) THEN
            I = IS
            DO 210 K=KS,KE
            DO 210 J=JS,JE
C ......... ( 流出時は勾配ゼロ )
CC               IF( INDU(I,J,K).EQ.-1 ) VVX(I,J,K) = VV1
               IF( INDU(I,J,K).EQ.-1 ) THEN
                 VVD = VV1
                 IF(INDP(I,J  ,K).GT.0.AND.UU(I,J,K).GT.0.0D0)
     1             VVD = 0.5D0*(VV(I  ,J-1,K)+VV(I  ,J,K))
                 IF(INDP(I+1,J,K).GT.0.AND.UU(I,J,K).LT.0.0D0)
     1             VVD = 0.5D0*(VV(I+1,J-1,K)+VV(I+1,J,K))
                 IF( NB_SC.GT.0.AND.N.EQ.1 ) VVD=UWCAD(J-JS+1,K-KS+1,2)
                 IF( NB_SC.GT.0.AND.N.EQ.2 ) VVD=UECAD(J-JS+1,K-KS+1,2)
C
                 VVX(I,J,K) = VVD
               END IF
  210       CONTINUE
C
C ...... 法線方向がZ方向の面
         ELSE IF( IDIR.EQ.3 ) THEN
            K = KS
            DO 220 J=JS,JE
            DO 220 I=IS,IE
C ......... ( 流出時は勾配ゼロ )
CC               IF( INDW(I,J,K).EQ.-1 ) VVZ(I,J,K) = VV1
               IF( INDW(I,J,K).EQ.-1 ) THEN
                 VVD = VV1
                 IF(INDP(I,J  ,K).GT.0.AND.WW(I,J,K).GT.0.0D0)
     1             VVD = 0.5D0*(VV(I,J-1,K  )+VV(I,J,K  ))
                 IF(INDP(I,J,K+1).GT.0.AND.WW(I,J,K).LT.0.0D0)
     1             VVD = 0.5D0*(VV(I,J-1,K+1)+VV(I,J,K+1))
                 VVZ(I,J,K) = VVD
               END IF
  220       CONTINUE
         END IF
  200 CONTINUE
C
C----------------------------------------------------------------------
C     (3) 自由流入出境界
C----------------------------------------------------------------------
C     NFL = 1 ( VVX,VVZを使用せず )
C
      IF(NFL.EQ.0) THEN
C
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
C ...... 法線方向がX方向の面
         IF( IDIR.EQ.1 ) THEN
            I = IS
            DO 310 K=KS,KE
            DO 310 J=JS,JE
               IF( INDP(I,J,K).GT.0 ) THEN
                  VVX(I,J,K) = 0.5D0*(VV(I  ,J-1,K)+VV(I  ,J,K))
               ELSE
                  VVX(I,J,K) = 0.5D0*(VV(I+1,J-1,K)+VV(I+1,J,K))
               END IF
  310       CONTINUE
C
C ...... 法線方向がZ方向の面
         ELSE IF( IDIR.EQ.3 ) THEN
            K = KS
            DO 320 J=JS,JE
            DO 320 I=IS,IE
               IF( INDP(I,J,K).GT.0 ) THEN
                  VVZ(I,J,K) = 0.5D0*(VV(I,J-1,K  )+VV(I,J,K  ))
               ELSE
                  VVZ(I,J,K) = 0.5D0*(VV(I,J-1,K+1)+VV(I,J,K+1))
               END IF
  320       CONTINUE
         END IF
  300 CONTINUE
C
      END IF
C
      RETURN
      END
