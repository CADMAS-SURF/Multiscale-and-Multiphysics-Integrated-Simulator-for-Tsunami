      SUBROUTINE FLUXUP(DU,UU,VV,WW,TMU,XC,YC,ZC,INDU,LLWALP)
C======================================================================
C     X方向の運動量保存式の右辺に、板境界における剪断応力を付加する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'BOUNDI.h'
      INCLUDE 'BOUNDR.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'TABLER.h'
C
      REAL(8),INTENT(INOUT)::DU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::LLWALP(8,MLWALP)
C
      REAL(8)::DD1,ENU,UU1,UU2,VEL1,VIS1,VTAU,VV1,WW1
      INTEGER::I,IDIR,ITYP,J,K,M,N
C
C
C----------------------------------------------------------------------
C     (A) LOGLAW以外
C----------------------------------------------------------------------
!CDIR NODEP
      DO 100 N=1,MLWALP
         I = LLWALP(1,N)
         J = LLWALP(2,N)
         K = LLWALP(3,N)
         IDIR = LLWALP(4,N)
         M    = LLWALP(5,N)
         ITYP = LLWALP(6,N)
C
C ...... スリップ条件、壁関数の場合はスキップ
         IF( ITYP.EQ.1 .OR. ITYP.EQ.3 ) THEN
C
C----------------------------------------------------------------------
C     (1) 板の法線方向が±Y方向の場合
C----------------------------------------------------------------------
         IF( IDIR.EQ.2 ) THEN
            IF( ITYP.EQ.3 ) THEN
C ............ 接線方向速度固定
               IF( IWALL(1,M).EQ. 0 ) THEN
                  UU2 = RWALL(1,M)
               ELSE
                  UU2 = TABLE(IWALL(1,M))
               END IF
            ELSE
C ............ ノースリップ
               UU2 = 0.0D0
            END IF
C
C ......... 板の-Y側の面の剪断応力
            UU1 = 0.5D0*(UU(I-1,J,K)+UU(I,J,K))
            ENU = TMU(I,J,K)+ANUH
            VIS1 = ENU*(UU2-UU1)*2.0D0*YC(6,J)*YC(6,J)
            IF( INDU(I-1,J  ,K).GT.0 ) DU(I-1,J  ,K)
     $                               = DU(I-1,J  ,K) + VIS1*XC(7,I-1,J)
            IF( INDU(I  ,J  ,K).GT.0 ) DU(I  ,J  ,K)
     $                               = DU(I  ,J  ,K) + VIS1*XC(8,I  ,J)
C
C ......... 板の+Y側の面の剪断応力
            UU1 = 0.5D0*(UU(I-1,J+1,K)+UU(I,J+1,K))
            ENU = TMU(I,J+1,K)+ANUH
            VIS1 = ENU*(UU2-UU1)*2.0D0*YC(6,J+1)*YC(6,J+1)
            IF( INDU(I-1,J+1,K).GT.0 ) DU(I-1,J+1,K)
     $                               = DU(I-1,J+1,K) +VIS1*XC(7,I-1,J+1)
            IF( INDU(I  ,J+1,K).GT.0 ) DU(I  ,J+1,K)
     $                               = DU(I  ,J+1,K) +VIS1*XC(8,I  ,J+1)
C
C
C----------------------------------------------------------------------
C     (2) 板の法線方向が±Z方向の場合
C----------------------------------------------------------------------
         ELSE IF( IDIR.EQ.3 ) THEN
            IF( ITYP.EQ.3 ) THEN
C ............ 接線方向速度固定
               IF( IWALL(1,M).EQ. 0 ) THEN
                  UU2 = RWALL(1,M)
               ELSE
                  UU2 = TABLE(IWALL(1,M))
               END IF
            ELSE
C ............ ノースリップ
               UU2 = 0.0D0
            END IF
C
C ......... 板の-Z側の面の剪断応力
            UU1 = 0.5D0*(UU(I-1,J,K)+UU(I,J,K))
            ENU = TMU(I,J,K)+ANUV
            VIS1 = ENU*(UU2-UU1)*2.0D0*ZC(6,K)*ZC(6,K)
            IF( INDU(I-1,J,K  ).GT.0 ) DU(I-1,J,K  )
     $                               = DU(I-1,J,K  ) + VIS1*XC(7,I-1,J)
            IF( INDU(I  ,J,K  ).GT.0 ) DU(I  ,J,K  )
     $                               = DU(I  ,J,K  ) + VIS1*XC(8,I  ,J)
C
C ......... 板の+Z側の面の剪断応力
            UU1 = 0.5D0*(UU(I-1,J,K+1)+UU(I,J,K+1))
            ENU = TMU(I,J,K+1)+ANUV
            VIS1 = ENU*(UU2-UU1)*2.0D0*ZC(6,K+1)*ZC(6,K+1)
            IF( INDU(I-1,J,K+1).GT.0 ) DU(I-1,J,K+1)
     $                               = DU(I-1,J,K+1) + VIS1*XC(7,I-1,J)
            IF( INDU(I  ,J,K+1).GT.0 ) DU(I  ,J,K+1)
     $                               = DU(I  ,J,K+1) + VIS1*XC(8,I  ,J)
         END IF
         END IF
  100 CONTINUE
C
C
C----------------------------------------------------------------------
C     (B) LOGLAW
C----------------------------------------------------------------------
      IF( ILGLWP.EQ.1 ) THEN
      DO 200 N=1,MLWALP
         I = LLWALP(1,N)
         J = LLWALP(2,N)
         K = LLWALP(3,N)
         IDIR = LLWALP(4,N)
         M    = LLWALP(5,N)
         ITYP = LLWALP(6,N)
C
C----------------------------------------------------------------------
C     (1) 板の法線方向が±Y方向の場合
C----------------------------------------------------------------------
         IF( IDIR.EQ.2 .AND. ITYP.EQ.2 ) THEN
C ......... 板の-Y側の面の剪断応力
            UU1  = 0.5D0*(UU(I-1,J,K)+UU(I,J,K))
            WW1  = 0.5D0*(WW(I,J,K-1)+WW(I,J,K))
            VEL1 = SQRT(UU1*UU1+WW1*WW1)
            DD1  = 0.5D0*YC(4,J)
            CALL LOGLAW(VTAU,VEL1,DD1,ANUH)
            VIS1 = -UU1/MAX(VEL1,1.0D-20)*VTAU*VTAU*YC(6,J)
C
            IF( INDU(I-1,J  ,K).GT.0 ) DU(I-1,J  ,K)
     $                               = DU(I-1,J  ,K) + VIS1*XC(7,I-1,J)
            IF( INDU(I  ,J  ,K).GT.0 ) DU(I  ,J  ,K)
     $                               = DU(I  ,J  ,K) + VIS1*XC(8,I  ,J)
C
C ......... 板の+Y側の面の剪断応力
            UU1  = 0.5D0*(UU(I-1,J+1,K)+UU(I,J+1,K))
            WW1  = 0.5D0*(WW(I,J+1,K-1)+WW(I,J+1,K))
            VEL1 = SQRT(UU1*UU1+WW1*WW1)
            DD1  = 0.5D0*YC(4,J+1)
            CALL LOGLAW(VTAU,VEL1,DD1,ANUH)
            VIS1 = -UU1/MAX(VEL1,1.0D-20)*VTAU*VTAU*YC(6,J+1)
C
            IF( INDU(I-1,J+1,K).GT.0 ) DU(I-1,J+1,K)
     $                               = DU(I-1,J+1,K) +VIS1*XC(7,I-1,J+1)
            IF( INDU(I  ,J+1,K).GT.0 ) DU(I  ,J+1,K)
     $                               = DU(I  ,J+1,K) +VIS1*XC(8,I  ,J+1)
C
C
C----------------------------------------------------------------------
C     (2) 板の法線方向が±Z方向の場合
C----------------------------------------------------------------------
         ELSE IF( IDIR.EQ.3 .AND. ITYP.EQ.2 ) THEN
C ......... 板の-Z側の面の剪断応力
            UU1  = 0.5D0*(UU(I-1,J,K)+UU(I,J,K))
            VV1  = 0.5D0*(VV(I,J-1,K)+VV(I,J,K))
            VEL1 = SQRT(UU1*UU1+VV1*VV1)
            DD1  = 0.5D0*ZC(4,K)
            CALL LOGLAW(VTAU,VEL1,DD1,ANUV)
            VIS1 = -UU1/MAX(VEL1,1.0D-20)*VTAU*VTAU*ZC(6,K)
C
            IF( INDU(I-1,J,K  ).GT.0 ) DU(I-1,J,K  )
     $                               = DU(I-1,J,K  ) + VIS1*XC(7,I-1,J)
            IF( INDU(I  ,J,K  ).GT.0 ) DU(I  ,J,K  )
     $                               = DU(I  ,J,K  ) + VIS1*XC(8,I  ,J)
C
C ......... 板の+Z側の面の剪断応力
            UU1  = 0.5D0*(UU(I-1,J,K+1)+UU(I,J,K+1))
            VV1  = 0.5D0*(VV(I,J-1,K+1)+VV(I,J,K+1))
            VEL1 = SQRT(UU1*UU1+VV1*VV1)
            DD1  = 0.5D0*ZC(4,K+1)
            CALL LOGLAW(VTAU,VEL1,DD1,ANUV)
            VIS1 = -UU1/MAX(VEL1,1.0D-20)*VTAU*VTAU*ZC(6,K+1)
C
            IF( INDU(I-1,J,K+1).GT.0 ) DU(I-1,J,K+1)
     $                               = DU(I-1,J,K+1) + VIS1*XC(7,I-1,J)
            IF( INDU(I  ,J,K+1).GT.0 ) DU(I  ,J,K+1)
     $                               = DU(I  ,J,K+1) + VIS1*XC(8,I  ,J)
         END IF
  200 CONTINUE
      END IF
C
C
C ... DU(MXM,J,K) = DU(MXM,J,K)+DU(1,J,K):EAST ( for DOMAIN-DECOMP )
      CALL FTIMER(75,0)
      CALL CP_NEIBREV(DU,1)
      CALL FTIMER(75,1)
C
      RETURN
      END
