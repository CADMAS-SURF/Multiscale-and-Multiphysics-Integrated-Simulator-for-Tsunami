      SUBROUTINE FLUXVP(DV,UU,VV,WW,TMU,XC,YC,ZC,INDV,LLWALP)
C======================================================================
C     Y方向の運動量保存式の右辺に、板境界における剪断応力を付加する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'BOUNDI.h'
      INCLUDE 'BOUNDR.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'TABLER.h'
C
      REAL(8),INTENT(INOUT)::DV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::LLWALP(8,MLWALP)
C
      REAL(8)::DD1,ENU,UU1,VEL1,VIS1,VTAU,VV1,VV2,WW1
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
C     (1) 板の法線方向が±X方向の場合
C----------------------------------------------------------------------
         IF( IDIR.EQ.1 ) THEN
            IF( ITYP.EQ.3 ) THEN
C ............ 接線方向速度固定
               IF( IWALL(2,M).EQ. 0 ) THEN
                  VV2 = RWALL(2,M)
               ELSE
                  VV2 = TABLE(IWALL(2,M))
               END IF
            ELSE
C ............ ノースリップ
               VV2 = 0.0D0
            END IF
C
C ......... 板の-X側の面の剪断応力
            VV1 = 0.5D0*(VV(I,J-1,K)+VV(I,J,K))
            ENU = TMU(I,J,K)+ANUH
            VIS1 = ENU*(VV2-VV1)*2.0D0*XC(6,I,J)*XC(6,I,J)
            IF( INDV(I  ,J-1,K).GT.0 ) DV(I  ,J-1,K)
     $                               = DV(I  ,J-1,K) + VIS1*YC(7,J-1)
            IF( INDV(I  ,J  ,K).GT.0 ) DV(I  ,J  ,K)
     $                               = DV(I  ,J  ,K) + VIS1*YC(8,J  )
C
C ......... 板の+X側の面の剪断応力
            VV1 = 0.5D0*(VV(I+1,J-1,K)+VV(I+1,J,K))
            ENU = TMU(I+1,J,K)+ANUH
            VIS1 = ENU*(VV2-VV1)*2.0D0*XC(6,I+1,J)*XC(6,I+1,J)
            IF( INDV(I+1,J-1,K).GT.0 ) DV(I+1,J-1,K)
     $                               = DV(I+1,J-1,K) + VIS1*YC(7,J-1)
            IF( INDV(I+1,J  ,K).GT.0 ) DV(I+1,J  ,K)
     $                               = DV(I+1,J  ,K) + VIS1*YC(8,J  )
C
C
C----------------------------------------------------------------------
C     (2) 板の法線方向が±Z方向の場合
C----------------------------------------------------------------------
         ELSE IF( IDIR.EQ.3 ) THEN
            IF( ITYP.EQ.3 ) THEN
C ............ 接線方向速度固定
               IF( IWALL(2,M).EQ. 0 ) THEN
                  VV2 = RWALL(2,M)
               ELSE
                  VV2 = TABLE(IWALL(2,M))
               END IF
            ELSE
C ............ ノースリップ
               VV2 = 0.0D0
            END IF
C
C ......... 板の-Z側の面の剪断応力
            VV1 = 0.5D0*(VV(I,J-1,K)+VV(I,J,K))
            ENU = TMU(I,J,K)+ANUH
            VIS1 = ENU*(VV2-VV1)*2.0D0*ZC(6,K)*ZC(6,K)
            IF( INDV(I,J-1,K  ).GT.0 ) DV(I,J-1,K  )
     $                               = DV(I,J-1,K  ) + VIS1*YC(7,J-1)
            IF( INDV(I,J  ,K  ).GT.0 ) DV(I,J  ,K  )
     $                               = DV(I,J  ,K  ) + VIS1*YC(8,J  )
C
C ......... 板の+Z側の面の剪断応力
            VV1 = 0.5D0*(VV(I,J-1,K+1)+VV(I,J,K+1))
            ENU = TMU(I,J,K+1)+ANUV
            VIS1 = ENU*(VV2-VV1)*2.0D0*ZC(6,K+1)*ZC(6,K+1)
            IF( INDV(I,J-1,K+1).GT.0 ) DV(I,J-1,K+1)
     $                               = DV(I,J-1,K+1) + VIS1*YC(7,J-1)
            IF( INDV(I,J  ,K+1).GT.0 ) DV(I,J  ,K+1)
     $                               = DV(I,J  ,K+1) + VIS1*YC(8,J  )
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
C     (1) 板の法線方向が±X方向の場合
C----------------------------------------------------------------------
         IF( IDIR.EQ.1 .AND. ITYP.EQ.2 ) THEN
C ......... 板の-X側の面の剪断応力
            VV1  = 0.5D0*(VV(I,J-1,K)+VV(I,J,K))
            WW1  = 0.5D0*(WW(I,J,K-1)+WW(I,J,K))
            VEL1 = SQRT(VV1*VV1+WW1*WW1)
            DD1  = 0.5D0*XC(4,I,J)
            CALL LOGLAW(VTAU,VEL1,DD1,ANUH)
            VIS1 = -VV1/MAX(VEL1,1.0D-20)*VTAU*VTAU*XC(6,I,J)
C
            IF( INDV(I  ,J-1,K).GT.0 ) DV(I  ,J-1,K)
     $                               = DV(I  ,J-1,K) + VIS1*YC(7,J-1)
            IF( INDV(I  ,J  ,K).GT.0 ) DV(I  ,J  ,K)
     $                               = DV(I  ,J  ,K) + VIS1*YC(8,J  )
C
C ......... 板の+X側の面の剪断応力
            VV1  = 0.5D0*(VV(I+1,J-1,K)+VV(I+1,J,K))
            WW1  = 0.5D0*(WW(I+1,J,K-1)+WW(I+1,J,K))
            VEL1 = SQRT(VV1*VV1+WW1*WW1)
            DD1  = 0.5D0*XC(4,I+1,J)
            CALL LOGLAW(VTAU,VEL1,DD1,ANUH)
            VIS1 = -VV1/MAX(VEL1,1.0D-20)*VTAU*VTAU*XC(6,I+1,J)
C
            IF( INDV(I+1,J-1,K).GT.0 ) DV(I+1,J-1,K)
     $                               = DV(I+1,J-1,K) + VIS1*YC(7,J-1)
            IF( INDV(I+1,J  ,K).GT.0 ) DV(I+1,J  ,K)
     $                               = DV(I+1,J  ,K) + VIS1*YC(8,J  )
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
            VIS1 = -VV1/MAX(VEL1,1.0D-20)*VTAU*VTAU*ZC(6,K)
C
            IF( INDV(I,J-1,K  ).GT.0 ) DV(I,J-1,K  )
     $                               = DV(I,J-1,K  ) + VIS1*YC(7,J-1)
            IF( INDV(I,J  ,K  ).GT.0 ) DV(I,J  ,K  )
     $                               = DV(I,J  ,K  ) + VIS1*YC(8,J  )
C
C ......... 板の+Z側の面の剪断応力
            UU1  = 0.5D0*(UU(I-1,J,K+1)+UU(I,J,K+1))
            VV1  = 0.5D0*(VV(I,J-1,K+1)+VV(I,J,K+1))
            VEL1 = SQRT(UU1*UU1+VV1*VV1)
            DD1  = 0.5D0*ZC(4,K+1)
            CALL LOGLAW(VTAU,VEL1,DD1,ANUV)
            VIS1 = -VV1/MAX(VEL1,1.0D-20)*VTAU*VTAU*ZC(6,K+1)
C
            IF( INDV(I,J-1,K+1).GT.0 ) DV(I,J-1,K+1)
     $                               = DV(I,J-1,K+1) + VIS1*YC(7,J-1)
            IF( INDV(I,J  ,K+1).GT.0 ) DV(I,J  ,K+1)
     $                               = DV(I,J  ,K+1) + VIS1*YC(8,J  )
         END IF
  200 CONTINUE
      END IF
C
C
C ... DV(I,MYM,K) = DV(I,MYM,K)+DV(I,1,K):NORTH ( for DOMAIN-DECOMP )
      CALL FTIMER(75,0)
      CALL CP_NEIBREV(DV,2)
      CALL FTIMER(75,1)
C
      RETURN
      END
