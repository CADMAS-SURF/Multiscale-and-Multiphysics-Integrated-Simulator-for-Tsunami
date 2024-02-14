      SUBROUTINE FLUXWP(DW,UU,VV,WW,TMU,XC,YC,ZC,INDW,LLWALP)
C======================================================================
C     Z方向の運動量保存式の右辺に、板境界における剪断応力を付加する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'BOUNDI.h'
      INCLUDE 'BOUNDR.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'PROPTY.h'
      INCLUDE 'TABLER.h'
C
      REAL(8),INTENT(INOUT)::DW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      INTEGER,INTENT(INOUT)::INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::LLWALP(8,MLWALP)
C
      REAL(8)::DD1,ENU,UU1,VEL1,VIS1,VTAU,VV1,WW1,WW2
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
               IF( IWALL(3,M).EQ. 0 ) THEN
                  WW2 = RWALL(3,M)
               ELSE
                  WW2 = TABLE(IWALL(3,M))
               END IF
            ELSE
C ............ ノースリップ
               WW2 = 0.0D0
            END IF
C
C ......... 板の-X側の面の剪断応力
            WW1 = 0.5D0*(WW(I,J,K-1)+WW(I,J,K))
            ENU = TMU(I,J,K)+ANUH
            VIS1 = ENU*(WW2-WW1)*2.0D0*XC(6,I,J)*XC(6,I,J)
            IF( INDW(I  ,J,K-1).GT.0 ) DW(I  ,J,K-1)
     $                               = DW(I  ,J,K-1) + VIS1*ZC(7,K-1)
            IF( INDW(I  ,J,K  ).GT.0 ) DW(I  ,J,K  )
     $                               = DW(I  ,J,K  ) + VIS1*ZC(8,K  )
C
C ......... 板の+X側の面の剪断応力
            WW1 = 0.5D0*(WW(I+1,J,K-1)+WW(I+1,J,K))
            ENU = TMU(I+1,J,K)+ANUH
            VIS1 = ENU*(WW2-WW1)*2.0D0*XC(6,I+1,J)*XC(6,I+1,J)
            IF( INDW(I+1,J,K-1).GT.0 ) DW(I+1,J,K-1)
     $                               = DW(I+1,J,K-1) + VIS1*ZC(7,K-1)
            IF( INDW(I+1,J,K  ).GT.0 ) DW(I+1,J,K  )
     $                               = DW(I+1,J,K  ) + VIS1*ZC(8,K  )
C
C
C----------------------------------------------------------------------
C     (2) 板の法線方向が±Y方向の場合
C----------------------------------------------------------------------
         ELSE IF( IDIR.EQ.2 ) THEN
            IF( ITYP.EQ.3 ) THEN
C ............ 接線方向速度固定
               IF( IWALL(3,M).EQ. 0 ) THEN
                  WW2 = RWALL(3,M)
               ELSE
                  WW2 = TABLE(IWALL(3,M))
               END IF
            ELSE
C ............ ノースリップ
               WW2 = 0.0D0
            END IF
C
C ......... 板の-Y側の面の剪断応力
            WW1 = 0.5D0*(WW(I,J,K-1)+WW(I,J,K))
            ENU = TMU(I,J,K)+ANUH
            VIS1 = ENU*(WW2-WW1)*2.0D0*YC(6,J)*YC(6,J)
            IF( INDW(I,J  ,K-1).GT.0 ) DW(I,J  ,K-1)
     $                               = DW(I,J  ,K-1) + VIS1*ZC(7,K-1)
            IF( INDW(I,J  ,K  ).GT.0 ) DW(I,J  ,K  )
     $                               = DW(I,J  ,K  ) + VIS1*ZC(8,K  )
C
C ......... 板の+Y側の面の剪断応力
            WW1 = 0.5D0*(WW(I,J+1,K-1)+WW(I,J+1,K))
            ENU = TMU(I,J+1,K)+ANUH
            VIS1 = ENU*(WW2-WW1)*2.0D0*YC(6,J+1)*YC(6,J+1)
            IF( INDW(I,J+1,K-1).GT.0 ) DW(I,J+1,K-1)
     $                               = DW(I,J+1,K-1) + VIS1*ZC(7,K-1)
            IF( INDW(I,J+1,K  ).GT.0 ) DW(I,J+1,K  )
     $                               = DW(I,J+1,K  ) + VIS1*ZC(8,K  )
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
            VIS1 = -WW1/MAX(VEL1,1.0D-20)*VTAU*VTAU*XC(6,I,J)
C
            IF( INDW(I  ,J,K-1).GT.0 ) DW(I  ,J,K-1)
     $                               = DW(I  ,J,K-1) + VIS1*ZC(7,K-1)
            IF( INDW(I  ,J,K  ).GT.0 ) DW(I  ,J,K  )
     $                               = DW(I  ,J,K  ) + VIS1*ZC(8,K  )
C
C ......... 板の+X側の面の剪断応力
            VV1  = 0.5D0*(VV(I+1,J-1,K)+VV(I+1,J,K))
            WW1  = 0.5D0*(WW(I+1,J,K-1)+WW(I+1,J,K))
            VEL1 = SQRT(VV1*VV1+WW1*WW1)
            DD1  = 0.5D0*XC(4,I+1,J)
            CALL LOGLAW(VTAU,VEL1,DD1,ANUH)
            VIS1 = -WW1/MAX(VEL1,1.0D-20)*VTAU*VTAU*XC(6,I+1,J)
C
            IF( INDW(I+1,J,K-1).GT.0 ) DW(I+1,J,K-1)
     $                               = DW(I+1,J,K-1) + VIS1*ZC(7,K-1)
            IF( INDW(I+1,J,K  ).GT.0 ) DW(I+1,J,K  )
     $                               = DW(I+1,J,K  ) + VIS1*ZC(8,K  )
C
C
C----------------------------------------------------------------------
C     (2) 板の法線方向が±Y方向の場合
C----------------------------------------------------------------------
         ELSE IF( IDIR.EQ.2 .AND. ITYP.EQ.2 ) THEN
C ......... 板の-Y側の面の剪断応力
            UU1  = 0.5D0*(UU(I-1,J,K)+UU(I,J,K))
            WW1  = 0.5D0*(WW(I,J,K-1)+WW(I,J,K))
            VEL1 = SQRT(UU1*UU1+WW1*WW1)
            DD1  = 0.5D0*YC(4,J)
            CALL LOGLAW(VTAU,VEL1,DD1,ANUH)
            VIS1 = -WW1/MAX(VEL1,1.0D-20)*VTAU*VTAU*YC(6,J)
C
            IF( INDW(I,J  ,K-1).GT.0 ) DW(I,J  ,K-1)
     $                               = DW(I,J  ,K-1) + VIS1*ZC(7,K-1)
            IF( INDW(I,J  ,K  ).GT.0 ) DW(I,J  ,K  )
     $                               = DW(I,J  ,K  ) + VIS1*ZC(8,K  )
C
C ......... 板の+Y側の面の剪断応力
            UU1  = 0.5D0*(UU(I-1,J+1,K)+UU(I,J+1,K))
            WW1  = 0.5D0*(WW(I,J+1,K-1)+WW(I,J+1,K))
            VEL1 = SQRT(UU1*UU1+WW1*WW1)
            DD1  = 0.5D0*YC(4,J+1)
            CALL LOGLAW(VTAU,VEL1,DD1,ANUH)
            VIS1 = -WW1/MAX(VEL1,1.0D-20)*VTAU*VTAU*YC(6,J+1)
C
            IF( INDW(I,J+1,K-1).GT.0 ) DW(I,J+1,K-1)
     $                               = DW(I,J+1,K-1) + VIS1*ZC(7,K-1)
            IF( INDW(I,J+1,K  ).GT.0 ) DW(I,J+1,K  )
     $                               = DW(I,J+1,K  ) + VIS1*ZC(8,K  )
         END IF
  200 CONTINUE
      END IF
C
      RETURN
      END
