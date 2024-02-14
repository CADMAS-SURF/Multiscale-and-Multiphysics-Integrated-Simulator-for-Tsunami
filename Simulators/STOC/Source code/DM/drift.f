      SUBROUTINE DRIFT
C----------------------------------------
C     漂流物の移動計算を行うメインルーチン
C----------------------------------------
      USE M_TIME
      USE M_GRID
      USE M_DRIFT
      USE M_FLUID
      USE M_MODEL
      USE M_OUTPUT,ONLY:IFL
      USE M_WIND,ONLY:WIN_CAL
C
      IMPLICIT NONE
C
      REAL(8),PARAMETER::PI=3.141592653589793D0
C
      REAL(8)::XSM(MXSIDE),XSN(MXSIDE),XSB(MXSIDE),XPS(MXSIDE)
      REAL(8)::YSM(MXSIDE),YSN(MXSIDE),YSB(MXSIDE),YPS(MXSIDE)
C
      REAL(8)::UFSM(MXNK,MXSIDE),UFSN(MXNK,MXSIDE)
      REAL(8)::UFSB(MXNK,MXSIDE),UFPS(MXNK,MXSIDE)
      REAL(8)::VFSM(MXNK,MXSIDE),VFSN(MXNK,MXSIDE)
      REAL(8)::VFSB(MXNK,MXSIDE),VFPS(MXNK,MXSIDE)
C
      REAL(8)::UFSM1(MXNK,MXSIDE),UFSN1(MXNK,MXSIDE)
      REAL(8)::UFSB1(MXNK,MXSIDE),UFPS1(MXNK,MXSIDE)
      REAL(8)::VFSM1(MXNK,MXSIDE),VFSN1(MXNK,MXSIDE)
      REAL(8)::VFSB1(MXNK,MXSIDE),VFPS1(MXNK,MXSIDE)
C
      REAL(8)::WXSM(MXSIDE),WXSN(MXSIDE)
      REAL(8)::WXSB(MXSIDE),WXPS(MXSIDE)
      REAL(8)::WYSM(MXSIDE),WYSN(MXSIDE)
      REAL(8)::WYSB(MXSIDE),WYPS(MXSIDE)
C
      REAL(8)::UFG(MXNK),VFG(MXNK)
C
      REAL(8)::COST,SINT
      REAL(8)::HHAD,WW,DH
      REAL(8)::FDX1,FDY1,MDZ1
      REAL(8)::FDX2,FDY2,MDZ2
      REAL(8)::FWX,FWY,MWZ
      REAL(8)::FX1,FY1,MZ1
      REAL(8)::FX2F,FY2F,MZ2F
      REAL(8)::FX,FY,FT,FA,AI2
      INTEGER::M,N,K,K1,K2,NA
cs 2014/03/25 honda
      REAL(8)::FRC
ce 2014/03/25 honda
      REAL(8),PARAMETER::GRAV=9.807D0
      REAL(8)::XRAND,YRAND,URAND,VRAND,DU,DV,DHSUM
      REAL(8)::KAPPA,UMEAN,VMEAN
c
CDEBUG      integer,save::icount=0
C
C
C----------------------------------------
C     N-LOOP(最外):START *** 漂流物の数だけ繰り返し
C----------------------------------------
      DO N=1,ND
cs 2014/03/25 honda
c         IF( LD(N).EQ.-1.OR.LD(N).EQ.-2 ) THEN    ! 地上に乗り上げている場合と領域外の場合はスキップ
         IF( LD(N).EQ.-2 ) THEN    ! 領域外の場合はスキップ
ce 2014/03/25 honda
            CYCLE
         ENDIF
C
         COST = COS(TD(N))
         SINT = SIN(TD(N))
C
         K1 = KAD(N)
         K2 = KAZ(N)
C
C ...... 辺上の点のリストを作成
         CALL XYSIDE(XSM,XSN,XSB,XPS,YSM,YSN,YSB,YPS,N)
C
C ...... 辺上の点と重心における流体速度を補間
         CALL INTERP_V(UFG,VFG,XD(N),YD(N),K1,K2,0,INAR(N)) ! 重心
C
         DO M=1,NDDIV(1,N)                          ! SBとPS上の点
            CALL INTERP_V(UFSB(1,M), VFSB(1,M), XSB(M),YSB(M),K1,K2,0,
     &                                                          INAR(N))
            CALL INTERP_V(UFPS(1,M), VFPS(1,M), XPS(M),YPS(M),K1,K2,0,
     &                                                          INAR(N))
            CALL INTERP_V(UFSB1(1,M),VFSB1(1,M),XSB(M),YSB(M),K1,K2,1,
     &                                                          INAR(N))
            CALL INTERP_V(UFPS1(1,M),VFPS1(1,M),XPS(M),YPS(M),K1,K2,1,
     &                                                          INAR(N))
         ENDDO
C
         DO M=1,NDDIV(2,N)                          ! SMとSN上の点
            CALL INTERP_V(UFSM(1,M), VFSM(1,M), XSM(M),YSM(M),K1,K2,0,
     &                                                          INAR(N))
            CALL INTERP_V(UFSN(1,M), VFSN(1,M), XSN(M),YSN(M),K1,K2,0,
     &                                                          INAR(N))
            CALL INTERP_V(UFSM1(1,M),VFSM1(1,M),XSM(M),YSM(M),K1,K2,1,
     &                                                          INAR(N))
            CALL INTERP_V(UFSN1(1,M),VFSN1(1,M),XSN(M),YSN(M),K1,K2,1,
     &                                                          INAR(N))
         ENDDO
C
C ...... 風応力計算用
         IF( WIN_CAL ) THEN
            DO M=1,NDDIV(1,N)                          ! SBとPS上の点
               CALL INTERP_WIN(WXSB(M),WYSB(M),XSB(M),YSB(M),INAR(N))
               CALL INTERP_WIN(WXPS(M),WYPS(M),XPS(M),YPS(M),INAR(N))
            ENDDO
C
            DO M=1,NDDIV(2,N)                          ! SMとSN上の点
               CALL INTERP_WIN(WXSM(M),WYSM(M),XSM(M),YSM(M),INAR(N))
               CALL INTERP_WIN(WXSN(M),WYSN(M),XSN(M),YSN(M),INAR(N))
            ENDDO
         ENDIF
C
C ...... 水深が浅い場合と深い場合の抗力の重み付け係数を設定
         HHAD = (HFD(N)-HTD(N))/AD0(N)
         IF(HHAD.GT.1.2D0) THEN
            WW = 0.05D0
         ELSE IF (HHAD.GE.1.0) THEN
            WW = 1.0D0 - 0.95D0/0.2D0*(HHAD-1.0D0)
         ELSE
c            WRITE(*,*) 'WARNING: HH/AD < 1 AT ',N
            WW = 1.0D0
         ENDIF
C
C ...... 力の初期化
         FX = 0.0D0
         FY = 0.0D0
         FT = 0.0D0
C----------------------------------------
C        K-LOOP(N-LOOPの内側):START *** 底面の高さから、水面の高さまで以下を繰り返す
C----------------------------------------
         DO K=K1,K2
C
C ......... 層の厚さ
            NA = INAR(N)
            DH = MIN(ZG(NA,K),HAZ(N)) - MAX(ZG(NA,K-1),HAD(N))
            IF( DH.LT.0.0D0 ) THEN
c               WRITE(*,*) 'WARNING AT DRIFT:DH<0'
c               CALL ERRMSG('DRIFT',1)
               DH=0.0D0 ! DH=0のとき、抗力、付加質量は0になる
            ENDIF
C
C----------------------------------------
C           (1) 抗力の計算
C----------------------------------------
C ......... 水深が深い場合に主となる抗力
            CALL DRAG1(FDX1,FDY1,MDZ1,COST,SINT,
     $                 UFSM,UFSN,UFSB,UFPS,VFSM,VFSN,VFSB,VFPS,
     $                 DH,K,N)
C
C ......... 水深が浅い場合に主となる抗力
            CALL DRAG2(FDX2,FDY2,MDZ2,COST,SINT,UFG,VFG,DH,K,N)
C
            FX1 = (1.0D0-WW)*FDX1 + WW*FDX2
            FY1 = (1.0D0-WW)*FDY1 + WW*FDY2
            MZ1 = (1.0D0-WW)*MDZ1 + WW*MDZ2
C
C----------------------------------------
C           (2) 付加質量の計算
C----------------------------------------
            CALL VMASS(FX2F,FY2F,MZ2F,COST,SINT,
     $                 UFSM,UFSN,UFSB,UFPS,VFSM,VFSN,VFSB,VFPS,
     $                 UFSM1,UFSN1,UFSB1,UFPS1,VFSM1,VFSN1,VFSB1,VFPS1,
     $                 DH,K,N)
C
C
C ......... 抗力と付加質量力の座標変換と足し合わせ
            FX = FX + (FX1+FX2F)*COST - (FY1+FY2F)*SINT
            FY = FY + (FX1+FX2F)*SINT + (FY1+FY2F)*COST
            FT = FT + MZ1 + MZ2F
CDEBUG            write(*,*) 'COS,SIN   = ',COST,SINT
CDEBUG            write(*,*) 'FDX1,FDX2 = ',FDX1,FDX2
CDEBUG            write(*,*) 'FDY1,FDY2 = ',FDY1,FDY2
CDEBUG            write(*,*) 'MDZ1,MDZ2 = ',MDZ1,MDZ2
CDEBUG            write(*,*) 'FX1,FX2F = ',FX1,FX2F
CDEBUG            write(*,*) 'FY1,FY2F = ',FY1,FY2F
CDEBUG            write(*,*) 'MZ1,MZ2F = ',MZ1,MZ2F
C
         ENDDO
C----------------------------------------
C        K-LOOP:END
C----------------------------------------
C
C----------------------------------------
C        (3) 風応力の計算
C----------------------------------------
         IF( WIN_CAL.AND.AZ1(N).GT.AD(N) ) THEN
            DH=AZ1(N)-AD(N)
            CALL DRAGWIN(FWX,FWY,MWZ,COST,SINT,
     $                   WXSM,WXSN,WXSB,WXPS,WYSM,WYSN,WYSB,WYPS,
     $                   DH,N)
C ......... 風応力の座標変換と足し合わせ
            FX = FX + FWX*COST - FWY*SINT
            FY = FY + FWX*SINT + FWY*COST
            FT = FT + MWZ
CDEBUG            write(*,*) 'FWX = ',FWX
CDEBUG            write(*,*) 'FWY = ',FWY
CDEBUG            write(*,*) 'MWZ = ',MWZ
         ENDIF
C
C
C----------------------------------------
C        (4) 不確実性モデルの計算
C----------------------------------------
         IF( LD(N).EQ.1 ) THEN
            IF( L_RAND==1 ) THEN
               NA = INAR(N)
               CALL SETKD(HTD(N),K1,INAR(N)) ! HTDの高さを含む流体セルのインデックスK1を定める
               K2=KFD(N)
               CALL INTERP_V(UFG,VFG,XD(N),YD(N),K1,K2,0,INAR(N)) ! 重心
c
               UMEAN = 0.0D0
               VMEAN = 0.0D0
               DH = 0.0D0
               DHSUM=0.0D0
C
               DO K=K1,K2
                  DH = MIN(ZG(NA,K),HFD(N)) - MAX(ZG(NA,K-1),HTD(N))
                  UMEAN = UMEAN + UFG(K) * DH
                  VMEAN = VMEAN + VFG(K) * DH
                  DHSUM=DHSUM+DH
               ENDDO
C
               DH = MAX(HFD(N)-HTD(N),1.0D-10)
               UMEAN = UMEAN / DH
               VMEAN = VMEAN / DH
               KAPPA = 0.064D0*SQRT(GRAV)*AMNG_RAND
     $               *SQRT(UMEAN**2+VMEAN**2)*DH**(5.D0/6.D0)
!
               CALL RANDOM_NUMBER(XRAND)
               CALL RANDOM_NUMBER(YRAND)
               URAND=SQRT(XRAND)*COS(2.D0*PI*YRAND)
               VRAND=SQRT(XRAND)*SIN(2.D0*PI*YRAND)
!               IF( N.EQ.1 ) WRITE(199,*) KAPPA,DH,DHSUM
               DU=SQRT(4.D0*KAPPA/DT)*URAND
               DV=SQRT(4.D0*KAPPA/DT)*VRAND
            ELSE
               DU=0.D0
               DV=0.D0
            ENDIF
         ENDIF
C
C
C----------------------------------------
C        (5) 速度、角速度、重心位置の更新
C----------------------------------------
         IF( LD(N).EQ.1 ) THEN
            AI2 = AM(N)*(AL(N)**2+AB(N)**2)/12.0D0
C
            UD1(N) = UD(N) + FX/(AM(N)+CM*AM(N))*DT
            VD1(N) = VD(N) + FY/(AM(N)+CM*AM(N))*DT
            OD1(N) = OD(N) + FT/(AI(N)+CM*AI2)*DT
CDEBUG            write(74,99) time,td(n)*180.0d0/PI,mdz1,mdz2,MZ2F,
CDEBUG     $         OD(N)*180.0d0/PI,OD1(N)*180.0d0/PI
CDEBUG   99       format(f7.2,f5.1,1pe12.4,e12.4,e12.4,0pf6.2,f6.2)
C
            XD1(N) = XD(N) + 0.5D0*(UD(N)+UD1(N))*DT + DU*DT
            YD1(N) = YD(N) + 0.5D0*(VD(N)+VD1(N))*DT + DV*DT
            TD1(N) = TD(N) + 0.5D0*(OD(N)+OD1(N))*DT
            TD1(N) = MOD(TD1(N),2.0D0*PI)
         ENDIF
C     
C----------------------------------------
C        (6) 係留時の処理
C----------------------------------------
         IF( LD(N).EQ.0 ) THEN
            UD1(N) = 0.0D0
            VD1(N) = 0.0D0
            OD1(N) = 0.0D0
C
            FA = SQRT(FX**2+FY**2)              ! 移動開始の判定
            IF( FA.GT.FMAX_INI(N) ) THEN
               WRITE(IFL,*) 'SHIP BEGIN TO MOVING DUE TO DRAG FORCE.'
               WRITE(IFL,*) '       ND,FORCE=',N,FA
cdel 20130423  LD(N) = 1
cadd 1line 20130423
               FMAX_INI(N)=-1.0D50
            ENDIF
         ENDIF
C
cs 2014/03/25 honda
C----------------------------------------
C        (7) 速度、角速度、重心位置の更新（乗り上げ時：摩擦考慮）
C----------------------------------------
         IF( LD(N).EQ.-1 ) THEN
            AI2 = AM(N)*(AL(N)**2+AB(N)**2)/12.0D0
            DH  = MIN(AD(N),MAX(0.0D0,HAZ(N)-HAD(N)))
            FRC = 0.65D0
     &            * (AM(N) - RHO * AL(N) * AB(N) * DH) * GRAV
ccc
            FRC = 10.0D0 * FRC
ccc
            FA  = SQRT(FX**2+FY**2)
C
            IF( SQRT(UD(N)**2+VD(N)**2).LE.1.0D-4 )THEN
              IF( FRC.GE.FA )THEN
                UD1(N) = 0.0D0
                VD1(N) = 0.0D0
                OD1(N) = 0.0D0
              ELSE
                FX = FX - FRC * FX / MAX(FA,1.D-6)
                FY = FY - FRC * FY / MAX(FA,1.D-6)
                UD1(N) = UD(N) + FX/(AM(N)+CM*AM(N)*DH/AD(N))*DT
                VD1(N) = VD(N) + FY/(AM(N)+CM*AM(N)*DH/AD(N))*DT
                OD1(N) = OD(N) + FT/(AI(N)+CM*AI2*DH/AD(N))*DT
              ENDIF
            ELSE
              FX = FX - FRC * UD(N) / SQRT(UD(N)**2+VD(N)**2)
              FY = FY - FRC * VD(N) / SQRT(UD(N)**2+VD(N)**2)
              UD1(N) = UD(N) + FX/(AM(N)+CM*AM(N)*DH/AD(N))*DT
              VD1(N) = VD(N) + FY/(AM(N)+CM*AM(N)*DH/AD(N))*DT
              OD1(N) = OD(N) + FT/(AI(N)+CM*AI2*DH/AD(N))*DT
              IF( UD1(N)*UD(N).LE.0.0D0 )THEN
                UD1(N) = 0.0D0
              ENDIF
              IF( VD1(N)*VD(N).LE.0.0D0 )THEN
                VD1(N) = 0.0D0
              ENDIF
              IF( SQRT(UD1(N)**2+VD1(N)**2).LE.1.0D-4 )THEN
                UD1(N) = 0.0D0
                VD1(N) = 0.0D0
                OD1(N) = 0.0D0
              ENDIF
            ENDIF
C
CDEBUG            write(74,99) time,td(n)*180.0d0/PI,mdz1,mdz2,MZ2F,
CDEBUG     $         OD(N)*180.0d0/PI,OD1(N)*180.0d0/PI
CDEBUG   99       format(f7.2,f5.1,1pe12.4,e12.4,e12.4,0pf6.2,f6.2)
C
            XD1(N) = XD(N) + 0.5D0*(UD(N)+UD1(N))*DT
            YD1(N) = YD(N) + 0.5D0*(VD(N)+VD1(N))*DT
            TD1(N) = TD(N) + 0.5D0*(OD(N)+OD1(N))*DT
            TD1(N) = MOD(TD1(N),2.0D0*PI)
         ENDIF
ce 2014/03/25 honda
C
      ENDDO
C----------------------------------------
C     N-LOOP:END
C----------------------------------------
C
      RETURN
      END
