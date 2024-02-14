      SUBROUTINE CLSURF(HH,KF,XC,YC,ZC,YCOS,GV,GX,GY,GZ,UU,VV,WW,
     $                  HU,HV,HW,PP,RHOW,HDEP,PATM,DPS,WX,WY,DZ,
     $                  INDU,INDV,INDP,KG,KP,NFL)
C======================================================================
C     水位を計算する、また水面における境界条件を設定する
C     具体的には以下の変数を更新する
C
C     HH: 水位(m)
C     KF: 水面セルのz方向セルインデックス
C
C     DZ: 水面移動、作業用配列
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'AREA.h'
C
      REAL(8),INTENT(INOUT)::HH(MX,MY)
C
      REAL(8),INTENT(INOUT)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::YCOS(MY)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GY(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HU(MX,MY,MZ),HV(MX,MY,MZ),HW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::PP(MX,MY,MZ),RHOW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::PATM(MX,MY),DPS(MX,MY),HDEP(MX,MY)
      REAL(8),INTENT(INOUT)::WX(MX,MY),WY(MX,MY)
      REAL(8),INTENT(INOUT)::DZ(MX,MY)
      INTEGER,INTENT(INOUT)::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KP(MX,MY),KG(MX,MY)
      INTEGER,INTENT(INOUT)::NFL
C
      INTEGER::IDB=1
C
      REAL(8)::GVZ,GVZ2,HH1,HH2,UT,VT,WT,VELIJK,DZ0
      INTEGER::I,J,K,M,N,IS,IE,JS,JE,KS,KE,IDIR
      INTEGER::II,IVMAX,JVMAX,KVMAX,IH1,KF1
      INTEGER::IUP,IDN,IERR,JERR,NERR
      INTEGER::NERR0=0
      SAVE NERR0
C
C
C----------------------------------------------------------------------
C     (1) 水面移動量DZを計算する
C----------------------------------------------------------------------
C
      DO 100 J=2,MYM
      DO 100 I=2,MXM
C ...... 水面のある領域のみ計算(水面セルで水がある)
         IF( KF(I,J).LT.MZ ) THEN
C ......... 水面移動量を計算
            DZ(I,J) = ( GZ(I,J,KG(I,J)-1)*WW(I,J,KG(I,J)-1)
     $              - ( HU(I,J,MZ)-HU(I-1,J,MZ) )*XC(6,I,J)
     $              - ( HV(I,J,MZ)-HV(I,J-1,MZ) )*YC(6,J)/YCOS(J) )* DT
         ELSE
            DZ(I,J) = 0.0D0
         END IF
  100 CONTINUE
C
cC ... 漂流物計算モデル
c      IF(MLNS.EQ.2) THEN
cC
c        IS = NDIJK(1)
c        IE = NDIJK(2)
c        JS = NDIJK(3)
c        JE = NDIJK(4)
c        DO 150 J=JS,JE
c        DO 150 I=IS,IE
cC ...... 水面のある領域のみ計算(水面セルで水がある)
c          IF( KF(I,J).LT.MZ ) THEN
cC ......... 水面移動量を計算
c            KS = KG(I,J)
c            KE = MAX(KF(I,J-1),KF(I-1,J),KF(I,J),KF(I+1,J),KF(I,J+1))
c            KE = MIN(KE,MZM)
c            DO 160 K=KS,KE
c              IF(INDP(I,J,K-1).EQ.0) THEN
c                DZ0 = GZ(I,J,K-1)*WW(I,J,K-1)*DT
c              END IF
c              DZ0 = DZ0-((HU(I,J,K)-HU(I-1,J,K))*XC(6,I,J)
c     $                  +(HV(I,J,K)-HV(I,J-1,K))*YC(6,J)/YCOS(J))
c     $            *ZC(4,K)*DT
c  160       CONTINUE
c            DZ(I,J) = DZ0                        
c          END IF
c  150   CONTINUE
c      END IF
C
C
C----------------------------------------------------------------------
C     (2) 潮位設定境界の水位変化をゼロにする
C----------------------------------------------------------------------
C
      DO 200 N=1,NOUTLT
         M  = MOUTLT(N)
         IS = IAREA(1,M)
         IE = IAREA(2,M)
         JS = IAREA(3,M)
         JE = IAREA(4,M)
         KS = IAREA(5,M)
         KE = IAREA(6,M)
         IDIR = IAREA(7,M)
         IH1 = IOUTLT(3,N)
C
         IF(IH1.NE.0) THEN
            IF( IDIR.EQ.1 ) THEN
               I = IS
               IF(KF(IS,JS).EQ.MZ) I=IS+1
               DO 210 J=JS,JE
                  DZ(I,J) = 0.0D0
  210          CONTINUE
C
            ELSE IF( IDIR.EQ.2 ) THEN
               J = JS
               IF(KF(IS,JS).EQ.MZ) J=JS+1
               DO 220 I=IS,IE
                  DZ(I,J) = 0.0D0
  220          CONTINUE
            END IF
         END IF
  200 CONTINUE
C
C
C----------------------------------------------------------------------
C     (3) 水位を更新する(水位セルと上下1メッシュ)
C----------------------------------------------------------------------
      IUP = 0
      IDN = 0
      IERR = 0
      JERR = 0
C
      DO 300 J=2,MYM
      DO 300 I=2,MXM
C ...... 水面のある領域のみ計算
         K = KF(I,J)
         IF( K.LT.MZ ) THEN
            GVZ = 0.5D0*(GZ(I,J,K)+GZ(I,J,K-1))
            HH1 = HH(I,J) + DZ(I,J)/GVZ
C
C ......... 水位が上に移動
            IF( HH1.GE.ZC(1,K) ) THEN
               DZ(I,J) = DZ(I,J) - ( ZC(1,K) - HH(I,J) )*GVZ
C
               IF( K+1.GT.MZM ) THEN
                  HH(I,J) = ZC(1,MZM) + DZ(I,J)
                  WRITE(LP,600) I,J,TIME,HH(I,J)
                  IERR = 1
               ELSE
                  GVZ2 = 0.5D0*(GZ(I,J,K)+GZ(I,J,K+1))
                  HH2 = ZC(1,K) + DZ(I,J)/GVZ2
                  IF( HH2.LT.ZC(1,K+1) ) THEN
                     DZ(I,J) = 0.0D0
                     HH(I,J) = HH2
                  ELSE
                     DZ(I,J) = DZ(I,J) - ZC(4,K+1)/GVZ2
                     IUP = 1
                     IF( K+1.EQ.MZM ) THEN
                        HH(I,J) = ZC(1,MZM) + DZ(I,J)
                        WRITE(LP,600) I,J,TIME,HH(I,J)
                        IERR = 1
                     END IF
                  END IF
               END IF
C
C ......... 水位の移動なし
            ELSE IF( HH1.GE.ZC(1,K-1) .OR. K.EQ.KG(I,J) ) THEN
               DZ(I,J) = 0.0D0
               IF( .NOT.(HH1.GE.HDEP(I,J)) ) THEN
                  JERR = JERR+1
                  HH1 = HDEP(I,J)+EPSH
               END IF
               HH(I,J) = HH1
C
C ......... 水位が下に移動
            ELSE
               DZ(I,J) = DZ(I,J) + ( HH(I,J) - ZC(1,K-1) )*GVZ
               GVZ2 = 0.5D0*(GZ(I,J,K-1)+GZ(I,J,K-2))
               HH2 = ZC(1,K-1) + DZ(I,J)/GVZ2
C
               IF( HH2.GE.ZC(1,K-2) .OR. K-1.EQ.KG(I,J) ) THEN
                  DZ(I,J) = 0.0D0
                  IF( .NOT.(HH2.GE.HDEP(I,J)) ) THEN
                     JERR = JERR+1
                     HH2 = HDEP(I,J)+EPSH
                  END IF
                  HH(I,J) = HH2
               ELSE
                  DZ(I,J) = DZ(I,J) + ZC(4,K-1)/GVZ2
                  IDN = 1
               END IF
            END IF
         END IF
  300 CONTINUE
C
C
C ... 水位が解析領域の上端を超えようとした場合、計算を終了する。
      IF( IERR.GT.0 ) THEN
         CALL ERRMSG('CLSURF',6880)
         DO 310 J=2,MYM
         DO 310 I=2,MXM
           IF( KF(I,J).LT.MZ) THEN
             IF(HH(I,J).GE.ZC(1,MZM) ) WRITE(LP,600) I,J,TIME,HH(I,J)
           END IF
  310    CONTINUE
         WRITE(LP,*) 'HH(I,J)='
         CALL DBWR2D(HH,3,1,MX,MY,MZ,LP)
         WRITE(LP,*) 'STOP HH OVER'
         CALL ABORT1('')
         RETURN
      END IF
C
C
C----------------------------------------------------------------------
C     (4) 水位を更新する(水位が2メッシュ以上移動)
C----------------------------------------------------------------------
      IF( IUP.GT.0.OR.IDN.GT.0 ) THEN
         IERR = 0
C
         DO 400 J=2,MYM
         DO 400 I=2,MXM
C
C ......... 水位が上に移動
            IF( DZ(I,J).GT.0.0D0 ) THEN
               DO 410 K=KF(I,J)+2,MZM
                  GVZ = 0.5D0*(GZ(I,J,K-1)+GZ(I,J,K))
                  HH1 = ZC(1,K-1) + DZ(I,J)/GVZ
C
                  IF( HH1.LT.ZC(1,K) ) THEN
                     DZ(I,J) = 0.0D0
                     HH(I,J) = HH1
                     GO TO 420
                  ELSE
                     DZ(I,J) = DZ(I,J) - ZC(4,K)/GVZ
                     IF( K.EQ.MZM ) THEN
                        HH(I,J) = ZC(1,MZM) + DZ(I,J)
                        IERR = 1
                     END IF
                  END IF
  410          CONTINUE
  420          CONTINUE
C
            ELSE IF( DZ(I,J) .LT. 0.0D0 ) THEN
               DO 450 K=KF(I,J)-2,2,-1
                  GVZ = 0.5D0*(GZ(I,J,K-1)+GZ(I,J,K))
                  HH1 = ZC(1,K) + DZ(I,J)/GVZ
C
                  IF( HH1.GE.ZC(1,K-1) .OR. K.EQ.KG(I,J) ) THEN
                     DZ(I,J) = 0.0D0
                     IF( .NOT.(HH1.GE.HDEP(I,J)) ) THEN
                        JERR = JERR+1
                        HH1 = HDEP(I,J)+EPSH
                     END IF
                     HH(I,J) = HH1
                     GO TO 460
                  ELSE
                     DZ(I,J) = DZ(I,J) + ZC(4,K)/GVZ
                  END IF
  450          CONTINUE
  460          CONTINUE
            END IF
  400    CONTINUE
C
C
C ...... 水位が解析領域の上端を超えようとした場合、計算を終了する。
         IF( IERR.GT.0 ) THEN
            CALL ERRMSG('CLSURF',6881)
            DO 480 J=2,MYM
            DO 480 I=2,MXM
              IF( KF(I,J).LT.MZ) THEN
                IF(HH(I,J).GE.ZC(1,MZM) ) WRITE(LP,600) I,J,TIME,HH(I,J)
              END IF
  600         FORMAT('WATER SURFACE IS OVER Z-RANGE  (I,J) =',2I4,
     $               '    TIME =',1PD10.3,'  HH=',1pd10.3)
  480       CONTINUE
            WRITE(LP,*) 'HH(I,J)='
            CALL DBWR2D(HH,3,1,MX,MY,MZ,LP)
            WRITE(LP,*) 'STOP HH OVER'
            CALL ABORT1('')
            RETURN
         END IF
      END IF
C
C
C ...... 水位が地面よりも下に下がろうとした場合、水位を地面位置として計算を続行する。
      IF( JERR.GT.0 .AND. NERR0.LT.1000 ) THEN
         NERR = 0
         DO 490 J=2,MYM
         DO 490 I=2,MXM
            IF( HH(I,J).EQ.HDEP(I,J)+EPSH .AND. NERR.LT.3 ) THEN
               DZ0 = ( GZ(I,J,KG(I,J)-1)*WW(I,J,KG(I,J)-1)
     $             - ( HU(I,J,MZ)-HU(I-1,J,MZ) )*XC(6,I,J)
     $             - ( HV(I,J,MZ)-HV(I,J-1,MZ) )*YC(6,J)/YCOS(J) )* DT
               IF( .NOT.(DZ0.GE.0.0D0) ) THEN
                  NERR = NERR + 1
                  NERR0 = NERR0 + 1
                  WRITE(LP,605) I,J
  605             FORMAT('DRYUP AT (I,J)=',2I5)
               END IF
            END IF
  490    CONTINUE
      END IF
C
C
      IF(NFL.NE.0) RETURN
C----------------------------------------------------------------------
C     以下、出力用
C----------------------------------------------------------------------
      ivmax=0
      jvmax=0
      kvmax=0
      VELMAX = 0.0D0
      DO 350 K=2,MZM
      DO 360 J=2,MYM
      DO 370 I=2,MXM
          IF(INDP(I,J,K).GT.0) THEN
            UT = 0.5D0*(UU(I-1,J,K)+UU(I,J,K))
            VT = 0.5D0*(VV(I,J-1,K)+VV(I,J,K))
            WT = 0.5D0*(WW(I,J,K-1)+WW(I,J,K))
            VELIJK = DSQRT(UT*UT+VT*VT+WT*WT)
            if(velijk.gt.velmax) then
               ivmax=i
               jvmax=j
               kvmax=k
               velmax = velijk
            endif
          END IF
  370 CONTINUE
  360 CONTINUE
  350 CONTINUE
C
      i=ivmax
      j=jvmax
      k=kvmax
      if(i.ne.0.and.j.ne.0.and.k.ne.0)
     $   write(6,21) time,velmax,i,j,k,uu(i-1,j,k),uu(i,j,k),
     $               vv(i,j-1,k),vv(i,j,k),ww(i,j,k-1),ww(i,j,k)
 21   format('time,velmax,i,j,k,u-,u+,v-,v+,w-,w+=',1p,2e10.3,
     $        0p,3i5,1p,6e10.3)
C
C ... 線流量をストアしておく(出力用 UU(*,*,MZ),VV(*,*,MZ) )
      DO 380 J=1,MY
      DO 380 I=1,MX
        UU(I,J,MZ) = HU(I,J,MZ)
        VV(I,J,MZ) = HV(I,J,MZ)
  380 CONTINUE
C
      RETURN
      END
