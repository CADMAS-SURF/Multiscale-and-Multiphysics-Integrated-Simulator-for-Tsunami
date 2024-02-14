      MODULE MOD_FAULT
C----------------------------------------------------------------------
C     水位変動量の計算を行うためのモジュール
C
C     含まれるサブルーチン
C
C     SET_PARAM_FAULT : 計算パラメータの初期設定を行う
C     SET_UTM         : UTM座標系の原点をセットする
C     EN2LB           : UTM及び19座標系の座標値を経緯度に変換する
C     SHIGOSEN        : 赤道からの子午線長を計算する
C     DISPLACE        : 断層パラメータから地盤変動量を計算する(港空研殿提供ルーチンに基づく)
C     USCAL           : DISPLACEから呼ばれる処理ルーチン1(港空研殿提供ルーチンに基づく)
C     UDCAL           : DISPLACEから呼ばれる処理ルーチン2(港空研殿提供ルーチンに基づく)
C
C     含まれる関数
C
C     ATN             : atan2を修正したもの(港空研殿提供ルーチンに基づく)
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER,PARAMETER:: NSOKU=3 ! 対応する測地系の数
C                                   1:旧日本測地系(BESSEL)
C                                   2:世界測地系(GRS80)
C                                   3:WGS84
C
      REAL(8):: PI          ! Π
      REAL(8):: D2R         ! 度からRADへの変換係数
      REAL(8):: R2D         ! RADから度への変換係数
      REAL(8):: FAI0        ! 初期値(垂足緯度(RAD))
      REAL(8):: EPSB        ! 収束判定値(垂足緯度)
C
      REAL(8):: ORI_L(0:19) ! UTM(0)及び19座標系の原点の経度(RAD)
      REAL(8):: ORI_B(0:19) ! UTM(0)及び19座標系の原点の緯度(RAD)
      REAL(8):: XADDUTM     ! UTM座標系で東方向の座標に加える距離(M)
C
      REAL(8):: RA(NSOKU)   ! 地球楕円体の長半径(M)
      REAL(8):: RF(NSOKU)   ! 地球楕円体の扁平率の逆数(-)
C
      REAL(8):: FF(NSOKU)   ! 地球楕円体の扁平率(-)
      REAL(8):: RB(NSOKU)   ! 地球楕円体の短半径(M)
      REAL(8):: E1(NSOKU)   ! 第1離心率
      REAL(8):: E2(NSOKU)   ! 第2離心率
      REAL(8):: E1S(NSOKU)  ! 第1離心率の2乗
      REAL(8):: E2S(NSOKU)  ! 第2離心率の2乗
      REAL(8):: C_SHIGO(6,NSOKU) ! 子午線長計算式の係数
C
      REAL(8):: DELTX(NSOKU,NSOKU) ! 測地系変換の平行移動パラメータ(M)
      REAL(8):: DELTY(NSOKU,NSOKU) ! 測地系変換の平行移動パラメータ(M)
      REAL(8):: DELTZ(NSOKU,NSOKU) ! 測地系変換の平行移動パラメータ(M)
C
      INTEGER,PARAMETER:: ISIZ_PARAM=10 ! 断層パラメータの配列サイズ1
      INTEGER:: NFLT                    ! 断層パラメータの配列サイズ2
      INTEGER:: NFLTNOW                 ! 断層パラメータの現在の参照位置
      REAL(8),ALLOCATABLE:: FPARAM(:,:) ! 断層パラメータ
      REAL(8):: XOR,YOR                 ! 断層計算の原点
C
      INTEGER:: ICOORD=0 ! 座標系の番号(=0:UTM,>0:19座標系の番号)
      INTEGER:: ISYSTEM=2 ! 計算メッシュの測地系の番号(1-3)
      INTEGER:: JSYSTEM=2 ! 断層パラメータの測地系の番号(1-3)
C
C
      CONTAINS
C
      SUBROUTINE SET_PARAM_FAULT
C----------------------------------------
C     計算パラメータの初期設定を行う
C----------------------------------------
      IMPLICIT NONE
C
      REAL(8):: E3,E9
      INTEGER:: N
C
      PI = 2.D0*ACOS(0.D0)
      D2R = PI/180.D0
      R2D = 180.D0/PI
      FAI0 = 40.D0*D2R          ! 40度
      EPSB = 2.D-5/3600.D0*D2R  ! 2E-5秒
C     
      ORI_L(1) = 129.D0+30.D0/60.D0 ! 19座標系
      ORI_B(1) = 33.D0
      ORI_L(2) = 131.D0
      ORI_B(2) = 33.D0
      ORI_L(3) = 132.D0+10.D0/60.D0
      ORI_B(3) = 36.D0
      ORI_L(4) = 133.D0+30.D0/60.D0
      ORI_B(4) = 33.D0
      ORI_L(5) = 134.D0+20.D0/60.D0
      ORI_B(5) = 36.D0
      ORI_L(6) = 136.D0
      ORI_B(6) = 36.D0
      ORI_L(7) = 137.D0+10.D0/60.D0
      ORI_B(7) = 36.D0
      ORI_L(8) = 138.D0+30.D0/60.D0
      ORI_B(8) = 36.D0
      ORI_L(9) = 139.D0+50.D0/60.D0
      ORI_B(9) = 36.D0
      ORI_L(10) = 140.D0+50.D0/60.D0
      ORI_B(10) = 40.D0
      ORI_L(11) = 140.D0+15.D0/60.D0
      ORI_B(11) = 44.D0
      ORI_L(12) = 142.D0+15.D0/60.D0
      ORI_B(12) = 44.D0
      ORI_L(13) = 144.D0+15.D0/60.D0
      ORI_B(13) = 44.D0
      ORI_L(14) = 142.D0
      ORI_B(14) = 26.D0
      ORI_L(15) = 127.D0+30.D0/60.D0
      ORI_B(15) = 26.D0
      ORI_L(16) = 124.D0
      ORI_B(16) = 26.D0
      ORI_L(17) = 131.D0
      ORI_B(17) = 26.D0
      ORI_L(18) = 136.D0
      ORI_B(18) = 20.D0
      ORI_L(19) = 154.D0
      ORI_B(19) = 26.D0
      DO N=1,19 ! 度 -> RAD
         ORI_L(N)=ORI_L(N)*D2R
         ORI_B(N)=ORI_B(N)*D2R
      ENDDO
      XADDUTM = 500.D3
C     
      WRITE(*,*) 'DEBUG INFO (AT SET_PARAM_FAULT)'
C     
C ... Bessel
      RA(1)=6377.397155D3
      RF(1)=299.152813D0
C ... GR80
      RA(2)=6378.137D3
      RF(2)=298.257222101D0
C ... WGS84
      RA(3)=6378.137D3
      RF(3)=298.257223563D0
C     
      DO N=1,NSOKU
         FF(N)=1.D0/RF(N)
         RB(N)=RA(N)*(1.D0-FF(N))
         E1S(N)=FF(N)*(2.D0-FF(N))
         E2S(N)=E1S(N)/(1.D0-FF(N))**2
         E1(N)=SQRT(E1S(N))
         E2(N)=SQRT(E2S(N))
C     
         IF( N==1 ) WRITE(*,*) 'BESSEL'
         IF( N==2 ) WRITE(*,*) 'GRS80'
         IF( N==3 ) WRITE(*,*) 'WGS84'
         WRITE(*,*) ' <A>   =',RA(N)
         WRITE(*,*) '  B    =',RB(N)
         WRITE(*,*) ' <1/F> =',RF(N)
         WRITE(*,*) '  F    =',FF(N)
         WRITE(*,*) '  E    =',E1(N)
         WRITE(*,*) '  E2   =',E2(N)
         WRITE(*,*) '  E**2 =',E1S(N)
         WRITE(*,*) '  E2**2=',E2S(N)
C     
         E9=RA(N)*(1.D0-E1S(N))
         C_SHIGO(1,N)=E9*( 1.D0 + 3.D0/4.D0*E1S(N)
     $      + 45.D0/64.D0*E1S(N)**2 + 175.D0/256.D0*E1S(N)**3
     $      + 11025.D0/16384.D0*E1S(N)**4
     $      + 43659.D0/65536.D0*E1S(N)**5 )
         C_SHIGO(2,N)=-E9/2.D0*( 3.D0/4.D0*E1S(N)
     $      + 15.D0/16.D0*E1S(N)**2 + 525.D0/512.D0*E1S(N)**3
     $      + 2205.D0/2048.D0*E1S(N)**4
     $      + 72765.D0/65536.D0*E1S(N)**5 )
         C_SHIGO(3,N)=E9/4.D0*( 15.D0/64.D0*E1S(N)**2
     $      + 105.D0/256.D0*E1S(N)**3 + 2205.D0/4096.D0*E1S(N)**4
     $      + 10395.D0/16384.D0*E1S(N)**5 )
         C_SHIGO(4,N)=-E9/6.D0*( 35.D0/512.D0*E1S(N)**3
     $      + 315.D0/2048.D0*E1S(N)**4
     $      + 31185.D0/131072.D0*E1S(N)**5 )
         C_SHIGO(5,N)=E9/8.D0*( 315.D0/16384.D0*E1S(N)**4
     $      + 3465.D0/65536.D0*E1S(N)**5 )
         C_SHIGO(6,N)=-E9/10.D0*( 693.D0/131072.D0*E1S(N)**5 )
      ENDDO
C     
C     BESSEL→GRS80 ! 東京大正一等三角点における変換パラメータ
      DELTX(1,2)=-146.414D0
      DELTY(1,2)= 507.337D0
      DELTZ(1,2)= 680.507D0
      DELTX(2,1)=-DELTX(1,2)
      DELTY(2,1)=-DELTY(1,2)
      DELTZ(2,1)=-DELTZ(1,2)
C     GRS80→WGS84 ! 飛田幹男 "世界測地系と座標変換",PP.38より
      DELTX(2,3)= 0.0780D0
      DELTY(2,3)=-0.5050D0
      DELTZ(2,3)=-0.2530D0
      DELTX(3,2)=-DELTX(2,3)
      DELTY(3,2)=-DELTY(2,3)
      DELTZ(3,2)=-DELTZ(2,3)
C     BESSEL→WGS84 (1,2)+(2,3)
      DELTX(1,3)= DELTX(1,2)+DELTX(2,3)
      DELTY(1,3)= DELTY(1,2)+DELTY(2,3)
      DELTZ(1,3)= DELTZ(1,2)+DELTZ(2,3)
      DELTX(3,1)=-DELTX(1,3)
      DELTY(3,1)=-DELTY(1,3)
      DELTZ(3,1)=-DELTZ(1,3)
C     
      RETURN
      END SUBROUTINE SET_PARAM_FAULT
C     
C     
      SUBROUTINE SET_UTM(LC_DEG)
C----------------------------------------
C     UTM座標系の原点をセットする
C
C     <INPUT>
C     LC_DEG ! UTM座標系の中央経線の経度(度)
C----------------------------------------
      IMPLICIT NONE
      REAL(8),INTENT(IN):: LC_DEG
C
      PI = 2.D0*ACOS(0.D0)
      D2R = PI/180.D0
      ORI_L(0) = LC_DEG*D2R
      ORI_B(0) = 0.D0
C
      RETURN
      END SUBROUTINE SET_UTM
C
C
      SUBROUTINE EN2LB(XE,YN,L,B,IG,ID)
C----------------------------------------
C     UTM及び19座標系の座標値を経緯度に変換する
C     (参考文献: 数値地図ユーザーガイド PP.471)
C
C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     ! 19座標系の本来の定義ではXEがYで、YNがXとなる !
C     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C     <INPUT>
C     XE:東西方向の座標値(M)
C     YN:南北方向の座標値(M)
C     IG:座標系の番号(=0:UTM,>1:19座標系)
C     ID:測地系 (=1:旧日本測地系(BESSEL),=2:世界測地系(GRS80))
C     <OUTPUT>
C     L:経度(RAD)
C     B:緯度(RAD)
C----------------------------------------
      IMPLICIT NONE
      REAL(8),INTENT(IN) :: XE,YN
      REAL(8),INTENT(OUT):: L,B
      INTEGER,INTENT(IN) :: IG,ID
C
      REAL(8)::P,Q,FAI1,FAI2,SHIGO0,SHIGO1,PS,PQ
      REAL(8)::T1,T1S,G1S,A1,C1,S1,A1S
      INTEGER::N
C
      IF( IG==0 ) THEN ! UTM
         P=(XE-XADDUTM)/0.9996D0
         Q=YN/0.9996D0
C         WRITE(*,*) 'P,Q=',P,Q
      ELSE
         P=XE/0.9999D0
         Q=YN/0.9999D0
      ENDIF
      CALL SHIGOSEN(ORI_B(IG),SHIGO0,ID)
C
C     NEWTON法(3回程度で収束)
      FAI1=FAI0
      DO N=1,10
         CALL SHIGOSEN(FAI1,SHIGO1,ID)
         FAI2=FAI1-(SHIGO1-SHIGO0-Q)/(RA(ID)*(1.D0-E1S(ID)))*
     $      (1.D0-E1S(ID)*SIN(FAI1)**2)**1.5D0
C
         IF( ABS(FAI2-FAI1)<EPSB ) THEN
            FAI1=FAI2
            EXIT
         ELSEIF( N==10 ) THEN
            WRITE(*,*) 'ERROR: NEWTON METHOD NOT CONVERGED'
            WRITE(*,*) '       IN SUBROUTINE EN2LB'
            STOP
         ENDIF
C
         FAI1=FAI2
      ENDDO
C
      PS=P**2
      PQ=PS**2
C
      C1=COS(FAI1)
      S1=SIN(FAI1)
      T1=S1/C1
      T1S=T1**2
      G1S=E2S(ID)*C1**2
      A1=RA(ID)/SQRT(1.D0-E1S(ID)*S1**2)
      A1S=A1**2
C
      B=FAI1-PS*(1.D0+G1S)*T1/(2.D0*A1S)*
     $   (1.D0-PS/(12.D0*A1S)*(5.D0+3.D0*T1S+G1S-9.D0*T1S*G1S)+
     $   PQ/(360.D0*A1S**2)*(61.D0+90.D0*T1S+45.D0*T1S**2))
      L=ORI_L(IG)+P/(A1*C1)*(1.D0-PS/(6.D0*A1S)*(1.D0+2.D0*T1S+G1S)+
     $   PQ/(120.D0*A1S**2)*(5.D0+28.D0*T1S+24.D0*T1S**2))
C
      RETURN
      END SUBROUTINE EN2LB
C
C
      SUBROUTINE LB2LB(L1,B1,L2,B2,ID1,ID2)
C----------------------------------------
C     経緯度の測地系を変換する
C
C     <INPUT>
C     L1:変換前の経度(RAD)
C     B1:変換前の緯度(RAD)
C     ID1:変換前の測地系 (=1:旧日本測地系,=2:世界測地系)
C     ID2:変換後の測地系 (=1:旧日本測地系,=2:世界測地系)
C     <OUTPUT>
C     L2:変換後の経度(RAD)
C     B2:変換後の緯度(RAD)
C----------------------------------------
      IMPLICIT NONE
      REAL(8),INTENT(IN) :: L1,B1
      REAL(8),INTENT(OUT):: L2,B2
      INTEGER,INTENT(IN) :: ID1,ID2
C
      REAL(8)::S1,C1,S2,C2,A1,H1,X,Y,Z,P1,R1,M1
      INTEGER::ID
C
C     楕円体から直交座標
      ID=ID1
      S1=SIN(B1)
      C1=COS(B1)
      S2=SIN(L1)
      C2=COS(L1)
      A1=RA(ID)/SQRT(1.D0-E1S(ID)*S1**2)
      H1=0.D0 ! 経緯度のみの変換の場合、高さによる誤差は僅少
      X=(A1+H1)*C1*C2
      Y=(A1+H1)*C1*S2
      Z=(A1*(1.D0-E1S(ID))+H1)*S1
C
C     直交座標の平行移動
      X=X+DELTX(ID1,ID2)
      Y=Y+DELTY(ID1,ID2)
      Z=Z+DELTZ(ID1,ID2)
C
C     直交座標から楕円体
      ID=ID2
      P1=SQRT(X**2+Y**2)
      R1=SQRT(P1**2+Z**2)
      M1=ATAN2(Z*((1.D0-FF(ID))+E1S(ID)*RA(ID)/R1),P1)
      S1=SIN(M1)
      C1=COS(M1)
      B2=ATAN2(Z*(1.D0-FF(ID))+E1S(ID)*RA(ID)*S1**3,
     $         (1.D0-FF(ID))*(P1-E1S(ID)*RA(ID)*C1**3))
      L2=ATAN2(Y,X)
C
      RETURN
      END SUBROUTINE LB2LB
C
C
      SUBROUTINE SHIGOSEN(FAI,B,ID)
C----------------------------------------
C     赤道からの子午線長を計算する
C
C     <INPUT>
C     FAI: 緯度
C     ID:測地系 (=0:旧日本測地系,=1:世界測地系)
C     <OUTPUT>
C     B: 赤道からの子午線長
C----------------------------------------
      IMPLICIT NONE
      REAL(8),INTENT(IN) :: FAI
      REAL(8),INTENT(OUT):: B
      INTEGER,INTENT(IN) :: ID
C
      REAL(8)::COSF,SINF,SINF1,SINF2
      INTEGER::N
C
      COSF=COS(2.D0*FAI)
      SINF=SIN(2.D0*FAI)
      SINF1=0.D0
      B=C_SHIGO(1,ID)*FAI + C_SHIGO(2,ID)*SINF
      DO N=3,6
         SINF2=SINF1
         SINF1=SINF
         SINF =2.D0*COSF*SINF1-SINF2
         B=B+C_SHIGO(N,ID)*SINF
      ENDDO
C
      RETURN
      END SUBROUTINE SHIGOSEN
C
C
      SUBROUTINE DISPLACE(XL,YL,XOR,PARAM,PARAM2,ZZ)
C--------------------------------------------------
C     断層パラメータから地盤変動量を計算する
C
C     <input>
C      XL,YL: 座標 (rad)
C      XOR  : (1,1)のX座標 (rad)
C      PARAM: 断層パラメータ
C        1:FL    : FAULT LENGTH (M)
C        2:FW    : FAULT WIDTH (M)
C        3:HH    : DEPTH OF UPPER FAULT (M)
C        4:STR   : STRIKE ANGLE (rad)
C        5:DIP   : DIP ANGLE (rad)
C        6:SLIP  : SLIP ANGLE (rad)
C        7:DISL  : TOTAL DISLOCATION (M)
C        8:Y0    : LOCATION OF FAULT
C        9:X0    : LOCATION OF FAULT
C     <output>
C      ZZ: 地盤変動量(m) (正が隆起、負が沈降)
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      REAL(8),INTENT(IN)::XL,YL,XOR,PARAM(10),PARAM2(6)
      REAL(8),INTENT(OUT)::ZZ
C
      REAL(8),PARAMETER:: REARTH = 6.37D+6 ! RADIUS OF EARTH (M)
      REAL(8):: HH,DISL,STR,DIP,SLIP,FL,FW,Y0,X0
      REAL(8):: SIND,COSD,TAND,SINP,COSP,SINS,COSS
      REAL(8):: FL2,H1,H2,DS,DD,XDL,YDL,X1,X2,X3
      REAL(8):: F1,F2,F3,F4,G1,G2,G3,G4,US,UD
C
C
      FL  =PARAM(1)
      FW  =PARAM(2)
      HH  =PARAM(3)
      STR =PARAM(4) ! rad
      DIP =PARAM(5) ! rad
      SLIP=PARAM(6) ! rad
      DISL=PARAM(7)
      Y0  =PARAM(8) ! rad
      X0  =PARAM(9) ! rad
!      XOR ! rad
!      YL  ! rad
!      XL  ! rad
      SIND=PARAM2(1) ! SIN(DIP)
      COSD=PARAM2(2) ! COS(DIP)
      TAND=SIND/COSD
      SINP=PARAM2(3) ! SIN(SLIP)
      COSP=PARAM2(4) ! COS(SLIP)
      SINS=PARAM2(5) ! SIN(STR)
      COSS=PARAM2(6) ! COS(STR)
      FL2=0.5D0*FL
C
      H1  = HH/SIND
      H2  = HH/SIND+FW
      DS  =  DISL*COSP
      DD  = -DISL*SINP
C
      XDL = REARTH*((XL-XOR)*COS(YL)-(X0-XOR)*COS(Y0))
      YDL = REARTH*(YL-Y0)
      X1  = XDL*SINS+YDL*COSS-FL2
      X2  = XDL*COSS-YDL*SINS+HH/TAND
      X3  = 0.D0
! for strike slip
      CALL USCAL (X1,X2,X3, FL2,H2,SIND,COSD,TAND,F1)
      CALL USCAL (X1,X2,X3, FL2,H1,SIND,COSD,TAND,F2)
      CALL USCAL (X1,X2,X3,-FL2,H2,SIND,COSD,TAND,F3)
      CALL USCAL (X1,X2,X3,-FL2,H1,SIND,COSD,TAND,F4)
! for dip slip
      CALL UDCAL (X1,X2,X3, FL2,H2,SIND,COSD,G1)
      CALL UDCAL (X1,X2,X3, FL2,H1,SIND,COSD,G2)
      CALL UDCAL (X1,X2,X3,-FL2,H2,SIND,COSD,G3)
      CALL UDCAL (X1,X2,X3,-FL2,H1,SIND,COSD,G4)
      US = (F1-F2-F3+F4)*DS/(12.D0*PI)
      UD = (G1-G2-G3+G4)*DD/(12.D0*PI)
      ZZ = - (US+UD)
C
      RETURN
      END SUBROUTINE DISPLACE
C
C
      SUBROUTINE USCAL (X1,X2,X3,C,CC,SN,CS,TN,F)
C--------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      C1 = C
      C2 = CC*CS
      C3 = CC*SN
      R = SQRT((X1-C1)**2+(X2-C2)**2+(X3-C3)**2)
      Q = SQRT((X1-C1)**2+(X2-C2)**2+(X3+C3)**2)
      R2 = X2*SN-X3*CS
      R3 = X2*CS+X3*SN
      Q2 = X2*SN+X3*CS
      Q3 = -X2*CS+X3*SN
      A1 = LOG(R+R3-CC)
      A2 = LOG(Q+Q3+CC)
      A3 = LOG(Q+X3+C3)
      B1 = 1.D0+3.D0*TN**2
      B2 = 3.D0*TN/CS
      B3 = 2.D0*R2*SN
      B4 = Q2+X2*SN
      B5 = 2.D0*R2**2*CS
      B6 = R*(R+R3-CC)
      B7 = 4.D0*Q2*X3*SN**2
      B8 = 2.D0*(Q2+X2*SN)*(X3+Q3*SN)
      B9 = Q*(Q+Q3+CC)
      B10 = 4.D0*Q2*X3*SN
      B11 = (X3+C3)-Q3*SN
      B12 = 4.D0*Q2**2*Q3*X3*CS*SN
      B13 = 2.D0*Q+Q3+CC
      B14 = Q**3*(Q+Q3+CC)**2
      F = CS*(A1+B1*A2-B2*A3)
     $ +B3/R+2.D0*SN*B4/Q-B5/B6+(B7-B8)/B9
     $ +B10*B11/Q**3-B12*B13/B14
C
      RETURN
      END SUBROUTINE USCAL
C
C
      SUBROUTINE UDCAL (X1,X2,X3,C,CC,SN,CS,F)
C--------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      C1 = C
      C2 = CC*CS
      C3 = CC*SN
      R = SQRT((X1-C1)**2+(X2-C2)**2+(X3-C3)**2)
      Q = SQRT((X1-C1)**2+(X2-C2)**2+(X3+C3)**2)
      R2 = X2*SN-X3*CS
      R3 = X2*CS+X3*SN
      Q2 = X2*SN+X3*CS
      Q3 = -X2*CS+X3*SN
      H = SQRT(Q2**2+(Q3+CC)**2)
      A1 = LOG(R+X1-C1)
      A2 = LOG(Q+X1-C1)
      B1 = Q*(Q+X1-C1)
      B2 = R*(R+X1-C1)
      B3 = Q*(Q+Q3+CC)
      D1 = X1-C1
      D2 = X2-C2
      D3 = X3-C3
      D4 = X3+C3
      D5 = R3-CC
      D6 = Q3+CC
      T1 = ATN(D1*D2,(H+D4)*(Q+H))
      T2 = ATN(D1*D5,R2*R)
      T3 = ATN(D1*D6,Q2*Q)
      F = SN*(D2*(2.D0*D3/B2+4.D0*D3/B1-4.D0*C3*X3*D4
     $   *(2.D0*Q+D1)/(B1**2*Q))
     $   -6.D0*T1+3.D0*T2-6.D0*T3)
     $   +CS*(A1-A2-2.D0*(D3**2)/B2
     $   -4.D0*(D4**2-C3*X3)/B1-4.D0*C3*X3*D4**2
     $   *(2.D0*Q+X1-C1)/(B1**2*Q))
     $   +6.D0*X3*(CS*SN*(2.D0*D6/B1+D1/B3)-Q2*(SN**2-CS**2)/B1)
C
      RETURN
      END SUBROUTINE UDCAL
C
C
      DOUBLE PRECISION FUNCTION ATN ( AX, AY )
C----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (GX=1.0D-6)
C
      AAX = ABS(AX)
      AAY = ABS(AY)
      P = AX*AY
      IF( AAX.LE.GX .AND. AAY.LE.GX ) GOTO 10
      SR = ATAN2(AAX,AAY)
      ATN = SIGN(SR,P)
      RETURN
C
   10 WRITE(6,100) AX, AY
  100 FORMAT(1H ,'ATAN --    AX=',E15.7,2X,'AY=',E15.7)
      ATN = 0.2D0
C
      RETURN
      END FUNCTION ATN
C
      END MODULE MOD_FAULT
