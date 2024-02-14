      SUBROUTINE GENERATE_DEBRIS(ROUGH,N_INUND,D_INUND,T_INUND,
     $                           T_MAXD,D_MAXD,IDST,TMDST)
!//////////////////////////////////////////////////
!     がれきを発生させてdrift.datに出力する
!     (配置できなかったものはdrift.errに出力する)
!//////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'CONTROL.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'GRID.h'
!
      REAL(8),INTENT(IN) :: ROUGH(MX,MY)   ! マニングの粗度[s]
      INTEGER,INTENT(IN) :: N_INUND        ! 浸水深の基準値の数
      REAL(8),INTENT(IN) :: D_INUND(N_INUND) ! 浸水深の基準値
      REAL(8),INTENT(IN) :: T_INUND(N_INUND,MX,MY)
!                                            d_inundの到達時刻[s]
      REAL(8),INTENT(IN) :: T_MAXD(MX,MY)  ! 最大浸水深の時刻[s]
      REAL(8),INTENT(IN) :: D_MAXD(MX,MY)  ! 最大浸水深[m]
!     (STOC-DS-MODE時)
      INTEGER,INTENT(IN) :: IDST(MX,MY)    ! 破壊フラグ
      REAL(8),INTENT(IN) :: TMDST(MX,MY)   ! 破壊時刻
!
      INTEGER,PARAMETER:: OUT1=61,OUT2=62
      INTEGER:: NDST      ! 破壊が発生したセルの数
      INTEGER:: NUM,NUME  ! drift.dat,drift.errへの出力数
      INTEGER:: I,J,M,N,II,JJ
      INTEGER,PARAMETER:: MAXD=100
      REAL(8):: DMX,P1,P2,D1,D2,T1,T2,XINUND
      REAL(8),PARAMETER:: EPS=1.D-3 ! 1msec
      REAL(8),PARAMETER:: EPD=1.D-3 ! 1mm
!
!     モデルの計算部で使用するローカル変数
      REAL(8),PARAMETER:: PI=3.14159265358979D0
      REAL(8):: X0,Y0       ! 計算メッシュの左下隅の座標[m]
      REAL(8):: X1,Y1       ! 計算メッシュの右下隅の座標[m]
      REAL(8):: DX,DY       ! メッシュサイズ[m]
      REAL(8):: ROUGHNESS_N ! マニングの粗度
      REAL(8):: R_BUIL      ! 建物占有率
      REAL(8):: AREA_BUIL   ! 建物占有面積[m2]
      REAL(8):: DMAXD       ! 最大浸水深[m]
      REAL(8):: PD          ! 建物被災率
      REAL(8):: RRAND       ! 乱数[0,1]
      REAL(8):: RN_DEBRIS   ! がれきの発生個数(実数)
      INTEGER:: N_DEBRIS    ! がれきの発生個数(整数)
      REAL(8):: TIME_DEBRIS ! がれきの発生時刻
      INTEGER:: IDS         ! STOC-DS-MODEでの破壊の有無
!                             (=0:なし、=1:破壊)
      REAL(8):: XD,YD       ! がれきの重心位置
      REAL(8):: LD          ! がれきの長さ[m]
      REAL(8):: BD          ! がれきの幅[m]
      REAL(8):: DD          ! がれきの喫水[m]
!
!     drift.dat出力用パラメータ
      CHARACTER(8),PARAMETER:: TYPED='gareki' ! がれきの文字列
      REAL(8),PARAMETER:: THTD=90.D0
!
!
!     乱数の初期化
      IF(CMODEL=='RANDOM') CALL INIT_RANDOM
!
!
!////////////////////////////////////////
!     drift.datをOPENしてから、
!     メッシュ毎にがれきを算定する
!////////////////////////////////////////
      OPEN(OUT1,FILE='drift.dat',FORM='FORMATTED',ACTION='WRITE')
      OPEN(OUT2,FILE='drift.err',FORM='FORMATTED',ACTION='WRITE')
      WRITE(OUT1,800)
      WRITE(OUT2,800)
!
      WRITE(LP,*) ''
      WRITE(LP,*) 'GENERATING DEBRIS ...'
      NDST=0
      NUM =0
      NUME=0
!
      DO J=2,MYM
      IF( MOD(J,100)==0 )
     $      WRITE(LP,'(3X,A2,I5,1X,A1,1X,I5)') 'J=',J,'/',MY
      DO I=2,MXM
!
         IDS=0
         IF( IDST(I,J)==-11 ) IDS=1
!
!     ////////////////////////////////////////
!     / (1) 建物占有率と占有面積を計算
!     ////////////////////////////////////////
         X0=XGRID(I-1)
         X1=XGRID(I)
         Y0=YGRID(J-1)
         Y1=YGRID(J)
         DX=X1-X0
         DY=Y1-Y0
         IF(ICORDTYPE==2) THEN ! 経緯度からm単位へ
            DX=DX*PI/180.D0*REARTH*COS(0.5D0*(Y0+Y1)*PI/180.D0)
            DY=DY*PI/180.D0*REARTH
         ENDIF
         ROUGHNESS_N=ROUGH(I,J)
!
         IF( ROUGHNESS_N<=0.03D0 ) THEN
            R_BUIL=0.D0
         ELSEIF( ROUGHNESS_N<=0.04D0 ) THEN
            R_BUIL=10.0D0*ROUGHNESS_N-0.30D0
         ELSEIF( ROUGHNESS_N<=0.06D0 ) THEN
            R_BUIL=12.5D0*ROUGHNESS_N-0.40D0
         ELSEIF( ROUGHNESS_N<=0.08D0 ) THEN
            R_BUIL=15.0D0*ROUGHNESS_N-0.55D0
         ELSE
            R_BUIL=0.65D0
         ENDIF
!
!        STOC-DS-MODEで破壊されている場合は占有率1
         IF(IDS==1) R_BUIL=1.D0
!
         AREA_BUIL=R_BUIL*DX*DY
!
!        建物なしの場合は以下の処理をスキップ
         IF(R_BUIL==0.D0) CYCLE
!
!
!     ////////////////////////////////////////
!     / (2) 建物被災率を計算
!     ////////////////////////////////////////
         DMAXD=D_MAXD(I,J)
!        [mm]以下の桁を四捨五入
         DMAXD=NINT(DMAXD*1000.D0)/1000.D0
!
         M=0
         DO N=1,N_TBL
            IF(DMAXD<=TBL_INUND(N)) THEN
               M=N
               EXIT
            ENDIF
         ENDDO
         IF(M==0) M=N_TBL+1
!
         IF(M==1) THEN
            PD=TBL_PDST(1)
         ELSEIF(M<=N_TBL) THEN
            PD=((TBL_INUND(M)-DMAXD)*TBL_PDST(M-1)
     $        + (DMAXD-TBL_INUND(M-1))*TBL_PDST(M))
     $        /(TBL_INUND(M)-TBL_INUND(M-1))
         ELSE
            PD=TBL_PDST(N_TBL)
         ENDIF
!
!        STOC-DS-MODEで破壊されている場合は被災率1
         IF(IDS==1) PD=1.D0
!
!        被災なしの場合は以下の処理をスキップ
         IF(PD==0.D0) CYCLE
!
!
!     ////////////////////////////////////////
!     / (3) 一様乱数を発生させ破壊判定
!     /     (MODEL=='RANDOM'の場合)
!     ////////////////////////////////////////
         IF(CMODEL=='RANDOM')THEN
            CALL RANDOM_NUMBER(RRAND)
!
!           破壊しない場合は以下の処理をスキップ
            IF( RRAND>=PD ) CYCLE
         ENDIF
         NDST=NDST+1
!
!
!     ////////////////////////////////////////
!     / (4) がれきを発生させて、ファイルに出力する
!     ////////////////////////////////////////
!        <<<がれきの発生数>>>
         RN_DEBRIS=QD*R_BUIL*DBLE(DIVX*DIVY)/(RHOD*HD)
         IF(CMODEL=='AVERAGE') RN_DEBRIS=RN_DEBRIS*PD
!        四捨五入により整数化
         N_DEBRIS=NINT(RN_DEBRIS)
!
!        <<<がれきの発生時刻>>>
         IF(IDS==1) THEN
            TIME_DEBRIS=TMDST(I,J)
!            
         ELSE
            DMX=MIN(DMAXD,G_INUND)
            DO N=1,N_INUND
               IF( DMX>D_INUND(N) ) THEN
                  EXIT
               ENDIF
            ENDDO
!
!           (FRAGILITYテーブルにしたがって発生:RANDOMモデルのみ)
            IF(G_INUND==0.D0.AND.CMODEL=='RANDOM') THEN
!              浸水基準値XINUNDをRRANDとTBL_PDSTの関係で決定する
               M=0
               DO N=2,N_TBL
                  IF(RRAND<=TBL_PDST(N)) THEN
                     P1=TBL_PDST(N-1)
                     P2=TBL_PDST(N)
                     D1=TBL_INUND(N-1)
                     D2=TBL_INUND(N)
                     EXIT
                  ENDIF
!
!                 念のためチェック
                  IF(N==N_TBL) THEN
                     CALL ERRMSG('GENERATE_DEBRIS',47)
                     WRITE(LP,*) '  UNEXPECTED ERROR'
                     WRITE(LP,*) '  TBL_PDST(N)=',TBL_PDST(N)
                     WRITE(LP,*) '  N=',N
                     WRITE(LP,*) '  RRAND=',RRAND
                     CALL ABORT1('')
                  ENDIF
               ENDDO
!
               XINUND=((P2-RRAND)*D1+(RRAND-P1)*D2)/(P2-P1)
!              (上記のDO分の処理上P2-P1=0となることはない)
!                 念のためチェック
               IF( XINUND<D1-EPD.OR.XINUND>D2+EPD ) THEN
                  CALL ERRMSG('GENERATE_DEBRIS',46)
                  WRITE(LP,*) 'UNEXPECTED INTERPOLATION ERROR'
                  WRITE(LP,*) 'P1,D1=',P1,D1
                  WRITE(LP,*) 'P2,D2=',P2,D2
                  WRITE(LP,*) 'XINUND=',XINUND
                  CALL ABORT1('')
               ENDIF
!
!           (入力値の基準浸水深に達した時刻に発生)
            ELSE
!              浸水基準値XINUNDは入力値とする
               XINUND=G_INUND
            ENDIF
!
            IF( DMAXD<=XINUND ) THEN
!           最大浸水深が基準値以下の場合、最大浸水深時刻に発生
               TIME_DEBRIS=T_MAXD(I,J)
!
            ELSE
!              XINUND<DMAXDの条件のもとで、
!              XINUNDに達した時刻を求める。
!              XINUNDを挟み込めるD1,D2を探して、
!              それに対応する時刻T1,T2の間で線形補間する
               DO N=2,N_INUND
                  IF(XINUND<D_INUND(N)) THEN
                     D1=D_INUND(N-1)
                     T1=T_INUND(N-1,I,J)
!
!                    (D_INUND(N-1)<=XINUND<D_INUND(N)  <DMAXD)
                     IF(D_INUND(N)<DMAXD) THEN
                        D2=D_INUND(N  )
                        T2=T_INUND(N  ,I,J)
!
!                    (D_INUND(N-1)<=XINUND<DMAXD  <D_INUND(N))
                     ELSE
                        D2=DMAXD
                        T2=T_MAXD(I,J)
                     ENDIF
!
                     EXIT
                  ENDIF
!
                  IF(N==N_INUND) THEN
!                 2点で挟み込めなかった場合、
!                 大きい側に最大浸水深を使う
!                 (D_INUND(N_INUND)<=XINUND<DMAXD)
                     D1=D_INUND(N)
                     D2=DMAXD
                     T1=T_INUND(N,I,J)
                     T2=T_MAXD(I,J)
                  ENDIF
               ENDDO
!
               IF(D2-D1<1.D-3) THEN
                  TIME_DEBRIS=T1
               ELSE
                  TIME_DEBRIS=((D2-XINUND)*T1+(XINUND-D1)*T2)/(D2-D1)
               ENDIF
!              念のためチェック
               IF( TIME_DEBRIS<T1-EPS.OR.TIME_DEBRIS>T2+EPS ) THEN
                  CALL ERRMSG('GENERATE_DEBRIS',43)
                  WRITE(LP,*) 'UNEXPECTED INTERPOLATION ERROR'
                  WRITE(LP,*) 'D1,T1=',D1,T1
                  WRITE(LP,*) 'D2,T2=',D2,T2
                  WRITE(LP,*) 'XINUND,TIME_DEBRIS=',
     $                         XINUND,TIME_DEBRIS
                  CALL ABORT1('')
               ENDIF
            ENDIF
         ENDIF
!
!        <<<がれきの発生位置とファイル出力>>>
         N=0
         LD=DY/DBLE(DIVY)
         BD=DX/DBLE(DIVX)
         DD=RHOD*HD/RHOL
!
         DO JJ=1,DIVY
         DO II=1,DIVX
            IF(N+1>N_DEBRIS) EXIT
!
!           重心座標
            XD=X0+(DBLE(II)-0.5D0)*BD
            YD=Y0+(DBLE(JJ)-0.5D0)*LD
!
            N=N+1
            NUM=NUM +1
            WRITE(OUT1,810) NUM,TYPED,XD,YD,THTD,
     $                      0.,0.,0.,
     $                      LD,BD,DD,0.D0,
     $                      LD,BD,DD,HD,HD,
     $                      -1.E+04,1.E+04,1.E+30,0.,0.,0.,0.,0.,
     $                      0.,0.,0.,0.,1.0,1.0,TIME_DEBRIS
  800       FORMAT('#    N   TYPE            XD          YD',
     $             '    TD   UD   VD   OD     AL     AB    ',
     $             'AD   AI     BL     BB    BD   AZ1   AZ2',
     $             '    HMIN   HMAX   FMAX ALP  CK  TS  TD ELIM',
     $             '  AB  AT ABD ATD  CQ  CD    TDST')
  810       FORMAT(I6.6,1X,A8,1X,F11.2,1X,F11.2,1X,F5.1,1X,
     $             F4.1,1X,F4.1,1X,F4.1,1X,
     $             F6.2,1X,F6.2,1X,F5.2,1X,F4.1,1X,
     $             F6.2,1X,F6.2,1X,F5.2,1X,F5.2,1X,F5.2,1X,
     $             1P,E7.0,E7.0,E7.0,0P,4F4.1,1X,F4.1,
     $             6F4.1,F8.1)
         ENDDO
         ENDDO
!
!        セル内に配置できなかったがれきの出力
         N=N_DEBRIS-DIVX*DIVY
         IF(N>0) THEN
!           重心位置はセル中心にしておく
            XD=0.5D0*(X0+X1)
            YD=0.5D0*(Y0+Y1)
            DO M=1,N
               NUME=NUME+1
               WRITE(OUT2,810) NUME,TYPED,XD,YD,THTD,
     $                         0.,0.,0.,
     $                         LD,BD,DD,0.D0,
     $                         LD,BD,DD,HD,HD,
     $                         -1.E+04,1.E+04,1.E+30,0.,0.,0.,0.,0.,
     $                         0.,0.,0.,0.,1.0,1.0,TIME_DEBRIS
            ENDDO
         ENDIF
      ENDDO
      ENDDO
!
      CLOSE(OUT1)
      IF( NUME==0 ) CLOSE(OUT2,STATUS='DELETE')
      IF( NUME>0  ) CLOSE(OUT2,STATUS='KEEP')
!
      WRITE(LP,*) 'DONE (GENERATING DEBRIS)'
      WRITE(LP,*) ''
!
      WRITE(LP,*) 'NUMBER OF SOURCE CELLS    =',NDST
      WRITE(LP,*) 'NUMBER OF PLACED   DEBRIS =',NUM
      WRITE(LP,*) 'NUMBER OF UNPLACED DEBRIS =',NUME
!
      RETURN
!
      END
