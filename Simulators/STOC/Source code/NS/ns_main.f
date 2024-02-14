      PROGRAM NS_MAIN
C======================================================================
C     (1) コモン変数の初期化
C     (2) 入力データの読み込み
C     (3) 格子定数の設定
C     (4) インデックス及び壁面処理用リストの作成
C     (5) ポロシティの設定
C     (6) 初期条件の設定
C     (7) 時間積分計算
C     (8) 計算の終了
C======================================================================
      use mod_comm,only: init_mpmd,comm_model
      use mod_list,only: ALLOC_LIST,ALLOC_LIST2,
     $                   LLWALL,LLWALP,LLWALB,LLOFL,HHOFL
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'FILEI.h'
      INCLUDE 'CPUCHK.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'OBSTI.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'INITL.h'
      INCLUDE 'GLOBAL.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'SEDIMENT.h'
      INCLUDE 'CADMAS.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'mpif.h'
      INCLUDE 'OUTPUT.h'
C
      INTEGER,ALLOCATABLE::
     $   INDP(:,:,:),INDU(:,:,:),INDV(:,:,:),INDW(:,:,:),INDK(:,:,:),
     $   IWRK1(:,:,:),IWRK2(:,:,:),IWRK3(:,:,:)
      INTEGER,ALLOCATABLE::
     $   KF(:,:),KG(:,:),KP(:,:),KH(:,:)
      REAL(8),ALLOCATABLE::
     $   XC(:,:,:),XCP(:,:,:),YC(:,:),ZC(:,:),
     $   XC_REF(:,:),YC_REF(:,:),
     $   YCOS(:),YCOSP(:),YSIN(:),YSINP(:),
     $   HH(:,:),HX(:,:),HY(:,:),
     $   PATM(:,:),WX(:,:),WY(:,:),DPS(:,:),QQ(:,:,:),QW(:,:),
     $   HDEP(:,:),AMNG(:,:),HHW(:,:),CD(:,:)
      REAL(8),ALLOCATABLE::
     $   GV0(:,:,:),GX0(:,:,:),GY0(:,:,:),GZ0(:,:,:),
     $   GV(:,:,:),GX(:,:,:),GY(:,:,:),GZ(:,:,:),
     $   GVD(:,:,:),GXD(:,:,:),GYD(:,:,:),GZD(:,:,:),
     $   CMD(:,:,:),CDD(:,:,:),COE1D(:,:,:),COE2D(:,:,:),
     $   UU(:,:,:),VV(:,:,:),WW(:,:,:),PP(:,:,:),
     $   HU(:,:,:),HV(:,:,:),HW(:,:,:),
     $   TT(:,:,:),CC(:,:,:),AK(:,:,:),EP(:,:,:),RL(:,:,:),
     $   TMU(:,:,:),FF(:,:,:),RHOW(:,:,:),VLEND(:,:,:),
     $   TMEND(:,:,:),FREND(:,:,:),FRIC(:,:,:),RMMB(:,:,:),RMMF(:,:,:)
      REAL(8),ALLOCATABLE::TMUBW(:,:),TIMBW(:,:)
      REAL(8),ALLOCATABLE::
     $   UN(:,:,:),VN(:,:,:),WN(:,:,:),TN(:,:,:),CN(:,:,:),AKN(:,:,:),
     $   EPN(:,:,:),AD0(:,:,:),AD(:,:,:),BB(:,:,:),DP(:,:,:),
     $   WRK1(:,:,:),WRK2(:,:,:),WRK3(:,:,:),WRK4(:,:,:),
     $   WRK5(:,:,:),WRK6(:,:,:),WRK7(:,:,:),WRK8(:,:,:)
      REAL(8),ALLOCATABLE::
     $   AL0(:,:,:,:),AL(:,:,:,:),AU(:,:,:,:)
      INTEGER,ALLOCATABLE::
     $   IBUF(:)
      INTEGER,ALLOCATABLE::
     $   I_ML(:,:),J_ML(:,:),K_ML(:,:),
     $   I_NS(:,:),J_NS(:,:),K_NS(:,:),
     $   KG_ML(:,:),KF_ML(:,:)
      INTEGER,ALLOCATABLE::
     $   INDU_ML(:,:,:),INDV_ML(:,:,:),INDW_ML(:,:,:),INDP_ML(:,:,:)
      REAL(8),ALLOCATABLE::
     $   BUF(:)
      REAL(8),ALLOCATABLE::
     $   XC_ML(:,:),YC_ML(:,:),ZC_ML(:,:),
     $   HH_ML(:,:),HDEP_ML(:,:),HHBCN(:,:)
      REAL(8),ALLOCATABLE::
     $   GX_ML(:,:,:),GY_ML(:,:,:),GZ_ML(:,:,:),
     $   UU_ML(:,:,:),VV_ML(:,:,:),WW_ML(:,:,:),
     $   TT_ML(:,:,:),CC_ML(:,:,:),AK_ML(:,:,:),EP_ML(:,:,:),
     $   UUBCN(:,:,:),VVBCN(:,:,:),WWBCN(:,:,:),
     &   TTBCN(:,:,:),CCBCN(:,:,:),AKBCN(:,:,:),EPBCN(:,:,:)
C
C ... 地形変化モデル用
      REAL(8),ALLOCATABLE::
     $   CSEDI(:,:,:),CSEDIN(:,:,:),CSDAVE(:,:),SHLSD(:,:),USSD(:,:),
     $   WEXSD(:,:),EXSDE(:,:),EXSDD(:,:),
     $   ZBED(:,:),ZBEDN(:,:),ZBED0(:,:),QBX(:,:),QBY(:,:),DZBUF(:,:),
     $   CSD_ML(:,:,:),ZBD_ML(:,:),CSDBCN(:,:,:),ZBDBCN(:,:)
      REAL(8),ALLOCATABLE::GXBDH(:,:),GYBDH(:,:)
      INTEGER,ALLOCATABLE::KIBDH(:,:),KJBDH(:,:)
C
C ... 風場計算用
      REAL(8),ALLOCATABLE:: ZCA(:,:)
      INTEGER,ALLOCATABLE:: INDPA(:,:,:),INDUA(:,:,:)
      INTEGER,ALLOCATABLE:: INDVA(:,:,:),INDWA(:,:,:)
      INTEGER,ALLOCATABLE:: KFA(:,:),KFNA(:,:)
      REAL(8),ALLOCATABLE:: GVA(:,:,:),GXA(:,:,:)
      REAL(8),ALLOCATABLE:: GYA(:,:,:),GZA(:,:,:)
      REAL(8),ALLOCATABLE:: PPA(:,:,:)
      REAL(8),ALLOCATABLE:: UUA(:,:,:),VVA(:,:,:),WWA(:,:,:)
      REAL(8),ALLOCATABLE:: UNA(:,:,:),VNA(:,:,:),WNA(:,:,:)
      REAL(8),ALLOCATABLE:: AKA(:,:,:),EPA(:,:,:),TMUA(:,:,:)
      REAL(8),ALLOCATABLE:: AKNA(:,:,:),EPNA(:,:,:)
      REAL(8),ALLOCATABLE:: FFA(:,:,:)
      REAL(8),ALLOCATABLE:: FFNA(:,:,:),GVNA(:,:,:)
      REAL(8),ALLOCATABLE:: HUA(:,:,:),HVA(:,:,:),HWA(:,:,:)
      REAL(8),ALLOCATABLE:: UUBCAIR(:,:,:),VVBCAIR(:,:,:)
      REAL(8),ALLOCATABLE:: UUBCAIRB(:,:,:),VVBCAIRB(:,:,:)
      REAL(8),ALLOCATABLE:: UUBCAIRF(:,:,:),VVBCAIRF(:,:,:)
      REAL(8),ALLOCATABLE:: AKBCAIR(:,:,:),EPBCAIR(:,:,:)
      REAL(8),ALLOCATABLE:: AKBCAIRB(:,:,:),EPBCAIRB(:,:,:)
      REAL(8),ALLOCATABLE:: AKBCAIRF(:,:,:),EPBCAIRF(:,:,:)
      REAL(8),ALLOCATABLE:: AD0A(:,:,:),AL0A(:,:,:,:)
      REAL(8),ALLOCATABLE:: ADA(:,:,:),ALA(:,:,:,:),AUA(:,:,:,:)
      REAL(8),ALLOCATABLE:: BBA(:,:,:),DPA(:,:,:)
C
C ... 落水モデル用
      REAL(8),ALLOCATABLE:: FALLWX(:,:),FALLWY(:,:),FALLWZ(:,:)
      REAL(8),ALLOCATABLE:: DHX(:,:),DHY(:,:)
      REAL(8),ALLOCATABLE:: CFALLWX(:,:),CFALLWY(:,:)
      REAL(8),ALLOCATABLE:: DFALLWNX(:,:),DFALLWNY(:,:)
      REAL(8),ALLOCATABLE:: DFALLWTX(:,:),DFALLWTY(:,:)
C
      INTEGER::ISTAT(MPI_STATUS_SIZE)
C
      REAL(8)::CPUCOM0,CPUCOM1,CPUCOM2,CPUIND0,CPUML2N,CPUNS2M
      REAL(8)::CPURCV0,CPURCV1,CPURCV2,CPUSND0,CPUSND1,CPUSND2
      INTEGER::ICHILD,IERR,IERR1,IERROR,IFLAG,IPARNT,ITAG,ICODE
      INTEGER::IREQ1,IREQ2,IREQ3,IREQ4,IREQ5,IREQ6,IREQ7
      INTEGER::IEAS,IWES,JNOR,JSOU,KBOT,KTOP
      INTEGER::IEAS_ML,IWES_ML,JNOR_ML,JSOU_ML,KBOT_ML,KTOP_ML
      INTEGER::L,MX_ML,MY_ML,MZ_ML
      INTEGER::MX_CHILD,MY_CHILD,MZ_CHILD
      INTEGER::MX_PARNT,MY_PARNT,MZ_PARNT
      INTEGER::N,NBUFSIZE,NBUFSIZEC,NBUFSIZEP
      INTEGER::NFL,NN,NS,NE,ICHILN,NCOUNT,IXYZ(6),NESMLD(4)
      INTEGER::MZ1
      INTEGER::IDUMMY(3,1)
      REAL(8)::RDUMMY(3)
      INTEGER::IRTRN
C
      REAL(8)::DUMMY(1,1),DUMMY2(1,1)
C
      call init_mpmd
C
C     経過時間の計測(開始時)
      WTM1=MPI_WTIME()
C
C     ファイルの基盤番号をセットする(残りはdefaltで設定)
C     INP   : 解析条件入力ファイルの(装置)番号：標準入力
C     LP    : リスト出力の(装置)番号：標準出力
      INP   = 15
      LP    = 16
      MLNS  = 1
C
      DO 100 L=1,NCPUSZ
        CPUSEC(L,1) = 0.0D0
        CPUSEC(L,2) = 0.0D0
 100  CONTINUE
C
      CALL FTIMER( 1,0)
      CALL FTIMER(27,0)
      CALL FTIMER(28,0)
C
      CALL FTIMER(2,0)
      CALL INCNCT(MLNS)
      CALL FTIMER(2,1)
C
      IPARNT=IPECON(2,NRANK+1)
      ICHILD=IPECON(3,NRANK+1)
      NESTFL = 0
      IF(IPARNT.GE.0) NESTFL=1
C
      OPEN(INP,FILE=NMFILE(NRANK+1),STATUS='OLD',FORM='FORMATTED',
     $     ERR=900)
C      OPEN(LP ,FILE='lp.out',STATUS='REPLACE',FORM='FORMATTED',ERR=900)
C      OPEN(LP ,FILE='lp.out',STATUS='NEW',FORM='FORMATTED',ERR=900)
C
C----------------------------------------------------------------------
C     (1) コモン変数の初期化
C----------------------------------------------------------------------
C
      WRITE(LP,*) '+----------------------------------+'
      WRITE(LP,*) '|  PROGRAM STOC(IC) VER.07.01.02v  |'
      WRITE(LP,*) '+----------------------------------+'
      WRITE(LP,*) '(MAIN) SET DEFAULT VALUE ...'
C      CALL FLUSH(6)
C
      CALL DEFALT
      CALL CADMAS_INIT
C
C
C----------------------------------------------------------------------
C     (2) 入力データの読み込み
C----------------------------------------------------------------------
C
      WRITE(LP,*) '(MAIN) READ INPUT DATA ...'
C      CALL FLUSH(6)
C
      IRTRN = 0
      CALL FTIMER(3,0)
      CALL INPUT(0,IRTRN)
      IF(IRTRN.NE.0) THEN
         CALL ERRMSG('NS_MAIN',6050)
         WRITE(LP,*) 'INPUT ERROR'
         CALL ABORT1('')
      ENDIF
      CALL FTIMER(3,1)
C
C ... デバッグ出力用ファイルを開く
      call flnam('.dbg')
      write(lp,*) trim(CFLNM)
      OPEN(IFLDB,FILE=trim(CFLNM),STATUS='NEW',
     $     FORM='FORMATTED',ERR=900)
C ... 温度濃度初期値入力用ファイルを開く
      IF(TTINIT.GT.1.0D10.OR.CCINIT.GT.1.0D10) THEN
        CFLNM(IFLNM-3:IFLNM) = '.ini'
        write(lp,*) CFLNM
        OPEN(IFINI,FILE=CFLNM(1:IFLNM),STATUS='OLD',
     $       FORM='FORMATTED',ERR=901)
      END IF
C ... 海底変動時間変化入力用ファイルを開く
      IF(NBOT.EQ.1) THEN
        CFLNM(IFLNM-3:IFLNM) = '.sbt'
        write(lp,*) CFLNM
        OPEN(IFLSB,FILE=CFLNM(1:IFLNM),STATUS='OLD',
     $       FORM='FORMATTED',ERR=901)
      END IF
C
C ... 親メッシュにおける子領域の範囲を求める
      CALL FTIMER(2,0)
      IWES=0
      IEAS=0
      JSOU=0
      JNOR=0
      KBOT=0
      KTOP=0
      CALL ARCHLD(IWES,IEAS,JSOU,JNOR,KBOT,KTOP)
      CALL ARCONE
      CALL CP_LIST_CMMBND(IERROR)
      CALL FTIMER(2,1)
      IF(ICHILD.GE.0) WRITE(LP,601) IWES,IEAS,JSOU,JNOR,KBOT,KTOP
 601  FORMAT('(CHILD AREA)',6I5)
C
      call mpi_barrier(comm_model,ierr) ! Synchronize (for debug) 1
C
C
C----------------------------------------------------------------------
C     (3) 格子定数の設定
C----------------------------------------------------------------------
C
      WRITE(LP,*) '(MAIN) MAKE GRID CONSTANT ...'
C      CALL FLUSH(6)
C
      IERR = 0
      ALLOCATE(XC(8,MX,MY),XCP(8,MX,MY),XC_REF(8,MX),
     $         YC(8,MY),YC_REF(8,MY),ZC(8,MZ),YCOS(MY),
     $         YCOSP(MY),YSIN(MY),YSINP(MY),STAT=IERR)
      IF(IERR.NE.0) THEN
        CALL ERRMSG('NS_MAIN',6051)
        WRITE(LP,*) 'CANNOT ALLOCATE XC,...'
        CALL ABORT1('')
      END IF
      XC=0.d0
      XCP=0.d0
      XC_REF=0.d0
      YC=0.d0
      YC_REF=0.d0
      ZC=0.d0
      YCOS=0.d0
      YCOSP=0.d0
      YSIN=0.d0
      YSINP=0.d0
C
      IF(LAIR.EQ.1)THEN
         ALLOCATE(ZCA(8,MZA),STAT=IERR)
      ELSE
         ALLOCATE(ZCA(8,1),STAT=IERR)
      ENDIF
      IF(IERR.NE.0) THEN
        CALL ERRMSG('NS_MAIN',6052)
        WRITE(LP,*) 'CANNOT ALLOCATE ZCA,...'
        CALL ABORT1('')
      END IF
      ZCA=0.d0
C
      CALL FTIMER(4,0)
      CALL MKGRID(XC,XCP,YC,ZC,YCOS,YCOSP,YSIN,YSINP,
     $            XC_REF,YC_REF,ZCA,IWES,IEAS,JSOU,JNOR)
      CALL FTIMER(4,1)
C
      call mpi_barrier(comm_model,ierr) ! Synchronize (for debug) 2
C
C     子に自分の格子数を送信する。
      IF(ICHILD.GE.0) THEN
         CALL FTIMER(5,0)
         NS = NUMPE(2,NRANK+1)
         NE = NS+NUMPE(1,NRANK+1)-1
         DO 110 N=NS,NE
         ICHILN = NUMCOM(1,N)
         ITAG=0
         CALL MPI_ISEND(MX,1,MPI_INTEGER,ICHILN,ITAG,comm_model,
     $                  IREQ1,IERROR)
         CALL MPI_ISEND(MY,1,MPI_INTEGER,ICHILN,ITAG,comm_model,
     $                  IREQ2,IERROR)
         CALL MPI_ISEND(MZ,1,MPI_INTEGER,ICHILN,ITAG,comm_model,
     $                  IREQ3,IERROR)
         CALL MPI_WAIT(IREQ1,ISTAT,IERROR)
         CALL MPI_WAIT(IREQ2,ISTAT,IERROR)
         CALL MPI_WAIT(IREQ3,ISTAT,IERROR)
  110    CONTINUE
         CALL FTIMER(5,1)
      ENDIF
C
C     親の格子数を受信する。
      MX_ML=0
      MY_ML=0
      MZ_ML=0
      IF(IPARNT.GE.0) THEN
         CALL FTIMER(6,0)
         CALL MPI_IRECV(MX_ML,1,MPI_INTEGER,IPARNT,MPI_ANY_TAG,
     $                  comm_model,IREQ1,IERROR)
         CALL MPI_IRECV(MY_ML,1,MPI_INTEGER,IPARNT,MPI_ANY_TAG,
     $                  comm_model,IREQ2,IERROR)
         CALL MPI_IRECV(MZ_ML,1,MPI_INTEGER,IPARNT,MPI_ANY_TAG,
     $                  comm_model,IREQ3,IERROR)
         CALL MPI_WAIT(IREQ1,ISTAT,IERROR)
         CALL MPI_WAIT(IREQ2,ISTAT,IERROR)
         CALL MPI_WAIT(IREQ3,ISTAT,IERROR)
         CALL FTIMER(6,1)
C
C     親の格子データに関する配列をアロケートする。
         ALLOCATE(XC_ML(8,MX_ML),YC_ML(8,MY_ML),
     $            ZC_ML(8,MZ_ML),STAT=IERR)
      ELSE
         ALLOCATE(XC_ML(8,1),YC_ML(8,1),ZC_ML(8,1),STAT=IERR)
      ENDIF
      IF(IERR.NE.0) THEN
         CALL ERRMSG('NS_MAIN',6053)
         WRITE(LP,*) 'CANNOT ALLOCATE XC_ML,...'
         CALL ABORT1('')
      ENDIF
      XC_ML=0.d0
      YC_ML=0.d0
      ZC_ML=0.d0
C
C     子に自分の格子データを送信する。
      IF(ICHILD.GE.0) THEN
         CALL FTIMER(7,0)
         NS = NUMPE(2,NRANK+1)
         NE = NS+NUMPE(1,NRANK+1)-1
         DO 120 N=NS,NE
         ICHILN = NUMCOM(1,N)
         ITAG=0
         CALL MPI_ISEND(XC_REF,8*MX,MPI_DOUBLE_PRECISION,ICHILN,ITAG,
     $                  comm_model,IREQ1,IERROR)
C
         CALL MPI_ISEND(YC_REF,8*MY,MPI_DOUBLE_PRECISION,ICHILN,ITAG,
     $                  comm_model,IREQ2,IERROR)
C
         CALL MPI_ISEND(ZC,8*MZ,MPI_DOUBLE_PRECISION,ICHILN,ITAG,
     $                  comm_model,IREQ3,IERROR)
         CALL MPI_WAIT(IREQ1,ISTAT,IERROR)
         CALL MPI_WAIT(IREQ2,ISTAT,IERROR)
         CALL MPI_WAIT(IREQ3,ISTAT,IERROR)
  120    CONTINUE
         CALL FTIMER(7,1)
      ENDIF
C
C     親の格子データを受信する。
      IF(IPARNT.GE.0) THEN
         CALL FTIMER(8,0)
         CALL MPI_IRECV(XC_ML,8*MX_ML,MPI_DOUBLE_PRECISION,IPARNT,
     *                  MPI_ANY_TAG,comm_model,IREQ1,IERROR)
C
         CALL MPI_IRECV(YC_ML,8*MY_ML,MPI_DOUBLE_PRECISION,IPARNT,
     *                  MPI_ANY_TAG,comm_model,IREQ2,IERROR)
C
         CALL MPI_IRECV(ZC_ML,8*MZ_ML,MPI_DOUBLE_PRECISION,IPARNT,
     *                  MPI_ANY_TAG,comm_model,IREQ3,IERROR)
         CALL MPI_WAIT(IREQ1,ISTAT,IERROR)
         CALL MPI_WAIT(IREQ2,ISTAT,IERROR)
         CALL MPI_WAIT(IREQ3,ISTAT,IERROR)
         CALL FTIMER(8,1)
      ENDIF
C
      IF( NB_SC.GT.0 ) CALL CADMAS_AREA
C
C
C----------------------------------------------------------------------
C     (4) インデックス及び壁面処理用リストの作成
C----------------------------------------------------------------------
C
      WRITE(LP,*) '(MAIN) MAKE INDEX ...'
C      CALL FLUSH(6)
C
      MZ1=MZ
      IF(LAIR.EQ.1.AND.MZA.GT.MZ) MZ1=MZA
C
      ALLOCATE(INDP(MX,MY,MZ),INDU(MX,MY,MZ),INDV(MX,MY,MZ),
     $         INDW(MX,MY,MZ),INDK(MX,MY,MZ),WRK1(MX,MY,MZ1),
     $         WRK2(MX,MY,MZ1),WRK3(MX,MY,MZ1),STAT=IERR)
      IF(IERR.NE.0) THEN
        CALL ERRMSG('NS_MAIN',6054)
        WRITE(LP,*) 'CANNOT ALLOCATE INDP,...'
        CALL ABORT1('')
      END IF
      INDP=0
      INDU=0
      INDV=0
      INDW=0
      INDK=0.d0
      WRK1=0.d0
      WRK2=0.d0
      WRK3=0.d0
C
      IF(LAIR.EQ.1)THEN
         ALLOCATE(INDPA(MX,MY,MZA),INDUA(MX,MY,MZA),INDVA(MX,MY,MZA),
     $            INDWA(MX,MY,MZA),KFA(MX,MY),KFNA(MX,MY),STAT=IERR)
      ELSE
         ALLOCATE(INDPA(1,1,1),INDUA(1,1,1),INDVA(1,1,1),
     $            INDWA(1,1,1),KFA(1,1),KFNA(1,1),STAT=IERR)
      ENDIF
      IF(IERR.NE.0) THEN
         CALL ERRMSG('NS_MAIN',6055)
         WRITE(LP,*) 'CANNOT ALLOCATE INDPA,...'
         CALL ABORT1('')
      END IF
      INDPA=0
      INDUA=0
      INDVA=0
      INDWA=0
      KFA=0
      KFNA=0
C
C ... インデックス作成に用いる作業用配列を確保
      ALLOCATE(IWRK1(MX,MY,MZ),IWRK2(MX,MY,MZ),IWRK3(MX,MY,MZ),
     $         STAT=IERR)
      IF(IERR.NE.0) THEN
        CALL ERRMSG('NS_MAIN',6056)
        WRITE(LP,*) 'CANNOT ALLOCATE IWRK1,...'
        CALL ABORT1('')
      END IF
      IWRK1=0
      IWRK2=0
      IWRK3=0
C
      CALL FTIMER(9,0)
C
      NN = IPECON(2,NRANK+1)
C
      IF(NUMPE(1,NN+1).EQ.1) THEN
        NFL = 0
        CALL MKIND1(INDP,INDU,INDV,INDW,IWRK1,WRK1,WRK2,NFL)
      ELSE IF(NUMPE(1,NN+1).GT.1) THEN
        NFL = 1
        CALL MKIND1(INDP,INDU,INDV,INDW,IWRK1,WRK1,WRK2,NFL)
C ....  INDPの通信
        WRK3=DBLE(INDP)
        CALL CP_DSR_DC2(MX,MY,MZ,0,1,WRK3)
        INDP=NINT(WRK3)
        NFL = 2
        CALL MKIND1(INDP,INDU,INDV,INDW,IWRK1,WRK1,WRK2,NFL)
      END IF
C
      CALL FTIMER(9,1)
      CALL ALLOC_LIST(MLWALL,MLWALP,LP)
C
      CALL FTIMER(10,0)
      CALL MKIND2(INDP,INDU,INDV,INDW,IWRK1,IWRK2,IWRK3,
     $            LLWALL,LLWALP,0)
      CALL FTIMER(10,1)
C
C ... インデックス作成で用いた作業用配列を解放
      DEALLOCATE(IWRK1,IWRK2,IWRK3,STAT=IERR)
      IF(IERR.NE.0) THEN
        CALL ERRMSG('NS_MAIN',6057)
        WRITE(LP,*) 'CANNOT ALLOCATE IWRK1,...'
        CALL ABORT1('')
      END IF
C
      call mpi_barrier(comm_model,ierr) ! Synchronize (for debug) 3
c
C     接合面の格子対応関係リスト（親と自分）を作成する。
      IF(IPARNT.GE.0) THEN
         ALLOCATE(I_ML(2,MX_ML),J_ML(2,MY_ML),K_ML(2,MZ_ML),
     $            I_NS(2,MX),J_NS(2,MY),K_NS(2,MZ),STAT=IERR)
         IF(IERR.NE.0) THEN
            CALL ERRMSG('NS_MAIN',6058)
            WRITE(LP,*) 'CANNOT ALLOCATE I_ML,...'
            CALL ABORT1('')
         ENDIF
         I_ML=0
         J_ML=0
         K_ML=0
         I_NS=0
         J_NS=0
         K_NS=0
C
         CALL FTIMER(11,0)
         CALL CP_MKIND3(XC_ML,YC_ML,ZC_ML,XC_REF,YC_REF,ZC,
     1                  I_ML,J_ML,K_ML,I_NS,J_NS,K_NS,
     2                  MX_ML,MY_ML,MZ_ML,MX,MY,MZ)
         IF(NUMPE(1,IPARNT+1).GT.1) THEN
           IXYZ(1) = I_NS(2,2)
           IXYZ(2) = I_NS(2,MXM)
           IXYZ(3) = J_NS(2,2)
           IXYZ(4) = J_NS(2,MYM)
           IXYZ(5) = K_NS(2,2)
           IXYZ(6) = K_NS(2,MZM)
           NCOUNT = 6
           ITAG = 0
           CALL MPI_ISEND( IXYZ,NCOUNT,MPI_INTEGER,IPARNT,ITAG,
     &                     comm_model,IREQ1,IERR )
           CALL MPI_WAIT(IREQ1,ISTAT,IERROR)
         END IF
         CALL FTIMER(11,1)
C
      ELSE
         ALLOCATE(I_ML(2,1),J_ML(2,1),K_ML(2,1),
     $            I_NS(2,1),J_NS(2,1),K_NS(2,1),STAT=IERR)
         IF(IERR.NE.0) THEN
            CALL ERRMSG('NS_MAIN',6059)
            WRITE(LP,*) 'CANNOT ALLOCATE I_ML,...'
            CALL ABORT1('')
         ENDIF
         I_ML=0
         J_ML=0
         K_ML=0
         I_NS=0
         J_NS=0
         K_NS=0
      ENDIF
      IF(NUMPE(1,NRANK+1).GT.1) THEN
         NS = NUMPE(2,NRANK+1)
         NE = NS+NUMPE(1,NRANK+1)-1
         DO 130 N=NS,NE
           ICHILN = NUMCOM(1,N)
           ITAG = 0
           NCOUNT = 6
           CALL MPI_IRECV(IXYZ,NCOUNT,MPI_INTEGER,ICHILN,MPI_ANY_TAG,
     $                    comm_model,IREQ7,IERROR)
           CALL MPI_WAIT(IREQ7,ISTAT,IERROR)
           DO 135 L=1,6
             INDCM2(L,N-NS+1) = IXYZ(L)
  135      CONTINUE
      write(6,1) nrank,n-ns+1,(indcm2(l,n-ns+1),l=1,6)
 1    format('nrank=',i5,'   n,indcm2=',7i5)
  130    CONTINUE
      END IF
C
      IF(NPROC.GT.1) THEN
         DO 140 N=1,4
           L = NOVRLP(N)
           CALL MPI_ALLREDUCE( L,NOVRLP(N),1,MPI_INTEGER,MPI_MAX,
     $                         CHILDCOMM,IERR )
  140    CONTINUE
      END IF
C
C     子に親メッシュサイズでのオーバーラップ幅と
C                                     計算範囲を送信する
      IF(ICHILD.GE.0) THEN
         CALL FTIMER(12,0)
         NS = NUMPE(2,NRANK+1)
         NE = NS+NUMPE(1,NRANK+1)-1
         DO 150 N=NS,NE
         ICHILN = NUMCOM(1,N)
         ITAG=0
         CALL MPI_ISEND(IWES,1,MPI_INTEGER,ICHILN,ITAG,comm_model,
     $                  IREQ1,IERROR)
         CALL MPI_ISEND(IEAS,1,MPI_INTEGER,ICHILN,ITAG,comm_model,
     $                  IREQ2,IERROR)
         CALL MPI_ISEND(JSOU,1,MPI_INTEGER,ICHILN,ITAG,comm_model,
     $                  IREQ3,IERROR)
         CALL MPI_ISEND(JNOR,1,MPI_INTEGER,ICHILN,ITAG,comm_model,
     $                  IREQ4,IERROR)
         CALL MPI_ISEND(KBOT,1,MPI_INTEGER,ICHILN,ITAG,comm_model,
     $                  IREQ5,IERROR)
         CALL MPI_ISEND(KTOP,1,MPI_INTEGER,ICHILN,ITAG,comm_model,
     $                  IREQ6,IERROR)
         CALL MPI_WAIT(IREQ1,ISTAT,IERROR)
         CALL MPI_WAIT(IREQ2,ISTAT,IERROR)
         CALL MPI_WAIT(IREQ3,ISTAT,IERROR)
         CALL MPI_WAIT(IREQ4,ISTAT,IERROR)
         CALL MPI_WAIT(IREQ5,ISTAT,IERROR)
         CALL MPI_WAIT(IREQ6,ISTAT,IERROR)
         CALL FTIMER(12,1)
C
         CALL FTIMER(13,0)
         CALL MPI_IRECV(NESML,4,MPI_INTEGER,ICHILN,MPI_ANY_TAG,
     $                  comm_model,IREQ7,IERROR)
         CALL MPI_WAIT(IREQ7,ISTAT,IERROR)
         CALL FTIMER(13,1)
  150    CONTINUE
      ENDIF
C
C     親のオーバーラップ幅と計算範囲を受信する。
      IWES_ML=0
      IEAS_ML=0
      JSOU_ML=0
      JNOR_ML=0
      KBOT_ML=0
      KTOP_ML=0
      IF(IPARNT.GE.0) THEN
         CALL FTIMER(13,0)
         CALL MPI_IRECV(IWES_ML,1,MPI_INTEGER,IPARNT,MPI_ANY_TAG,
     $                  comm_model,IREQ1,IERROR)
         CALL MPI_IRECV(IEAS_ML,1,MPI_INTEGER,IPARNT,MPI_ANY_TAG,
     $                  comm_model,IREQ2,IERROR)
         CALL MPI_IRECV(JSOU_ML,1,MPI_INTEGER,IPARNT,MPI_ANY_TAG,
     $                  comm_model,IREQ3,IERROR)
         CALL MPI_IRECV(JNOR_ML,1,MPI_INTEGER,IPARNT,MPI_ANY_TAG,
     $                  comm_model,IREQ4,IERROR)
         CALL MPI_IRECV(KBOT_ML,1,MPI_INTEGER,IPARNT,MPI_ANY_TAG,
     $                  comm_model,IREQ5,IERROR)
         CALL MPI_IRECV(KTOP_ML,1,MPI_INTEGER,IPARNT,MPI_ANY_TAG,
     $                  comm_model,IREQ6,IERROR)
         CALL MPI_WAIT(IREQ1,ISTAT,IERROR)
         CALL MPI_WAIT(IREQ2,ISTAT,IERROR)
         CALL MPI_WAIT(IREQ3,ISTAT,IERROR)
         CALL MPI_WAIT(IREQ4,ISTAT,IERROR)
         CALL MPI_WAIT(IREQ5,ISTAT,IERROR)
         CALL MPI_WAIT(IREQ6,ISTAT,IERROR)
         CALL FTIMER(13,1)
         CALL FTIMER(12,0)
         ITAG=0
         CALL MPI_ISEND(NOVRLP,4,MPI_INTEGER,IPARNT,ITAG,comm_model,
     $                 IREQ7,IERROR)
         CALL MPI_WAIT(IREQ7,ISTAT,IERROR)
         CALL FTIMER(12,1)
      END IF
C
      call mpi_barrier(comm_model,ierr) ! Synchronize (for debug) 4
c
C=================================================FOR OIL_PARTICLE START
C OIL_PARTICLE への格子情報の送信
C
      CALL OIL_COMM_GRID(XC_REF,YC_REF,MX,MY)
C=================================================FOR OIL_PARTICLE END
C
C----------------------------------------------------------------------
C     (5) ポロシティの設定
C----------------------------------------------------------------------
C
      WRITE(LP,*) '(MAIN) SET POROUS CONSTANT ...'
C      CALL FLUSH(6)
C
      ALLOCATE(GV0(MX,MY,MZ),GX0(MX,MY,MZ),GY0(MX,MY,MZ),GZ0(MX,MY,MZ),
     $         GV(MX,MY,MZ),GX(MX,MY,MZ),GY(MX,MY,MZ),GZ(MX,MY,MZ),
     $         GVD(MX,MY,MZ),GXD(MX,MY,MZ),GYD(MX,MY,MZ),GZD(MX,MY,MZ),
     $         CMD(MX,MY,MZ),CDD(MX,MY,MZ),COE1D(MX,MY,MZ),
     $         COE2D(MX,MY,MZ),FRIC(MX,MY,MZ),AMNG(MX,MY),CD(MX,MY),
     $         STAT=IERR)
      IF(IERR.NE.0) THEN
        CALL ERRMSG('NS_MAIN',6060)
        WRITE(LP,*) 'CANNOT ALLOCATE GV0,...'
        CALL ABORT1('')
      END IF
      GV0=0.d0
      GX0=0.d0
      GY0=0.d0
      GZ0=0.d0
      GV=0.d0
      GX=0.d0
      GY=0.d0
      GZ=0.d0
      GVD=0.d0
      GXD=0.d0
      GYD=0.d0
      GZD=0.d0
      CMD=0.d0
      CDD=0.d0
      COE1D=0.d0
      COE2D=0.d0
      FRIC=0.d0
      AMNG=0.d0
      CD=0.d0
C
      IF(LAIR.EQ.1) THEN
         ALLOCATE(GVA(MX,MY,MZA),GXA(MX,MY,MZA),GYA(MX,MY,MZA),
     $            GZA(MX,MY,MZA),STAT=IERR)
      ELSE
         ALLOCATE(GVA(1,1,1),GXA(1,1,1),GYA(1,1,1),
     $            GZA(1,1,1),STAT=IERR)
      ENDIF
      IF(IERR.NE.0) THEN
         CALL ERRMSG('NS_MAIN',6061)
         WRITE(LP,*) 'CANNOT ALLOCATE GVA,...'
         CALL ABORT1('')
      END IF
      GVA=0.d0
      GXA=0.d0
      GYA=0.d0
      GZA=0.d0
C
      CALL FTIMER(14,0)
      CALL MKPORS(GV,GX,GY,GZ,FRIC,AMNG,INDP,INDU,INDV,INDW,
     $            ICHILD,IWES,IEAS,JSOU,JNOR,KBOT,KTOP)
      CALL FTIMER(14,1)
C
      IF(LAIR.EQ.1) THEN
         CALL MKPORS_AIR(GVA,GXA,GYA,GZA,INDPA,INDUA,INDVA,INDWA,KFA)
      ENDIF
C
C     通信バッファのアロケート
      NBUFSIZEP=0
      NBUFSIZEC=0
C
      IF(IPARNT.GE.0) THEN
         MX_PARNT=IEAS_ML-IWES_ML+3
         MY_PARNT=JNOR_ML-JSOU_ML+3
         MZ_PARNT=KTOP_ML-KBOT_ML+3
         NBUFSIZEP=(NOVRLP(2)+2)*MY_PARNT*MZ_PARNT
     *            +(NOVRLP(3)+2)*MY_PARNT*MZ_PARNT
     *            +(NOVRLP(1)+2)*MX_PARNT*MZ_PARNT
     *            +(NOVRLP(4)+2)*MX_PARNT*MZ_PARNT
      ENDIF
C
      IF(ICHILD.GE.0) THEN
         MX_CHILD=IEAS-IWES+3
         MY_CHILD=JNOR-JSOU+3
         MZ_CHILD=KTOP-KBOT+3
         NBUFSIZEC=(NESML(2)+2)*MY_CHILD*MZ_CHILD
     *            +(NESML(3)+2)*MY_CHILD*MZ_CHILD
     *            +(NESML(1)+2)*MX_CHILD*MZ_CHILD
     *            +(NESML(4)+2)*MX_CHILD*MZ_CHILD
      ENDIF
C
      NBUFSIZE=MAX(1,MAX(NBUFSIZEP,NBUFSIZEC))
      ALLOCATE(IBUF(NBUFSIZE),BUF(NBUFSIZE),STAT=IERR)
      IF(IERR.NE.0) THEN
         CALL ERRMSG('NS_MAIN',6062)
         WRITE(LP,*) 'CANNOT ALLOCATE IBUF,...'
         CALL ABORT1('')
      ENDIF
      IBUF=0
      BUF=0.d0
C
C     INDU,INDV,INDW,GX,GY,GZを子に送信する。
      IF(ICHILD.GE.0) THEN
         CALL FTIMER(15,0)
         CALL CP_SNDIND(INDU,INDV,INDW,INDP,IBUF,
     1                  GX,GY,GZ,GV,BUF,
     2                  MX,MY,MZ,
     3                  IEAS,IWES,JSOU,JNOR,KBOT,KTOP,
     4                  NESML(2),NESML(3),NESML(1),NESML(4))
         CALL FTIMER(15,1)
      ENDIF
C
C     INDU,INDV,INDW,GX,GY,GZを親から受信する。
      IF(IPARNT.GE.0) THEN
         ALLOCATE(INDU_ML(MX_PARNT,MY_PARNT,MZ_PARNT),
     $            INDV_ML(MX_PARNT,MY_PARNT,MZ_PARNT),
     $            INDW_ML(MX_PARNT,MY_PARNT,MZ_PARNT),
     $            INDP_ML(MX_PARNT,MY_PARNT,MZ_PARNT),
     $            GX_ML(MX_PARNT,MY_PARNT,MZ_PARNT),
     $            GY_ML(MX_PARNT,MY_PARNT,MZ_PARNT),
     $            GZ_ML(MX_PARNT,MY_PARNT,MZ_PARNT),STAT=IERR)
         IF(IERR.NE.0) THEN
           CALL ERRMSG('NS_MAIN',6063)
           WRITE(LP,*) 'CANNOT ALLOCATE INDU_ML,...'
           CALL ABORT1('')
         ENDIF
         INDU_ML=0
         INDV_ML=0
         INDW_ML=0
         INDP_ML=0
         GX_ML=0.d0
         GY_ML=0.d0
         GZ_ML=0.d0
C
         CALL FTIMER(16,0)
         CALL CP_RCVIND(INDU_ML,INDV_ML,INDW_ML,INDP_ML,IBUF,
     1                  GX_ML,GY_ML,GZ_ML,WRK1,BUF,
     2                  MX_ML,MY_ML,MZ_ML,
     3                  IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,KTOP_ML,
     4                  NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4))
         CALL FTIMER(16,1)
      ELSE
         ALLOCATE(INDU_ML(1,1,1),
     $            INDV_ML(1,1,1),
     $            INDW_ML(1,1,1),
     $            INDP_ML(1,1,1),
     $            GX_ML(1,1,1),
     $            GY_ML(1,1,1),
     $            GZ_ML(1,1,1),STAT=IERR)
         IF(IERR.NE.0) THEN
            CALL ERRMSG('NS_MAIN',6064)
            WRITE(LP,*) 'CANNOT ALLOCATE INDU_ML,...'
            CALL ABORT1('')
         ENDIF
         INDU_ML=0
         INDV_ML=0
         INDW_ML=0
         INDP_ML=0
         GX_ML=0.d0
         GY_ML=0.d0
         GZ_ML=0.d0
      ENDIF
C
C
C----------------------------------------------------------------------
C     (6) 初期条件の設定
C----------------------------------------------------------------------
      WRITE(LP,*) '(MAIN) SET INITIAL CONDITION ...'
C      CALL FLUSH(6)
C
      ALLOCATE(UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ),
     $         HU(MX,MY,MZ),HV(MX,MY,MZ),HW(MX,MY,MZ),
     $         PP(MX,MY,MZ),TT(MX,MY,MZ),CC(MX,MY,MZ),
     $         AK(MX,MY,MZ),EP(MX,MY,MZ),RL(MX,MY,MZ),
     $         TMU(MX,MY,MZ),STAT=IERR)
      IF(IERR.NE.0) THEN
        CALL ERRMSG('NS_MAIN',6065)
        WRITE(LP,*) 'CANNOT ALLOCATE UU,...'
        CALL ABORT1('')
      END IF
      UU=0.d0
      VV=0.d0
      WW=0.d0
      HU=0.d0
      HV=0.d0
      HW=0.d0
      PP=0.d0
      TT=0.d0
      CC=0.d0
      AK=0.d0
      EP=0.d0
      RL=0.d0
      TMU=0.d0
C
      ALLOCATE(FF(MX,MY,MZ),RHOW(MX,MY,MZ),VLEND(MX,MY,16),
     $         TMEND(MX,MY,6),FREND(MX,MY,2+NFRAGL),
     $         RMMB(MX,MY,9),RMMF(MX,MY,9),HH(MX,MY),
     $         HX(MX,MY),HY(MX,MY),PATM(MX,MY),WX(MX,MY),WY(MX,MY),
     $         DPS(MX,MY),QQ(MX,MY,MZ),QW(MX,MY),HDEP(MX,MY),
     $         KF(MX,MY),KG(MX,MY),KP(MX,MY),KH(MX,MY),STAT=IERR)
      IF(IERR.NE.0) THEN
        CALL ERRMSG('NS_MAIN',6066)
        WRITE(LP,*) 'CANNOT ALLOCATE FF,...'
        CALL ABORT1('')
      END IF
      FF=0.d0
      RHOW=0.d0
      VLEND=0.d0
      TMEND=0.d0
      FREND=0.d0
      RMMB=0.d0
      RMMF=0.d0
      HH=0.d0
      HX=0.d0
      HY=0.d0
      PATM=0.d0
      WX=0.d0
      WY=0.d0
      DPS=0.d0
      QQ=0.d0
      QW=0.d0
      HDEP=0.d0
      KF=0
      KG=0
      KP=0
      KH=0
C
      IF( LBRKW.GT.0 ) THEN
         ALLOCATE(TMUBW(MX,MY),TIMBW(MX,MY),STAT=IERR)
      ELSE
         ALLOCATE(TMUBW(1,1),TIMBW(1,1),STAT=IERR)
      ENDIF
      IF(IERR.NE.0) THEN
        CALL ERRMSG('NS_MAIN',6067)
        WRITE(LP,*) 'CANNOT ALLOCATE TMUBW,...'
        CALL ABORT1('')
      END IF
      TMUBW=0.d0
      TIMBW=-9999.0D0
C
      IF( LSEDI.EQ.1 ) THEN
         NXY = MAX(MX,MY)
         ALLOCATE(CSEDI(MX,MY,MZ),CSEDIN(MX,MY,MZ),CSDAVE(MX,MY),
     $            SHLSD(MX,MY),USSD(MX,MY),WEXSD(MX,MY),
     $            EXSDE(MX,MY),EXSDD(MX,MY),
     $            ZBED(MX,MY),ZBEDN(MX,MY),ZBED0(MX,MY),QBX(MX,MY),
     $            QBY(MX,MY),DZBUF(MX,MY),GXBDH(MX,MY),GYBDH(MX,MY),
     $            KIBDH(MX,MY),KJBDH(MX,MY),STAT=IERR)
      ELSE
         ALLOCATE(CSEDI(1,1,1),CSEDIN(1,1,1),CSDAVE(1,1),SHLSD(1,1),
     $            USSD(1,1),WEXSD(1,1),EXSDE(1,1),
     $            EXSDD(1,1),ZBED(1,1),ZBEDN(1,1),ZBED0(1,1),QBX(1,1),
     $            QBY(1,1),DZBUF(1,1),GXBDH(1,1),GYBDH(1,1),
     $            KIBDH(1,1),KJBDH(1,1),STAT=IERR)
      ENDIF
      IF(IERR.NE.0) THEN
        CALL ERRMSG('NS_MAIN',6068)
        WRITE(LP,*) 'CANNOT ALLOCATE CSEDI,...'
        CALL ABORT1('')
      END IF
      CSEDI=0.d0
      CSEDIN=0.d0
      CSDAVE=0.d0
      SHLSD=0.d0
      USSD=0.d0
      WEXSD=0.d0
      EXSDE=0.d0
      EXSDD=0.d0
      ZBED=0.d0
      ZBEDN=0.d0
      ZBED0=0.d0
      QBX=0.d0
      QBY=0.d0
      DZBUF=0.d0
      GXBDH=0.d0
      GYBDH=0.d0
      KIBDH=0
      KJBDH=0
C
      IF(LAIR.EQ.1) THEN
         ALLOCATE(PPA(MX,MY,MZA),UUA(MX,MY,MZA),VVA(MX,MY,MZA),
     $            WWA(MX,MY,MZA),STAT=IERR)
      ELSE
         ALLOCATE(PPA(1,1,1),UUA(1,1,1),VVA(1,1,1),
     $            WWA(1,1,1),STAT=IERR)
      ENDIF
      IF(IERR.NE.0) THEN
        CALL ERRMSG('NS_MAIN',6069)
        WRITE(LP,*) 'CANNOT ALLOCATE PPA,...'
        CALL ABORT1('')
      END IF
      PPA=0.d0
      UUA=0.d0
      VVA=0.d0
      WWA=0.d0

      IF(LAIR.EQ.1.AND.LTURBA.EQ.2) THEN
         ALLOCATE(AKA(MX,MY,MZA),EPA(MX,MY,MZA),STAT=IERR)
      ELSE
         ALLOCATE(AKA(1,1,1),EPA(1,1,1),STAT=IERR)
      ENDIF
      IF(IERR.NE.0) THEN
        CALL ERRMSG('NS_MAIN',6070)
        WRITE(LP,*) 'CANNOT ALLOCATE AKA,...'
        CALL ABORT1('')
      END IF
      AKA=0.d0
      EPA=0.d0
C
      IF(IPARNT.GE.0) THEN
         ALLOCATE(UU_ML(MX_PARNT,MY_PARNT,MZ_PARNT),
     $            VV_ML(MX_PARNT,MY_PARNT,MZ_PARNT),
     $            WW_ML(MX_PARNT,MY_PARNT,MZ_PARNT),
     $            TT_ML(MX_PARNT,MY_PARNT,MZ_PARNT),
     $            CC_ML(MX_PARNT,MY_PARNT,MZ_PARNT),STAT=IERR)
         IF(IERR.NE.0) THEN
            CALL ERRMSG('NS_MAIN',6071)
            WRITE(LP,*) 'CANNOT ALLOCATE UU_ML,...'
            CALL ABORT1('')
         ENDIF
C
         IF( LSEDI.EQ.1 ) THEN
           ALLOCATE(CSD_ML(MX_PARNT,MY_PARNT,MZ_PARNT),
     $              ZBD_ML(MX_PARNT,MY_PARNT),STAT=IERR)
         ELSE
           ALLOCATE(CSD_ML(1,1,1),
     $              ZBD_ML(1,1),STAT=IERR)
         END IF
         IF(IERR.NE.0) THEN
            CALL ERRMSG('NS_MAIN',6072)
            WRITE(LP,*) 'CANNOT ALLOCATE CSD_ML,...'
            CALL ABORT1('')
         ENDIF
C
         ALLOCATE(AK_ML(MX_PARNT,MY_PARNT,MZ_PARNT),
     $            EP_ML(MX_PARNT,MY_PARNT,MZ_PARNT),
     $            HH_ML(MX_PARNT,MY_PARNT),
     $            KG_ML(MX_PARNT,MY_PARNT),
     $            KF_ML(MX_PARNT,MY_PARNT),
     $            HDEP_ML(MX_PARNT,MY_PARNT),STAT=IERR)
         IF(IERR.NE.0) THEN
            CALL ERRMSG('NS_MAIN',6073)
            WRITE(LP,*) 'CANNOT ALLOCATE AK_ML,...'
            CALL ABORT1('')
         ENDIF
C
         NXY = MAX(MX,MY)
         ALLOCATE(UUBCN(NXY,MZ,4),
     $            VVBCN(NXY,MZ,4),
     $            WWBCN(NXY,MZ,4),
     $            TTBCN(NXY,MZ,4),
     $            CCBCN(NXY,MZ,4),STAT=IERR)
         IF(IERR.NE.0) THEN
            CALL ERRMSG('NS_MAIN',6074)
            WRITE(LP,*) 'CANNOT ALLOCATE UUBCN,...'
            CALL ABORT1('')
         ENDIF
C
         IF( LSEDI.EQ.1 ) THEN
           ALLOCATE(CSDBCN(NXY,MZ,4),
     $              ZBDBCN(MX,MY),STAT=IERR)
         ELSE
           ALLOCATE(CSDBCN(1,1,1),
     $              ZBDBCN(1,1),STAT=IERR)
         END IF
         IF(IERR.NE.0) THEN
            CALL ERRMSG('NS_MAIN',6075)
            WRITE(LP,*) 'CANNOT ALLOCATE CSDBCN,...'
            CALL ABORT1('')
         ENDIF
C
         ALLOCATE(AKBCN(NXY,MZ,4),
     $            EPBCN(NXY,MZ,4),
     $            HHBCN(MX,MY),
     $            HHW(MX,MY),STAT=IERR)
         IF(IERR.NE.0) THEN
            CALL ERRMSG('NS_MAIN',6076)
            WRITE(LP,*) 'CANNOT ALLOCATE AKBCN,...'
            CALL ABORT1('')
         ENDIF
      ELSE
         ALLOCATE(UU_ML(1,1,1),VV_ML(1,1,1),WW_ML(1,1,1),
     $            TT_ML(1,1,1),CC_ML(1,1,1),CSD_ML(1,1,1),
     $            ZBD_ML(1,1),AK_ML(1,1,1),EP_ML(1,1,1),
     $            HH_ML(1,1),KG_ML(1,1),KF_ML(1,1),HDEP_ML(1,1),
     $            UUBCN(1,1,1),VVBCN(1,1,1),WWBCN(1,1,1),
     $            TTBCN(1,1,1),CCBCN(1,1,1),
     $            CSDBCN(1,1,1),ZBDBCN(1,1),
     $            AKBCN(1,1,1),EPBCN(1,1,1),
     $            HHBCN(1,1),HHW(1,1),STAT=IERR)
         IF(IERR.NE.0) THEN
            CALL ERRMSG('NS_MAIN',6077)
            WRITE(LP,*) 'CANNOT ALLOCATE UU_ML,...'
            CALL ABORT1('')
         ENDIF
      END IF
      UU_ML=0.d0
      VV_ML=0.d0
      WW_ML=0.d0
      TT_ML=0.d0
      CC_ML=0.d0
      CSD_ML=0.d0
      ZBD_ML=0.d0
      AK_ML=0.d0
      EP_ML=0.d0
      HH_ML=0.d0
      KG_ML=0
      KF_ML=0
      HDEP_ML=0.d0
      UUBCN=0.d0
      VVBCN=0.d0
      WWBCN=0.d0
      TTBCN=0.d0
      CCBCN=0.d0
      CSDBCN=0.d0
      ZBDBCN=0.d0
      AKBCN=0.d0
      EPBCN=0.d0
      HHBCN=0.d0
      HHW=0.d0
C
      WRITE(LP,*) '(MAIN) INITIALIZE VARIABLES ...'
C
      CALL FTIMER(17,0)
      CALL MKINIT(UU,VV,WW,PP,TT,CC,AK,EP,TMU,
     1            INDP,INDU,INDV,INDW,HDEP,AMNG,
     $            FF,HH,PATM,WX,WY,KF,KG,KP,
     $            XC,XCP,YC,ZC,YCOS,YCOSP,YSIN,YSINP,GV,GX,GY,
     $            VLEND,TMEND,FREND,HHBCN,UUBCN,VVBCN,
     $            DUMMY,DUMMY2,IDUMMY)
      CALL FTIMER(17,1)
C
      IF( NB_SC.GT.0 ) CALL CADMAS_PORS(INDP,INDU,INDV,INDW,LLWALL,
     $                                  GV,GX,GY,GZ,ZC,
     $                                  KF,KG,KP,FF,HH,HDEP)
C
      call mpi_barrier(comm_model,ierr) ! Synchronize (for debug) 5
C
      IF( LSEDI.EQ.1 ) THEN
         CALL MKINISD(CSEDI,CSEDIN,CSDAVE,SHLSD,USSD,WEXSD,
     $                EXSDE,EXSDD,ZBED,ZBEDN,ZBED0,QBX,QBY,DZBUF,AMNG)
      ENDIF
C
      IF(LAIR.EQ.1) THEN
         CALL MKIND_AIR(HH,ZCA,INDPA,INDUA,INDVA,INDWA,KFA,KFNA)
C
         CALL MKINIT_AIR(PPA,UUA,VVA,WWA,AKA,EPA,
     $                   INDPA,INDUA,INDVA,INDWA,KFA)
      ENDIF
C
      CALL CP_NEIBIND(INDP,INDU,INDV,INDW,KF,KP,KG,GV,GX,GY,GZ,
     $                FRIC,HDEP,AMNG)
C
C     接合面の物理量を子に送信する。
      IF(ICHILD.GE.0) THEN
         CALL FTIMER(18,0)
         CALL CP_SNDML2NS(KF,KG,IBUF,
     1                    UU,VV,WW,TT,CC,AK,EP,
     $                    HH,HDEP,CSEDI,ZBED,BUF,
     2                    MX,MY,MZ,
     3                    IEAS,IWES,JSOU,JNOR,KBOT,KTOP,
     4                    NESML(2),NESML(3),NESML(1),NESML(4),0)
         CALL FTIMER(18,1)
      ENDIF
C
C     接合面の物理量を受信する。
      IF(IPARNT.GE.0) THEN
         CALL FTIMER(19,0)
         IFLAG = 0
         CALL CP_RCVML2NS(KF_ML,KG_ML,IBUF,
     1                    UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,AK_ML,EP_ML,
     *                    HH_ML,HDEP_ML,CSD_ML,ZBD_ML,BUF,
     2                    MX_ML,MY_ML,MZ_ML,
     3                    IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,
     *                    KTOP_ML,
     4                    NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4),IFLAG)
         CALL FTIMER(19,1)
      ENDIF
C
C ... オーバーラップ領域の水深を変更
C20111124      IF(LOBST.EQ.1.AND.LSTART.EQ.0) THEN
      IF(LOBST.EQ.1) THEN
C20111124        IF(IPARNT.LT.0.AND.ICHILD.GE.0) THEN
        IF(IPARNT.LT.0.AND.ICHILD.GE.0.AND.LSTART.EQ.0) THEN
          CALL FTIMER(20,0)
          CALL CP_SNDDEP(HDEP,HH,BUF,MX,MY,
     1                   IEAS,IWES,JSOU,JNOR,
     2                   NESML(2),NESML(3),NESML(1),NESML(4),ICHILD)
          CALL FTIMER(20,1)
        END IF
C
        IERR1 = 0
        IF(IPARNT.GE.0) THEN
           IF(LSTART.EQ.0) THEN
            CALL FTIMER(21,0)
            CALL CP_RCVDEP(HDEP_ML,HH_ML,BUF,MX_ML,MY_ML,
     1                     IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
     2                     NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4),
     3                     IPARNT)
            CALL FTIMER(21,1)
           ENDIF
C
            IF(LMODDEP.EQ.1) THEN
            CALL FTIMER(22,0)
            CALL CP_MODDEP(INDP,INDU,INDV,INDW,GV,GX,GY,HDEP,HDEP_ML,
     1                     XC_REF,YC_REF,ZC,HH,HH_ML,FF,PP,
     2                     PATM,I_ML,J_ML,KF,KF_ML,KP,KG,
     3                     MX_ML,MY_ML,MZ_ML,MX,MY,MZ,
     4                     NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4),
     5                     IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,IERR1)
            CALL FTIMER(22,1)
            ENDIF
C
C20111124            IF(ICHILD.GE.0) THEN
            IF(ICHILD.GE.0.AND.LSTART.EQ.0) THEN
              CALL FTIMER(20,0)
              CALL CP_SNDDEP(HDEP,HH,BUF,MX,MY,
     1                       IEAS,IWES,JSOU,JNOR,
     2                       NESML(2),NESML(3),NESML(1),NESML(4),
     3                       ICHILD)
              CALL FTIMER(20,1)
            END IF
        END IF
        IF(IERR1.NE.0) THEN
          CALL ERRMSG('NS_MAIN',6078)
          CFLNM(IFLNM-3:IFLNM) = '.str'
          WRITE(*,*)  'FILE DATA UNMACTHED = ',CFLNM(1:IFLNM)
          WRITE(LP,*) 'FILE DATA UNMACTHED =',CFLNM(1:IFLNM)
          WRITE(LP,*) 'STOP ( PE NO. = )',NRANK
          CALL ABORT1('')
        END IF
      END IF
C
      IFLAG = 0
C
      CALL CP_NEIBCOM(UU,VV,WW,PP,RHOW,TT,CC,CSEDI,ZBED,
     1                AK,EP,TMU,WX,WY,CD,PATM,HH,KF,KP,IFLAG)
C
      IF(IPARNT.GE.0.AND.LSTART.EQ.0) THEN
C
         CALL FTIMER(23,0)
         CALL CP_BCML2NS(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     *                   INDU,INDV,INDP,
     1                   XC_ML,YC_ML,ZC_ML,XC_REF,YC_REF,ZC,
     2                   GX_ML,GX,GY_ML,GY,
     3                   I_ML,J_ML,K_ML,I_NS,J_NS,K_NS,
     4                   KF_ML,KG_ML,KF,KG,
     5                   UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,AK_ML,EP_ML,
     *                   HH_ML,HDEP_ML,HDEP,CSD_ML,ZBD_ML,
     6                   HHBCN,UUBCN,VVBCN,WWBCN,TTBCN,CCBCN,
     *                   AKBCN,EPBCN,CSDBCN,ZBDBCN,
     8                   MX_ML,MY_ML,MZ_ML,MX,MY,MZ,
     9                   IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
     A                   KBOT_ML,KTOP_ML,IFLAG)
         CALL FTIMER(23,1)
      END IF
C
      IF(IPARNT.GE.0.AND.LSTART.EQ.0) THEN
         CALL FTIMER(24,0)
         CALL CP_BCNS2ML(INDU_ML,INDV_ML,INDW_ML,INDP_ML,
     1                   INDU,INDV,INDW,INDP,
     2                   XC_ML,YC_ML,ZC_ML,XC_REF,YC_REF,ZC,
     3                   GX_ML,GY_ML,GZ_ML,GX,GY,GZ,GV,
     4                   I_ML,J_ML,K_ML,KF_ML,KG_ML,
     $                   I_NS,J_NS,KF,KG,
     5                   UU,VV,WW,TT,CC,HH,HDEP,
     6                   UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,HH_ML,HDEP_ML,
     $                   CSEDI,ZBED,CSD_ML,ZBD_ML,
     $                   AK,EP,AK_ML,EP_ML,
     7                   MX_ML,MY_ML,MZ_ML,MX,MY,MZ,
     8                   IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
     9                   KBOT_ML,KTOP_ML,IFLAG)
         CALL FTIMER(24,1)
      END IF
C
      IF(IPARNT.GE.0.AND.LSTART.EQ.0) THEN
         CALL FTIMER(25,0)
         CALL  CP_SNDNS2ML(UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,AK_ML,EP_ML,
     1                     HH_ML,CSD_ML,ZBD_ML,BUF,
     2                     MX_ML,MY_ML,MZ_ML,
     3                     IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,
     *                                                     KTOP_ML,
     4                     NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4),
     5                     IFLAG)
         CALL FTIMER(25,1)
      END IF
C
      IF(ICHILD.GE.0.AND.LSTART.EQ.0) THEN
         CALL FTIMER(26,0)
         CALL  CP_RCVNS2ML(UU,VV,WW,TT,CC,AK,EP,
     1                     HH,CSEDI,ZBED,BUF,
     2                     MX,MY,MZ,
     3                     IEAS,IWES,JSOU,JNOR,KBOT,KTOP,
     4                     NESML(2),NESML(3),NESML(1),NESML(4),
     5                     IFLAG)
         CALL FTIMER(26,1)
      END IF
C
C
      IF(LSEDI.EQ.1)THEN
      IF(ICHILD.GE.0) THEN
         CALL CP_SNDZBD(ZBED,BUF,MX,MY,
     $                  IEAS,IWES,JSOU,JNOR,
     $                  NESML(2),NESML(3),NESML(1),NESML(4),ICHILD)
      ENDIF
C
      IF(IPARNT.GE.0) THEN
         CALL CP_RCVZBD(ZBD_ML,BUF,MX_ML,MY_ML,
     $                  IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,
     $                  NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4),IPARNT)
         CALL CP_MODZBD(ZBED,ZBD_ML,
     $                  I_ML,J_ML,MX_ML,MY_ML,MX,MY,
     $                  NOVRLP(2),NOVRLP(3),NOVRLP(1),NOVRLP(4),
     $                  IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML)
         ZBED0(:,:)=ZBED(:,:)
      ENDIF
      ENDIF
C
C
      CALL CP_NEIBCOM(UU,VV,WW,PP,RHOW,TT,CC,CSEDI,ZBED,
     1                AK,EP,TMU,WX,WY,CD,PATM,HH,KF,KP,IFLAG)
C
C
C(120214. MKIND4をCALLする位置をcp_moddepの後に移動)
C ... MLWALBの大きさを調べる
      CALL MKIND4(GV,GX,GY,XC,YC,ZC,HDEP,RDUMMY,
     $            INDP,INDU,INDV,IDUMMY,IDUMMY,0,0)
C
      CALL ALLOC_LIST2(MLWALB,MLOFL,LP)
C
C ... 防潮堤用リストベクトルLLWALBを設定する
      CALL MKIND4(GV,GX,GY,XC,YC,ZC,HDEP,HHOFL,
     $            INDP,INDU,INDV,LLWALB,LLOFL,1,0)
C
C
C ... コモン変数の値を出力
      CALL COMOUT
C
      CALL FTIMER(27,1)
C
      CALL MKDPRS(GV,GX,GY,GZ,GV0,GX0,GY0,GZ0,GVD,GXD,GYD,GZD,
     $            CMD,CDD,COE1D,COE2D,INDP,INDU,INDV,INDW,
     $            ICHILD,IWES,IEAS,JSOU,JNOR,KBOT,KTOP)
C
      IF( LSEDI.EQ.1 ) THEN
         CALL MKIND5(GXBDH,GYBDH,KIBDH,KJBDH,GX0,GY0,INDP,INDU,INDV)
      ENDIF
C
C
C----------------------------------------------------------------------
C     (7) 時間積分計算
C----------------------------------------------------------------------
C
      ALLOCATE(UN(MX,MY,MZ),VN(MX,MY,MZ),WN(MX,MY,MZ),
     $         TN(MX,MY,MZ),CN(MX,MY,MZ),STAT=IERR)
      IF(IERR.NE.0) THEN
         CALL ERRMSG('NS_MAIN',6079)
         WRITE(LP,*) 'CANNOT ALLOCATE UN,...'
         CALL ABORT1('')
      END IF
      UN=0.d0
      VN=0.d0
      WN=0.d0
      TN=0.d0
      CN=0.d0
C
      ALLOCATE(AKN(MX,MY,MZ),EPN(MX,MY,MZ),AD0(MX,MY,MZ),
     $         AL0(3,MX,MY,MZ),AD(MX,MY,MZ),AL(3,MX,MY,MZ),
     $         AU(3,MX,MY,MZ),BB(MX,MY,MZ),DP(MX,MY,MZ),STAT=IERR)
      IF(IERR.NE.0) THEN
         CALL ERRMSG('NS_MAIN',6080)
         WRITE(LP,*) 'CANNOT ALLOCATE AKN,...'
         CALL ABORT1('')
      END IF
      AKN=0.d0
      EPN=0.d0
      AD0=0.d0
      AL0=0.d0
      AD=0.d0
      AL=0.d0
      AU=0.d0
      BB=0.d0
      DP=0.d0
C
      IF(LAIR.EQ.1) THEN
         NXY = MAX(MX,MY)
         ALLOCATE(UNA(MX,MY,MZA),VNA(MX,MY,MZA),WNA(MX,MY,MZA),
     $            FFA(MX,MY,MZA),FFNA(MX,MY,MZA),GVNA(MX,MY,MZA),
     $            HUA(MX,MY,MZA),HVA(MX,MY,MZA),HWA(MX,MY,MZA),
     $            TMUA(MX,MY,MZA),UUBCAIR(NXY,MZA,4),VVBCAIR(NXY,MZA,4),
     $            UUBCAIRB(NXY,MZA,4),VVBCAIRB(NXY,MZA,4),
     $            UUBCAIRF(NXY,MZA,4),VVBCAIRF(NXY,MZA,4),
     $            AD0A(MX,MY,MZA),AL0A(3,MX,MY,MZA),
     $            ADA(MX,MY,MZA),ALA(3,MX,MY,MZA),AUA(3,MX,MY,MZA),
     $            BBA(MX,MY,MZA),DPA(MX,MY,MZA),STAT=IERR)
      ELSE
         ALLOCATE(UNA(1,1,1),VNA(1,1,1),WNA(1,1,1),
     $            FFA(1,1,1),FFNA(1,1,1),GVNA(1,1,1),
     $            HUA(1,1,1),HVA(1,1,1),HWA(1,1,1),
     $            TMUA(1,1,1),UUBCAIR(1,1,4),VVBCAIR(1,1,4),
     $            UUBCAIRB(1,1,4),VVBCAIRB(1,1,4),
     $            UUBCAIRF(1,1,4),VVBCAIRF(1,1,4),
     $            AD0A(1,1,1),AL0A(3,1,1,1),
     $            ADA(1,1,1),ALA(3,1,1,1),AUA(3,1,1,1),
     $            BBA(1,1,1),DPA(1,1,1),STAT=IERR)
      ENDIF
      IF(IERR.NE.0) THEN
         CALL ERRMSG('NS_MAIN',6081)
         WRITE(LP,*) 'CANNOT ALLOCATE UNA,...'
         CALL ABORT1('')
      END IF
      UNA=UUA
      VNA=VVA
      WNA=WWA
      FFA=0.0D0
      FFNA=0.d0
      GVNA=0.d0
      HUA=0.0D0
      HVA=0.0D0
      HWA=0.0D0
      TMUA=0.0D0
      UUBCAIR=0.0D0
      VVBCAIR=0.0D0
      UUBCAIRB=0.0D0
      VVBCAIRB=0.0D0
      UUBCAIRF=0.0D0
      VVBCAIRF=0.0D0
      AD0A=0.d0
      AL0A=0.d0
      ADA=0.d0
      ALA=0.d0
      AUA=0.d0
      BBA=0.d0
      DPA=0.0D0

      IF(LAIR.EQ.1.AND.LTURBA.EQ.2) THEN
         ALLOCATE(AKNA(MX,MY,MZA),EPNA(MX,MY,MZA),
     $            AKBCAIR(NXY,MZA,4),EPBCAIR(NXY,MZA,4),
     $            AKBCAIRB(NXY,MZA,4),EPBCAIRB(NXY,MZA,4),
     $            AKBCAIRF(NXY,MZA,4),EPBCAIRF(NXY,MZA,4),
     $            STAT=IERR)
      ELSE
         ALLOCATE(AKNA(1,1,1),EPNA(1,1,1),
     $            AKBCAIR(1,1,4),EPBCAIR(1,1,4),
     $            AKBCAIRB(1,1,4),EPBCAIRB(1,1,4),
     $            AKBCAIRF(1,1,4),EPBCAIRF(1,1,4),
     $            STAT=IERR)
      ENDIF
      IF(IERR.NE.0) THEN
         CALL ERRMSG('NS_MAIN',6082)
         WRITE(LP,*) 'CANNOT ALLOCATE AKNA,...'
         CALL ABORT1('')
      END IF
      AKNA=AKA
      EPNA=EPA
      AKBCAIR=0.0D0
      EPBCAIR=0.0D0
      AKBCAIRB=0.0D0
      EPBCAIRB=0.0D0
      AKBCAIRF=0.0D0
      EPBCAIRF=0.0D0
C
      IF(IFALLW.EQ.1) THEN
         ALLOCATE(FALLWX(MX,MY),FALLWY(MX,MY),FALLWZ(MX,MY),
     $            DHX(MX,MY),DHY(MX,MY),
     $            CFALLWX(MX,MY),CFALLWY(MX,MY),DFALLWNX(MX,MY),
     $            DFALLWNY(MX,MY),DFALLWTX(MX,MY),DFALLWTY(MX,MY),
     $            STAT=IERR)
         CFALLWX(:,:)=CFALLW0
         CFALLWY(:,:)=CFALLW0
         DFALLWNX(:,:)=DFALLWN0
         DFALLWNY(:,:)=DFALLWN0
         DFALLWTX(:,:)=DFALLWT0
         DFALLWTY(:,:)=DFALLWT0
      ELSE
         ALLOCATE(FALLWX(1,1),FALLWY(1,1),FALLWZ(1,1),
     $            DHX(1,1),DHY(1,1),
     $            CFALLWX(1,1),CFALLWY(1,1),DFALLWNX(1,1),
     $            DFALLWNY(1,1),DFALLWTX(1,1),DFALLWTY(1,1),
     $            STAT=IERR)
         CFALLWX=0.d0
         CFALLWY=0.d0
         DFALLWNX=0.d0
         DFALLWNY=0.d0
         DFALLWTX=0.d0
         DFALLWTY=0.d0
      ENDIF
      IF(IERR.NE.0) THEN
         CALL ERRMSG('NS_MAIN',6083)
         WRITE(LP,*) 'CANNOT ALLOCATE FALLWX,...'
         CALL ABORT1('')
      END IF
      FALLWX=0.d0
      FALLWY=0.d0
      FALLWZ=0.d0
      DHX=0.d0
      DHY=0.d0
C
      ALLOCATE(WRK4(MX,MY,MZ1),WRK5(MX,MY,MZ1),WRK6(MX,MY,MZ1),
     $         WRK7(MX,MY,MZ1),WRK8(MX,MY,MZ),STAT=IERR)
      IF(IERR.NE.0) THEN
         CALL ERRMSG('NS_MAIN',6084)
         WRITE(LP,*) 'CANNOT ALLOCATE WRK4,...'
         CALL ABORT1('')
      END IF
      WRK4=0.d0
      WRK5=0.d0
      WRK6=0.d0
      WRK7=0.d0
      WRK8=0.d0
C
      CALL FTIMER(29,0)
      IF(LSEDI.EQ.0 .OR. MOFFLNSD.EQ.0)THEN
        CALL SOLVER(XC,XCP,YC,ZC,YCOS,YCOSP,YSIN,YSINP,
     $              XC_REF,YC_REF,
     $              XC_ML,YC_ML,ZC_ML,GV,GX,GY,GZ,
     $              GV0,GX0,GY0,GZ0,GVD,GXD,GYD,GZD,
     $              CMD,CDD,COE1D,COE2D,GX_ML,GY_ML,GZ_ML,
     $              UU,VV,WW,HU,HV,HW,PP,TT,CC,AK,EP,TMU,FF,RHOW,HH,
     $              TMUBW,TIMBW,
     $              PATM,DPS,QQ,QW,WX,WY,UN,VN,WN,TN,CN,AKN,EPN,RL,
     $              CSEDI,CSEDIN,CSDAVE,SHLSD,USSD,WEXSD,
     $              EXSDE,EXSDD,ZBED,ZBEDN,ZBED0,QBX,QBY,DZBUF,
     $              CSD_ML,ZBD_ML,CSDBCN,ZBDBCN,
     $              GXBDH,GYBDH,KIBDH,KJBDH,
     $              AD0,AL0,AD,AL,AU,BB,DP,
     $              WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,WRK8,VLEND,
     $              TMEND,FREND,FRIC,HDEP,AMNG,CD,HX,HY,HHW,RMMB,RMMF,
     $              INDP,INDU,INDV,INDW,INDK,
     $              INDP_ML,INDU_ML,INDV_ML,INDW_ML,
     $              KF,KG,KP,KH,KF_ML,KG_ML,IBUF,
     $              UU_ML,VV_ML,WW_ML,TT_ML,CC_ML,AK_ML,EP_ML,
     $              HH_ML,HDEP_ML,BUF,
     $              HHBCN,UUBCN,VVBCN,WWBCN,TTBCN,CCBCN,AKBCN,EPBCN,
     $              I_ML,J_ML,K_ML,I_NS,J_NS,K_NS,
     $              MX_ML,MY_ML,MZ_ML,
     $              IEAS_ML,IWES_ML,JSOU_ML,JNOR_ML,KBOT_ML,KTOP_ML,
     $              ZCA,GVA,GXA,GYA,GZA,
     $              PPA,UUA,VVA,WWA,UNA,VNA,WNA,AKA,EPA,
     $              TMUA,AKNA,EPNA,FFA,FFNA,GVNA,HUA,HVA,HWA,UUBCAIR,
     $              VVBCAIR,UUBCAIRB,VVBCAIRB,UUBCAIRF,VVBCAIRF,
     $              AKBCAIR,EPBCAIR,AKBCAIRB,EPBCAIRB,AKBCAIRF,EPBCAIRF,
     $              AD0A,AL0A,ADA,ALA,AUA,BBA,DPA,
     $              INDPA,INDUA,INDVA,INDWA,KFA,KFNA,
     $              FALLWX,FALLWY,FALLWZ,DHX,DHY,CFALLWX,CFALLWX,
     $              DFALLWNX,DFALLWNY,DFALLWTX,DFALLWTY)
      ELSE
        CALL SOLVER_OFFLINESD(INDP,INDU,INDV,INDW,INDP_ML,INDU_ML,
     $                        INDV_ML,INDW_ML,KF,KG,KF_ML,KG_ML,
     $                        KH,KP,MX_ML,MY_ML,MZ_ML,LLWALL,
     $                        LLWALP,LLWALB,I_ML,J_ML,K_ML,
     $                        I_NS,J_NS,K_NS,IEAS,IWES,JNOR,JSOU,
     $                        KBOT,KTOP,IEAS_ML,IWES_ML,
     $                        JNOR_ML,JSOU_ML,KBOT_ML,KTOP_ML,IBUF,
     $                        SHLSD,USSD,WEXSD,EXSDD,EXSDE,
     $                        QBX,QBY,CSEDI,CSEDIN,CSDAVE,
     $                        ZBED,ZBEDN,ZBED0,
     $                        CSD_ML,ZBD_ML,CSDBCN,ZBDBCN,
     $                        UU,VV,WW,HU,HV,HW,FF,HH,HX,
     $                        HDEP,HDEP_ML,HHBCN,
     $                        GV,GX,GY,GZ,GV0,GX0,GY0,GZ0,
     $                        GX_ML,GY_ML,GZ_ML,XC,XCP,
     $                        YC,YCOS,YCOSP,ZC,XC_REF,YC_REF,
     $                        XC_ML,YC_ML,ZC_ML,AMNG,TMU,
     $                        WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,
     $                        VLEND,TMEND,FREND,HHOFL,LLOFL,BUF,
     $                        GXBDH,GYBDH,KIBDH,KJBDH)
      ENDIF
      CALL FTIMER(29,1)
C
C
C----------------------------------------------------------------------
C     (8) 計算の終了
C----------------------------------------------------------------------
C
      WRITE(LP,*) '+----------------+'
      WRITE(LP,*) '|   NORMAL END   |'
      WRITE(LP,*) '+----------------+'
C
C ... 配列の解放
      DEALLOCATE(XC)
      DEALLOCATE(XCP)
      DEALLOCATE(XC_REF)
      DEALLOCATE(YC)
      DEALLOCATE(YC_REF)
      DEALLOCATE(ZC)
      DEALLOCATE(YCOS)
      DEALLOCATE(YCOSP)
      DEALLOCATE(YSIN)
      DEALLOCATE(YSINP)
      DEALLOCATE(XC_ML)
      DEALLOCATE(YC_ML)
      DEALLOCATE(ZC_ML)
      DEALLOCATE(INDP)
      DEALLOCATE(INDU)
      DEALLOCATE(INDV)
      DEALLOCATE(INDW)
      DEALLOCATE(INDK)
      DEALLOCATE(WRK1)
      DEALLOCATE(WRK2)
      DEALLOCATE(LLWALL)
      DEALLOCATE(LLWALP)
      DEALLOCATE(I_ML)
      DEALLOCATE(J_ML)
      DEALLOCATE(K_ML)
      DEALLOCATE(I_NS)
      DEALLOCATE(J_NS)
      DEALLOCATE(K_NS)
      DEALLOCATE(GV0)
      DEALLOCATE(GX0)
      DEALLOCATE(GY0)
      DEALLOCATE(GZ0)
      DEALLOCATE(GV)
      DEALLOCATE(GX)
      DEALLOCATE(GY)
      DEALLOCATE(GZ)
      DEALLOCATE(GVD)
      DEALLOCATE(GXD)
      DEALLOCATE(GYD)
      DEALLOCATE(GZD)
      DEALLOCATE(CMD)
      DEALLOCATE(CDD)
      DEALLOCATE(COE1D)
      DEALLOCATE(COE2D)
      DEALLOCATE(FRIC)
      DEALLOCATE(AMNG)
      DEALLOCATE(CD)
      DEALLOCATE(IBUF)
      DEALLOCATE(BUF)
      DEALLOCATE(INDU_ML)
      DEALLOCATE(INDV_ML)
      DEALLOCATE(INDW_ML)
      DEALLOCATE(INDP_ML)
      DEALLOCATE(GX_ML)
      DEALLOCATE(GY_ML)
      DEALLOCATE(GZ_ML)
      DEALLOCATE(LLWALB)
      DEALLOCATE(UU)
      DEALLOCATE(VV)
      DEALLOCATE(WW)
      DEALLOCATE(HU)
      DEALLOCATE(HV)
      DEALLOCATE(HW)
      DEALLOCATE(PP)
      DEALLOCATE(TT)
      DEALLOCATE(CC)
      DEALLOCATE(AK)
      DEALLOCATE(EP)
      DEALLOCATE(RL)
      DEALLOCATE(TMU)
      DEALLOCATE(FF)
      DEALLOCATE(RHOW)
      DEALLOCATE(VLEND)
      DEALLOCATE(TMEND)
      DEALLOCATE(FREND)
      DEALLOCATE(RMMB)
      DEALLOCATE(RMMF)
      DEALLOCATE(HH)
      DEALLOCATE(HX)
      DEALLOCATE(HY)
      DEALLOCATE(PATM)
      DEALLOCATE(WX)
      DEALLOCATE(WY)
      DEALLOCATE(DPS)
      DEALLOCATE(QQ)
      DEALLOCATE(QW)
      DEALLOCATE(HDEP)
      DEALLOCATE(KF)
      DEALLOCATE(KG)
      DEALLOCATE(KP)
      DEALLOCATE(KH)
      DEALLOCATE(UU_ML)
      DEALLOCATE(VV_ML)
      DEALLOCATE(WW_ML)
      DEALLOCATE(TT_ML)
      DEALLOCATE(CC_ML)
      DEALLOCATE(AK_ML)
      DEALLOCATE(EP_ML)
      DEALLOCATE(HH_ML)
      DEALLOCATE(KG_ML)
      DEALLOCATE(KF_ML)
      DEALLOCATE(HDEP_ML)
      DEALLOCATE(UUBCN)
      DEALLOCATE(VVBCN)
      DEALLOCATE(WWBCN)
      DEALLOCATE(TTBCN)
      DEALLOCATE(CCBCN)
      DEALLOCATE(AKBCN)
      DEALLOCATE(EPBCN)
      DEALLOCATE(HHBCN)
      DEALLOCATE(HHW)
      DEALLOCATE(UN)
      DEALLOCATE(VN)
      DEALLOCATE(WN)
      DEALLOCATE(TN)
      DEALLOCATE(CN)
      DEALLOCATE(AKN)
      DEALLOCATE(EPN)
      DEALLOCATE(AD0)
      DEALLOCATE(AL0)
      DEALLOCATE(AD)
      DEALLOCATE(AL)
      DEALLOCATE(AU)
      DEALLOCATE(BB)
      DEALLOCATE(DP)
      DEALLOCATE(WRK3)
      DEALLOCATE(WRK4)
      DEALLOCATE(WRK5)
      DEALLOCATE(WRK6)
      DEALLOCATE(WRK7)
      DEALLOCATE(WRK8)
C
      CALL FTIMER(1,1)
      WRITE(LP,600)
 600  FORMAT('#### CPU INFORMATION (SEC) ####')
      CPUIND0 = CPUSEC(9,1)+CPUSEC(10,1)+CPUSEC(11,1)
      CPUSND0 = CPUSEC( 5,1)+CPUSEC( 7,1)+CPUSEC(12,1)+CPUSEC(15,1)
     1        + CPUSEC(18,1)+CPUSEC(20,1)+CPUSEC(25,1)
      CPURCV0 = CPUSEC( 6,1)+CPUSEC( 8,1)+CPUSEC(13,1)+CPUSEC(16,1)
     1        + CPUSEC(19,1)+CPUSEC(21,1)+CPUSEC(26,1)
      CPUCOM0 = CPUSND0+CPURCV0+CPUSEC(2,1)
      CPUML2N = CPUSEC(42,1)+CPUSEC(52,1)
      CPUNS2M = CPUSEC(43,1)+CPUSEC(53,1)
      CPUSND1 = CPUSEC(40,1)+CPUSEC(44,1)
      CPURCV1 = CPUSEC(41,1)+CPUSEC(45,1)
      CPUCOM1 = CPUSND1+CPURCV1
      CPUSND2 = CPUSEC(50,1)+CPUSEC(54,1)
      CPURCV2 = CPUSEC(51,1)+CPUSEC(55,1)
      CPUCOM2 = CPUSND2+CPURCV2
      WRITE(LP,610) CPUSEC(1,1),CPUSEC(27,1),CPUSEC(3,1),CPUIND0,
     1              CPUSEC(17,1),CPUCOM0,CPUSND0,CPURCV0,
     2              CPUSEC(29,1),CPUSEC(28,1),CPUSEC(30,1),
     3              CPUSEC(33,1),CPUSEC(36,1),CPUSEC(37,1),
     3              CPUSEC(38,1),CPUSEC(39,1),CPUSEC(56,1),
     4              CPUSEC(57,1),CPUSEC(58,1),CPUSEC(48,1),
     5              CPUSEC(59,1),CPUSEC(60,1),
     6              CPUML2N,CPUSEC(42,1),CPUSEC(43,1),
     7              CPUNS2M,CPUSEC(43,1),CPUSEC(53,1),
     8              CPUCOM1,CPUSND1,CPURCV1,
     9              CPUCOM2,CPUSND2,CPURCV2,
     A              CPUSEC(71,1)+CPUSEC(72,1)+CPUSEC(73,1)+CPUSEC(74,1)
     B             +CPUSEC(75,1)+CPUSEC(76,1)+CPUSEC(77,1)+CPUSEC(78,1)
     C             +CPUSEC(79,1)
C
 610  FORMAT('     TOTAL             =',1PE12.5
     1      /'        PRE            =',  E12.5
     2      /'           INPUT       =',  E12.5
     *      /'           MKINDX      =',  E12.5
     *      /'           MKINIT      =',  E12.5
     3      /'           CPUCOM0     =',  E12.5
     *      /'           (SEND)      =',  E12.5
     *      /'           (RECV)      =',  E12.5
     4      /'        SOLVER         =',  E12.5
     5      /'        <TIME ZERO>    =',  E12.5
     6      /'        <TIME LOOP>    =',  E12.5
     7      /'           SETTBL      =',  E12.5
     8      /'           CLSURF      =',  E12.5
     A      /'           CLUEQ       =',  E12.5
     B      /'           CLVEQ       =',  E12.5
     C      /'           CLWEQ       =',  E12.5
     D      /'           BCSURF      =',  E12.5
     G      /'           CLENGY      =',  E12.5
     H      /'           CLCONC      =',  E12.5
     *      /'           CLPRES      =',  E12.5
     J      /'           CLTMIU      =',  E12.5
     K      /'           OUTPUT      =',  E12.5
     L      /'           CP_BCML2NS  =',  E12.5
     *      /'           (IFLAG=1)   =',  E12.5
     *      /'           (IFLAG=2)   =',  E12.5
     M      /'           CP_BCNS2ML  =',  E12.5
     *      /'           (IFLAG=1)   =',  E12.5
     *      /'           (IFLAG=2)   =',  E12.5
     N      /'           CPUCOM1     =',  E12.5
     *      /'           (SEND)      =',  E12.5
     *      /'           (RECV)      =',  E12.5
     O      /'           CPUCOM2     =',  E12.5
     *      /'           (SEND)      =',  E12.5
     *      /'           (RECV)      =',  E12.5
     P      /'           NEIBCOM     =',  E12.5 )
C
      CALL MPI_FINALIZE(IERR)
C
      STOP
C ... ファイルオープンエラー
  900 CONTINUE
      CALL ERRMSG('NS_MAIN',6085)
      WRITE(LP,*) 'FILE OPEN ERROR: DEBUG OUTPUT FILE'
      WRITE(LP,*) 'FILE NUMBER=',IFLDB
      CALL ABORT1('')
C
  901 CONTINUE
      CALL ERRMSG('NS_MAIN',6086)
      WRITE(LP,*) 'FILE OPEN ERROR: TC-INITIAL FILE'
      WRITE(LP,*) 'FILE NUMBER=',IFINI
      CALL ABORT1('')
      END
