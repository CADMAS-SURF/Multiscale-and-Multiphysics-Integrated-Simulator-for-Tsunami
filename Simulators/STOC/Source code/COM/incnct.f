      SUBROUTINE INCNCT(MLNS)
C======================================================================
C     計算領域の親子／兄弟関係と入力データファイル名を入力
C======================================================================
C
      use mod_comm,only: comm_group,comm_model
      IMPLICIT NONE
C
      INCLUDE 'mpif.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'FILE.h'
      INCLUDE 'GLOBAL.h'
      INCLUDE 'DRIFT.h'
      INCLUDE 'CADMAS.h'
      INCLUDE 'GRID.h'
      INCLUDE 'AUTODECOMP.h'
      INCLUDE 'DOMAIN.h'
C
      INTEGER,INTENT(INOUT)::MLNS
C
      INTEGER::PARENT,CHILD,EAST,WEST,NORTH,SOUTH
      CHARACTER(64)::NFILE
      INTEGER::ITRACE=1
C
      INTEGER::I,ID,IERR,IN,ISTOP,L,LL,M,MLFLG,N,NN,NUM,ICODE
      INTEGER::IDNEW,IRTRN,IDP
      INTEGER::KOUNT,MM,NNS,NNE,NNF
      INTEGER::NPROCA,GROUP,GROUPA,COMM,COMMA,RANKS(MAXPE)

      INTEGER::NPARNT(MAXPE)
      INTEGER::IREQ,ISTAT(MPI_STATUS_SIZE)
C
      INTEGER::IAUTODD(9,MAXPE)
C                     (1,*):自動分割前の領域番号
C                     (2,*):自動領域分割した領域内において自分が担当する部分領域の番号(≧1)
C                     (3,*):自動分割数NDIVX(=1のときは自動分割されていない)
C                     (4,*):自動分割数NDIVY(=1のときは自動分割されていない)
C                     (5,*):部分領域のX方向の範囲(開始点)
C                     (6,*):部分領域のX方向の範囲(終了点)
C                     (7,*):部分領域のY方向の範囲(開始点)
C                     (8,*):部分領域のY方向の範囲(終了点)
C                     (9,*):特殊な接続のフラグ(=0:通常時、=1:自分が球面座標で子が直交座標、=-1:CADMASと接続)
      REAL(8)::XAUTODD(4,MAXPE)
      REAL(8)::YAUTODD(4,MAXPE)
C                     (1,*):領域のX/Y座標の範囲(開始点)
C                     (2,*):領域のX/Y座標の範囲(終了点)
C                     (3,*):CHILD領域のX/Y座標の範囲(開始点)
C                     (4,*):CHILD領域のX/Y座標の範囲(終了点)
      REAL(8):: X1,X2,Y1,Y2,XP1,XP2,YP1,YP2
C
C
      CALL INIT_MPIENV(IERR)
C
      call flopen(6)
      write(6,*) 'nsize,nrank',nsize,nrank
      call flopen(lp)
C
      IF(NRANK.EQ.0) THEN
C
        IN = 51
        OPEN(IN,FILE='data.in',STATUS='OLD',FORM='FORMATTED',ERR=900)
C
        IDNEW=0
C
        DO 100 N=1,MAXPE
          READ(IN,*,END=200) ID,PARENT,CHILD,SOUTH,WEST,EAST,NORTH,
     $                        MLFLG,NFILE
          IF(ITRACE.NE.0)
     $    WRITE(6,810) ID,PARENT,CHILD,SOUTH,WEST,EAST,NORTH,
     $                MLFLG,trim(NFILE)
  810     FORMAT(I3,2(1X,I3),4(1X,I4),1X,I2,2X,A)
C
C ....... 計算条件ファイルの中のGRIDブロックを読みこみ、自動領域分割の指定を確認する
          OPEN(INP,FILE=NFILE,STATUS='OLD',FORM='FORMATTED',ERR=920)
          CALL INPUT(-1,IRTRN)
C                              IRTRNが自動領域分割数。分割しないときはIRTRN=1
          WRITE(LP,*) 'FILE,AUTO DECOMP.NUM.=',trim(NFILE),IRTRN
          CLOSE(INP)
C
C         東西南北のいずれかに兄弟領域が指定されているのに、自動分割が指定された場合
          IF( IRTRN.GT.1 .AND.
     $      (WEST.GT.0.OR.EAST.GT.0.OR.SOUTH.GT.0.OR.NORTH.GT.0) )THEN
             CALL ERRMSG('INCNCT',6560)
             WRITE(LP,*) '  FILE    = data.in'
             WRITE(LP,*) '    ID    = ',ID
             WRITE(LP,*) '  FILE    = ',trim(NFILE)
             WRITE(LP,*) '    NDIVX = ',NDIVX
             WRITE(LP,*) '    NDIVY = ',NDIVY
             WRITE(LP,*) 'DEFINITION OF BOTH MANUAL DECOMPOSITION',
     $              ' AND AUTOMATIC DECOMPOSITION ARE NOT ALLOWED'
             CALL ABORT1('')
          ENDIF
C
          DO M=1,IRTRN
             IDNEW=IDNEW+1
C
             IDTABL(1,IDNEW) = IDNEW
             IDTABL(2,IDNEW) = IDNEW-1
             IPECON(1,IDNEW) = IDNEW-1
             IPECON(2,IDNEW) = PARENT
             IPECON(3,IDNEW) = CHILD
             IPECON(4,IDNEW) = SOUTH
             IPECON(5,IDNEW) = WEST
             IPECON(6,IDNEW) = EAST
             IPECON(7,IDNEW) = NORTH
             IPECON(8,IDNEW) = MLFLG
             NMFILE(IDNEW) = NFILE
C
C .......... FOR AUTO DOMAIN DECOMPOSITIN
             IAUTODD(1,IDNEW)=ID
             IAUTODD(2,IDNEW)=M
             IAUTODD(3,IDNEW)=NDIVX
             IAUTODD(4,IDNEW)=NDIVY
             IAUTODD(5,IDNEW)=IDIV1(MOD(M-1,NDIVX)+1)
             IAUTODD(6,IDNEW)=IDIV2(MOD(M-1,NDIVX)+1)
             IAUTODD(7,IDNEW)=JDIV1((M-1)/NDIVX+1)
             IAUTODD(8,IDNEW)=JDIV2((M-1)/NDIVX+1)
             IAUTODD(9,IDNEW)=ICRDC
comout 20131023             IF( CHILD.LT.0.AND.CHILD.GT.-10 ) IAUTODD(9,IDNEW)=-1
             IF( CHILD.LT.0.AND.CHILD.GT.-10 ) IAUTODD(9,IDNEW)=CHILD
C
             XAUTODD(1,IDNEW)=XDIV1(MOD(M-1,NDIVX)+1)
             XAUTODD(2,IDNEW)=XDIV2(MOD(M-1,NDIVX)+1)
             YAUTODD(1,IDNEW)=YDIV1((M-1)/NDIVX+1)
             YAUTODD(2,IDNEW)=YDIV2((M-1)/NDIVX+1)
C
C .......... SPECIAL CASE
comout 20131023             IF( IAUTODD(9,IDNEW).EQ.-1 ) THEN
             IF( IAUTODD(9,IDNEW).LT.0.AND.
     $           IAUTODD(9,IDNEW).GT.-10 ) THEN
                XAUTODD(3,IDNEW)=XCAD1
                XAUTODD(4,IDNEW)=XCAD2
                YAUTODD(3,IDNEW)=YCAD1
                YAUTODD(4,IDNEW)=YCAD2
             ELSEIF( IAUTODD(9,IDNEW).EQ.1 ) THEN
                XAUTODD(3,IDNEW)=REGN(1)
                XAUTODD(4,IDNEW)=REGN(3)
                YAUTODD(3,IDNEW)=REGN(2)
                YAUTODD(4,IDNEW)=REGN(4)
             ENDIF
          ENDDO
  100   CONTINUE
  200   CONTINUE
        CLOSE(IN)
C
        ISTOP = 0
        IF(IDNEW.NE.NSIZE) THEN
          CALL ERRMSG('INCNCT',6561)
          WRITE(LP,*) '### NUM. OF DOMAIN .NE. NUM. OF PE ###',
     $                 IDNEW,NSIZE
          WRITE(LP,*) '    STOP SUB. INCNCT   '
          ISTOP = 1
          CALL ABORT1('')
        END IF
C
C
C ..... 東西南北の領域番号を修正
        DO N=1,NSIZE
           M    =IAUTODD(2,N)
           NDIVX=IAUTODD(3,N)
           NDIVY=IAUTODD(4,N)
C
C          自動分割の指定ありの場合
           IF(NDIVX*NDIVY.GT.1)THEN
              WEST  = -99
              EAST  = -99
              SOUTH = -99
              NORTH = -99
              IF(NDIVX.GT.1.AND.MOD(M-1,NDIVX).NE.0      ) WEST =N-1
              IF(NDIVX.GT.1.AND.MOD(M-1,NDIVX).NE.NDIVX-1) EAST =N+1
              IF(NDIVY.GT.1.AND.(M-1)/NDIVX   .NE.0      ) SOUTH=N-NDIVX
              IF(NDIVY.GT.1.AND.(M-1)/NDIVX   .NE.NDIVY-1) NORTH=N+NDIVX
C
              IPECON(4,N)=SOUTH
              IPECON(5,N)=WEST
              IPECON(6,N)=EAST
              IPECON(7,N)=NORTH
C
C          自動分割の指定なしの場合
           ELSE
              DO L=4,7        ! S, W, E, N
                 ID = IPECON(L,N)
                 IF( ID.GT.0 ) THEN
                    DO NN=1,NSIZE
                       IF( ID.EQ.IAUTODD(1,NN) ) THEN
                          ID=IDTABL(1,NN)
                          EXIT
                       ENDIF
                    ENDDO
                    IPECON(L,N)=ID
                 ENDIF
              ENDDO
           ENDIF
        ENDDO
C
C ..... 親子の領域番号を修正
        DO N=1,NSIZE
           NDIVX=IAUTODD(3,N)
           NDIVY=IAUTODD(4,N)
           IF(NDIVX*NDIVY.GT.1)THEN
comout 20131023              IPECON(3,N)=-99
              IF( IPECON(3,N).GT.0 ) IPECON(3,N)=-99
           ENDIF
        ENDDO
C
        DO N=NSIZE,1,-1
           IDP=IPECON(2,N)
C
C ........ まず、親のID(IDP)を決める
           IF( IDP.GT.0 ) THEN
              DO NN=1,NSIZE
                 IF( IDP.EQ.IAUTODD(1,NN) ) THEN
C
                    NDIVX=IAUTODD(3,NN)
                    NDIVY=IAUTODD(4,NN)
                    IF( NDIVX*NDIVY.EQ.1 ) THEN
                       IDP=IDTABL(1,NN)
                       EXIT
                    ELSE
C .................  area check
                       X1=XAUTODD(1,N)
                       X2=XAUTODD(2,N)
                       Y1=YAUTODD(1,N)
                       Y2=YAUTODD(2,N)
                       XP1=XAUTODD(1,NN)
                       XP2=XAUTODD(2,NN)
                       YP1=YAUTODD(1,NN)
                       YP2=YAUTODD(2,NN)
                       IF(IAUTODD(9,NN).EQ.1)THEN
                          X1=XAUTODD(3,NN)
                          X2=XAUTODD(4,NN)
                          Y1=YAUTODD(3,NN)
                          Y2=YAUTODD(4,NN)
                       ENDIF
                       IF( X1.GE.XP1.AND.X2.LE.XP2.AND.
     $                     Y1.GE.YP1.AND.Y2.LE.YP2 ) THEN
                          IDP=IDTABL(1,NN)
                          EXIT
                       ENDIF
                    ENDIF
                 ENDIF
              ENDDO
              IF(NN.EQ.NSIZE+1) THEN
                 CALL ERRMSG('INCNCT',6562)
                 WRITE(LP,*) '  PARENT ID OR AREA IS ILLEGAL'
                 WRITE(LP,*) '  FILE    = data.in'
                 WRITE(LP,*) '    MY     ID = ',IAUTODD(1,N)
                 WRITE(LP,*) '    PARENT ID = ',IDP
                 CALL ABORT1('')
              ENDIF
C
C ........... 親のID
              IPECON(2,N)=IDP
C
C ........... 親から見たときの子のID
              IPECON(3,NN)=N
C                 NのDOループを逆から回しているので、子が複数領域に分割されて
C                 いる場合も最終的に先頭の子が選択される
           ENDIF
        ENDDO
C
C ..... CADMASの親領域を修正
comout 20131023        DO N=1,NSIZE
comout 20131023comout 20131023           IF( IAUTODD(9,N).EQ.-1 ) THEN
comout 20131023           IF( IAUTODD(9,N).LT.0.AND.IAUTODD(9,N).GT.-10 ) THEN
comout 20131023              NDIVX=IAUTODD(3,N)
comout 20131023              NDIVY=IAUTODD(4,N)
comout 20131023C
comout 20131023C          自動分割の指定ありの場合
comout 20131023              IF( NDIVX*NDIVY.GT.1 ) THEN
comout 20131023                 IPECON(3,N)=-99
comout 20131023C ..............  area check
comout 20131023                 XP1=XAUTODD(1,N)
comout 20131023                 XP2=XAUTODD(2,N)
comout 20131023                 YP1=YAUTODD(1,N)
comout 20131023                 YP2=YAUTODD(2,N)
comout 20131023C
comout 20131023                 X1=XAUTODD(3,N)
comout 20131023                 X2=XAUTODD(4,N)
comout 20131023                 Y1=YAUTODD(3,N)
comout 20131023                 Y2=YAUTODD(4,N)
comout 20131023C
comout 20131023                 IF( X1.GE.XP1.AND.X2.LE.XP2.AND.
comout 20131023     $               Y1.GE.YP1.AND.Y2.LE.YP2 ) THEN
comout 20131023                    IPECON(3,N)=-1
comout 20131023                    EXIT
comout 20131023                 ENDIF
comout 20131023              ENDIF
comout 20131023           ENDIF
comout 20131023        ENDDO
C
C ...... OUTPUT data.in_debug
        IN = 52
        OPEN(IN,FILE='data.in_debug',FORM='FORMATTED',ERR=900)
        DO N=1,NSIZE
           WRITE(IN,810) N,(IPECON(I,N),I=2,8),trim(NMFILE(N))
        ENDDO
        CLOSE(IN)
C
        DO 300 N=1,NSIZE
          DO 310 L=2,7
            ID = IPECON(L,N)
            IF(ID.LT.0) THEN
              IF(L.NE.3) THEN
                 IPECON(L,N) = - 1
              ELSEIF(ID.GT.-10) THEN
                 WRITE(*,*) '-9 - -1 IS SET AS CHILD NUMBER IN data.in,'
                 WRITE(*,*) ' STOC-IC IS COUPLED WITH CADMAS-SURF' 
              ENDIF
              GO TO 310
            END IF
            DO 320 M=1,NSIZE
              IF(ID.EQ.IDTABL(1,M)) THEN
                IPECON(L,N) = IDTABL(2,M)
                GO TO 310
              END IF
  320       CONTINUE
            WRITE(LP,*) '### PE NO. ###',N,'### ID OF DOMAIN =',
     $                  ID,'     HAS NO PE ###'
            ISTOP = 1
  310     CONTINUE
  300   CONTINUE
C
        DO 400 N=1,NSIZE
          NPARNT(N) = 0
          NUMPE(1,N) = 0
          NUMPE(2,N) = 0
          DO 410 L=1,5
            NUMCOM(L,N) = 0
  410     CONTINUE
  400   CONTINUE
        NUMPE(1,0) = 0
        NUMPE(2,0) = 0
C
        NNS = 1
        DO 420 N=1,NSIZE
          IF(NPARNT(N).EQ.1) GO TO 420
C
          IF(N.EQ.1) THEN
            NN = - 1
            IF(IPECON(2,N).GE.0) THEN
              WRITE(LP,*) '### PE NO. = 0  HAS PARENT PE NO. =',
     $                     IPECON(2,N)
              ISTOP =1
            END IF
          ELSE
            NN = IPECON(2,N)
          END IF
          KOUNT = 0
          NPARNT(N) = 1
C
          DO 430 L=N+1,NSIZE
            IF(NPARNT(L).EQ.0.AND.NN.EQ.IPECON(2,L)) THEN
              KOUNT = KOUNT+1
              IF(KOUNT.EQ.1) THEN
C .... 最初の点の登録
                NUMPE(2,NN+1) = NNS
                NUMCOM(1,NNS) = IPECON(1,N)
                KOUNT = KOUNT+1
              END IF
              NUMCOM(1,NNS+KOUNT-1) = IPECON(1,L)
              NPARNT(L) = 1
            END IF
  430     CONTINUE
C
          NUMPE(1,NN+1) = KOUNT
          IF(NN.EQ.-1.AND.KOUNT.EQ.0) NUMPE(1,NN+1)=1
          IF(NN.NE.-1.AND.KOUNT.EQ.0) THEN
            NUMPE(1,NN+1)=1
            NUMPE(2,NN+1)=NNS
            NUMCOM(1,NNS) = IPECON(1,N)
            KOUNT = 1
          END IF
          NNS = NNS+KOUNT
  420   CONTINUE
C
        DO 440 N = 0,NSIZE
C          IF(NUMPE(1,N).GE.1) THEN
          IF(NUMPE(1,N).GT.1) THEN
            NNS = NUMPE(2,N)
            NNE = NNS+NUMPE(1,N)-1
            DO 450 L=NNS,NNE
              NN = NUMCOM(1,L)
              DO 460 MM=1,4
                NNF = 1
                IF(IPECON(3+MM,NN+1).GE.0) NNF=0
                NUMCOM(1+MM,L) = NNF
  460         CONTINUE
  450       CONTINUE
          END IF
C
          IF(N.EQ.0) GO TO 440
          NN = IPECON(1,N)
          LL = 0
          DO 470 L=1,NSIZE
            IF(L.NE.N) THEN
              IF(IPECON(3,L).EQ.NN) LL=LL+1
            END IF
  470     CONTINUE
          IF(LL.GT.1) THEN
            WRITE(LP,*) '### PE NO. =',N,'  HAS ',LL,' PARENTS ###'
            ISTOP =1
          END IF
C
  440   CONTINUE
C
        IF(ISTOP.NE.0) THEN
           CALL ERRMSG('INCNCT',6563)
           CALL ABORT1('')
        ENDIF
C
      ENDIF
C                     全データを送信
      NUM = 8*MAXPE
      CALL MPI_BCAST( IPECON,NUM,MPI_INTEGER,0,comm_group,IERR )
      NUM = 2*MAXPE
      CALL MPI_BCAST( IDTABL,NUM,MPI_INTEGER,0,comm_group,IERR )
      NUM = 64*MAXPE
      CALL MPI_BCAST( NMFILE,NUM,MPI_CHARACTER,0,comm_group,IERR )
      NUM = 2*(MAXPE+1)
      CALL MPI_BCAST( NUMPE,NUM,MPI_INTEGER,0,comm_group,IERR )
      NUM = 5*MAXPE
      CALL MPI_BCAST( NUMCOM,NUM,MPI_INTEGER,0,comm_group,IERR )
C
      NUM = 9*MAXPE
      CALL MPI_BCAST( IAUTODD,NUM,MPI_INTEGER,0,comm_group,IERR )
C
      IF( IPECON(3,NRANK+1).LT.0.AND.IPECON(3,NRANK+1).GT.-10 ) THEN
         NB_SC = -IPECON(3,NRANK+1)                       !################ CADMAS SETTING #############
comout 20131023         IPECON(3,NRANK+1) = -1
      ELSE
         NB_SC = 0
      ENDIF
C
      IAUTOD=0
      MYPROC=1
      NDIVX=IAUTODD(3,NRANK+1)
      NDIVY=IAUTODD(4,NRANK+1)
      MYIS=2
      MYJS=2
C      MYIE=MXM
C      MYJE=MYM
      MYIE=IAUTODD(6,NRANK+1)
      MYJE=IAUTODD(8,NRANK+1)
      IF( NDIVX*NDIVY.GT.1 ) THEN
         IAUTOD=1
         MYPROC=IAUTODD(2,NRANK+1)
         MYIS  =IAUTODD(5,NRANK+1)
         MYIE  =IAUTODD(6,NRANK+1)
         MYJS  =IAUTODD(7,NRANK+1)
         MYJE  =IAUTODD(8,NRANK+1)
      ENDIF
C
C                   計算モデルと入力データをチェックする
C
      WRITE(*,1) NRANK,(IPECON(I,NRANK+1),I=1,8),NMFILE(NRANK+1)
      WRITE(LP,1) NRANK,(IPECON(I,NRANK+1),I=1,8),NMFILE(NRANK+1)
 1    FORMAT('PE NO.=',I3,'   PARENT  CHILD  SOUTH WEST  EAST  NORTH',
     $                    '  MODEL     FILE NAME',
     $      /'IPECON=',I3,3I7,I6,I7,I6,I7,6X,A12)
      ISTOP = 0
      IF(MLNS.NE.IPECON(8,NRANK+1)) THEN
        CALL ERRMSG('INCNCT',6564)
        WRITE(LP,*) '### MODEL TYPE ERROR ###   PE NO.',NRANK
        WRITE(LP,*) '    MODEL TYPE =',MLNS,
     $              '    DATA TYPE =',IPECON(8,NRANK+1)
        CALL ABORT1('')
      END IF
C                   領域データをセットする
      IDCON(1) = IDTABL(1,NRANK+1)
      DO 500 N=2,7
        IF(IPECON(N,NRANK+1).GE.0) THEN
          IDCON(N) = IDTABL(1,IPECON(N,NRANK+1)+1)
        ELSE
          IDCON(N) = - 1
        END IF
  500 CONTINUE
      WRITE(LP,2) (IDCON(I),I=1,7)
  2    FORMAT('IDCON =',I3,3I7,I6,I7,I6)
C
      NN = IPECON(2,NRANK+1)
      NPROC = NUMPE(1,NN+1)
C
      DO 530 N=1,NSIZE
        NPARNT(N) = 0
  530 CONTINUE
C
      MM = 0
  505 CONTINUE
      MM = MM+1
      DO 510 N=MM,NSIZE
        NN = IPECON(2,N)
        NPROCA = NUMPE(1,NN+1)
        IF(NPARNT(N).EQ.0.AND.NPROCA.GT.1) GO TO 515
  510 CONTINUE
      GO TO 910
C
  515 CONTINUE
      MM = N
      NNS = NUMPE(2,NN+1)
      NNE = NNS+NUMPE(1,NN+1)-1
      DO 520 L=NNS,NNE
        RANKS(L-NNS+1) = NUMCOM(1,L)
  520 CONTINUE
C
      COMM = comm_model
      CALL MPI_COMM_GROUP(COMM,GROUP,IERR)
c      CHILDCOMM = 0
      CALL MPI_GROUP_INCL(GROUP,NPROCA,RANKS,GROUPA,IERR)
      CALL MPI_COMM_CREATE(COMM,GROUPA,COMMA,IERR)
      DO 540 L=NNS,NNE
        LL = NUMCOM(1,L)
        NPARNT(LL+1) = 1
        IF(NRANK.EQ.NUMCOM(1,L)) CHILDCOMM=COMMA
  540 CONTINUE
      write(6,*) 'nrank,nproc,childcomm=',nrank,nproc,childcomm
      GO TO 505
C
  910 CONTINUE
      RETURN
C
  900 CONTINUE
      CALL ERRMSG('INCNCT',6565)
      WRITE(LP,*) 'FILE OPEN ERROR: PARALLEL DATA FILE data.in'
      CALL ABORT1('')
C
  920 CONTINUE
      CALL ERRMSG('INCNCT',6566)
      WRITE(LP,*) 'FILE OPEN ERROR: ',trim(NFILE)
      CALL ABORT1('')
      END
