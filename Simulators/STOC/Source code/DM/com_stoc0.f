      SUBROUTINE COM_STOC0
C----------------------------------------
C     STOC との初期通信
C
C     <サブルーチン構造>
C        input.datのarea_dataの読み込み
C
C        if( online ) then
C           area_dataの情報をSTOCへ送信
C
C           STOCの領域設定情報の受信
C
C           STOCの領域サイズの受信
C
C           if( off_output ) then
C              ファイル出力
C           endif
C        else
C           ファイル読み込み
C        endif
C----------------------------------------
      use mod_comm,only: nsize_all,nrank_all,l_model,l_stoc_ml
     $                  ,l_stoc_ic,l_stoc_dm
     $                  ,comm_mlicds_dm,comm_group
      USE M_GRID,ONLY: MXAREA,MXNI,MXNJ,MXNK,MXNIJ,MXNIJK
     $                ,NI,NJ,NK,ISUB,JSUB,KSUB
      USE M_COM_STOC,ONLY:MAXPE,NSIZE,NRANK
     $   ,IDCON,IPECON,IDTABL,NUMPE,NUMCOM,NMFILE
     $   ,IB_SD,NP_STOC,IP_STOC,IP_STOC_MAIN,IA_STOC,NC_STOC,IC_STOC
      USE M_OUTPUT,ONLY:IFL
      USE M_FILEIN,ONLY:NOCAL,OFF_INTERVAL,IFLOFF,IFLWIN
     $   ,OFF_OUTPUT,ONLINE,OFFLINE,OFF_START
C
      IMPLICIT NONE
C
      INCLUDE 'mpif.h'
C
      INTEGER::NUM,N,NN,L
      INTEGER::IREQ1,ISTAT(MPI_STATUS_SIZE)
      INTEGER::IERR
      INTEGER::M,ICOUNT,NP,NP2,IEAST,IWEST,INORT,ISOUT
      INTEGER::IPARENT,IPARENT2,ICHILD,NC,NC2
C
      LOGICAL::AREA(MAXPE)
      NAMELIST /AREA_DATA/ AREA
      INTEGER::IWORK(3),NI1,NJ1,NK1,IS,JS,KS
      integer::tmpbuf(9*maxpe)
C
C
C----------------------------------------
C     AREA_DATAの処理
C----------------------------------------
      AREA(1:MAXPE)=.FALSE.
      OPEN (11,FILE='./drift/input.dat',
     &                             STATUS='OLD',FORM='FORMATTED',ERR=90)
      READ(11,AREA_DATA,IOSTAT=IERR)
      IF( IERR>0 ) THEN
         WRITE(*,*) 'UNRECOGNIZED VARIABLE IS FOUND IN /AREA_DATA/'
         CALL ERRMSG('COM_STOC0',-8)
      ELSEIF( IERR<0 ) THEN
         WRITE(*,*) 'NAMELIST /AREA_DATA/ IS NOT FOUND'
         WRITE(*,*) 'THIS ITEM CANNOT BE OMITTED'
         CALL ERRMSG('COM_STOC0',-9)
      ENDIF
      CLOSE(11)
      WRITE(IFL,AREA_DATA)
C
C----------------------------------------
C     オンライン計算用処理
C----------------------------------------
      IF( ONLINE ) THEN
C
      NP_STOC=0
      IFLOFF(0)=100
      DO NN=1,NSIZE
         IF( AREA(NN) ) THEN
            IB_SD(NN)=1
            IFLOFF(NN)=100+NN
            IFLWIN(NN)=200+NN
            NP_STOC=NP_STOC+1
            IP_STOC(NP_STOC)=NN-1
C
            IF( NP_STOC.EQ.1 ) THEN
               IB_SD(NN)=2
               IP_STOC_MAIN=NN-1
            ENDIF
         ELSE
            IB_SD(NN)=0
         ENDIF
C
         CALL MPI_ISEND(IB_SD(NN),1,MPI_INTEGER,NN-1,NN-1,
     $                  comm_mlicds_dm,IREQ1,IERR)
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         CALL MPI_ISEND(NOCAL,1,MPI_LOGICAL,NN-1,NN-1,
     $                  comm_mlicds_dm,IREQ1,IERR)
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         CALL MPI_ISEND(OFF_INTERVAL,1,MPI_DOUBLE_PRECISION,NN-1,NN-1,
     $                  comm_mlicds_dm,IREQ1,IERR)
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         CALL MPI_ISEND(OFF_START,1,MPI_DOUBLE_PRECISION,NN-1,NN-1,
     $                  comm_mlicds_dm,IREQ1,IERR)
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
      ENDDO
C
C
C----------------------------------------
C     STOC-ML,IC側の領域設定情報の受信と設定
C----------------------------------------
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
        CALL MPI_BCAST( tmpbuf,NUM,MPI_INTEGER,0,comm_group,IERR )
C
C
C----------------------------------------
C     領域分割されている領域のチェック
C----------------------------------------
      IA_STOC=0
      ICOUNT=0
C
C ... 兄弟PEのチェックと領域数の設定
      DO N=1,NP_STOC
         IF( IA_STOC(N).GT.0 ) CYCLE
C
         NP=IP_STOC(N)
         ICOUNT=ICOUNT+1
         IA_STOC(N)=ICOUNT
         IPARENT=MAX(IPECON(2,NP+1),-1)
C
         DO M=N+1,NP_STOC
            NP2=IP_STOC(M)
            IPARENT2=MAX(IPECON(2,NP2+1),-1)
            IF( IPARENT.EQ.IPARENT2 ) THEN
               IA_STOC(M)=ICOUNT
            ENDIF
         ENDDO
C
         ISOUT=IPECON(4,NP+1)
         IF( ISOUT.GT.0 ) THEN
            IERR=ISOUT
            DO M=1,NP_STOC
               IF( ISOUT.EQ.IP_STOC(M) ) THEN
                  IERR=0
                  EXIT
               ENDIF
            ENDDO
            IF( IERR.GT.0 ) GOTO 900
         ENDIF
      ENDDO
C
         IWEST=IPECON(5,NP+1)
         IF( IWEST.GT.0 ) THEN
            IERR=IWEST
            DO M=1,NP_STOC
               IF( IWEST.EQ.IP_STOC(M) ) THEN
                  IERR=0
                  EXIT
               ENDIF
            ENDDO
            IF( IERR.GT.0 ) GOTO 900
         ENDIF
C
         IEAST=IPECON(6,NP+1)
         IF( IEAST.GT.0 ) THEN
            IERR=IEAST
            DO M=1,NP_STOC
               IF( IEAST.EQ.IP_STOC(M) ) THEN
                  IERR=0
                  EXIT
               ENDIF
            ENDDO
            IF( IERR.GT.0 ) GOTO 900
         ENDIF
C
         INORT=IPECON(7,NP+1)
         IF( INORT.GT.0 ) THEN
            IERR=INORT
            DO M=1,NP_STOC
               IF( INORT.EQ.IP_STOC(M) ) THEN
                  IERR=0
                  EXIT
               ENDIF
            ENDDO
            IF( IERR.GT.0 ) GOTO 900
         ENDIF
C
      MXAREA=ICOUNT
      write(IFL,*) 'np_stoc,mxarea=',np_stoc,MXAREA
      do n=1,np_stoc
         write(IFL,*) 'ip_stoc,ia_stoc=',ip_stoc(n),ia_stoc(n)
      enddo
C
C
C ... 親子関係のチェック
      ICOUNT=0
      IC_STOC(:,:)=0
C
      DO N=1,MXAREA
         DO M=1,NP_STOC
            NP=IP_STOC(M)
            NC=IA_STOC(M)
C
            IF( NC.EQ.N ) THEN
               ICHILD=IPECON(3,NP+1)
C
               DO L=1,NP_STOC
                  NP2=IP_STOC(L)
                  NC2=IA_STOC(L)
                  IF( ICHILD.EQ.NP2 ) THEN
                     ICOUNT=ICOUNT+1
                     IC_STOC(1,ICOUNT)=NC
                     IC_STOC(2,ICOUNT)=NC2
                     EXIT
                  ENDIF
               ENDDO
C
            ENDIF
         ENDDO
      ENDDO
C
      NC_STOC=ICOUNT
C
      write(IFL,*) 'nc_stoc=',nc_stoc
      do n=1,nc_stoc
      write(IFL,*) 'ic_stoc(',n,')=',ic_stoc(1,n),ic_stoc(2,n)
      enddo
C
C
C ... 領域サイズを受信する
      DO N=1,NP_STOC
         NP=IP_STOC(N)
         CALL MPI_IRECV(IWORK,3,MPI_INTEGER,NP,MPI_ANY_TAG,
     $                  comm_mlicds_dm,IREQ1,IERR) !  NI,NJ,NK の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         ISUB(3,N)=IWORK(1)  ! N番目の流体側部分領域のX方向のサイズ
         JSUB(3,N)=IWORK(2)  ! N番目の流体側部分領域のY方向のサイズ
         KSUB(3,N)=IWORK(3)  ! N番目の流体側部分領域のZ方向のサイズ
      ENDDO
C
      MXNI=0
      MXNJ=0
      MXNK=0
      MXNIJ=0
      MXNIJK=0
      DO N=1,MXAREA
         NI1=0
         NJ1=0
         NK1=0
         IS=1
         JS=1
         KS=1
         DO M=1,NP_STOC
            NC=IA_STOC(M)
            IF( NC.EQ.N ) THEN        ! 領域Nに属するPEについてのみ処理を行う
               NP=IP_STOC(M)
               IF( IPECON(4,NP+1).LT.0 ) THEN ! 南側に領域分割境界なし
                  NI1=NI1+ISUB(3,M)
               ENDIF
               IF( IPECON(5,NP+1).LT.0 ) THEN ! 西側に領域分割境界なし
                  NJ1=NJ1+JSUB(3,M)
               ENDIF
               NK1=KSUB(3,M)
C
               ISUB(1,M)=IS                   ! M番目の流体側部分領域の,領域N内でのX方向の開始番号
               JSUB(1,M)=JS                   ! M番目の流体側部分領域の,領域N内でのY方向の開始番号
               KSUB(1,M)=KS                   ! M番目の流体側部分領域の,領域N内でのZ方向の開始番号(1固定)
               ISUB(2,M)=IS+ISUB(3,M)-1       ! M番目の流体側部分領域の,領域N内でのX方向の終了番号
               JSUB(2,M)=JS+JSUB(3,M)-1       ! M番目の流体側部分領域の,領域N内でのY方向の終了番号
               KSUB(2,M)=KS+KSUB(3,M)-1       ! M番目の流体側部分領域の,領域N内でのZ方向の終了番号
C
               IS=IS+ISUB(3,M)                ! ISを増加
               IF( IPECON(6,NP+1).LT.0 ) THEN ! 東側に領域分割境界なし(折り返し)でISをリセット,JSを増加
                  IS=1
                  JS=JS+JSUB(3,M)
               ENDIF
            ENDIF
            NI(N)=NI1
            NJ(N)=NJ1
            NK(N)=NK1
         ENDDO
         MXNI=MAX(MXNI,NI1)
         MXNJ=MAX(MXNJ,NJ1)
         MXNK=MAX(MXNK,NK1)
         MXNIJ=MAX(MXNIJ,NI1*NJ1)
         MXNIJK=MAX(MXNIJK,NI1*NJ1*NK1)
      ENDDO
C
      IF( OFF_OUTPUT ) THEN
         OPEN(IFLOFF(0),FILE='./drift/offline_ctl.dat',
     $        STATUS='NEW',FORM='UNFORMATTED',ERR=91)
C
         WRITE(IFLOFF(0),ERR=93) NSIZE,NP_STOC,NC_STOC,IP_STOC_MAIN
         WRITE(IFLOFF(0),ERR=93) (IB_SD(N),N=1,NSIZE)
         WRITE(IFLOFF(0),ERR=93) (IP_STOC(N),N=1,NP_STOC)
         WRITE(IFLOFF(0),ERR=93) (IA_STOC(N),N=1,NP_STOC)
         WRITE(IFLOFF(0),ERR=93) ((IC_STOC(M,N),M=1,2),N=1,NC_STOC)
C
         WRITE(IFLOFF(0),ERR=93) MXAREA,MXNI,MXNJ,MXNK,MXNIJ,MXNIJK
         WRITE(IFLOFF(0),ERR=93) (NI(N),N=1,MXAREA)
         WRITE(IFLOFF(0),ERR=93) (NJ(N),N=1,MXAREA)
         WRITE(IFLOFF(0),ERR=93) (NK(N),N=1,MXAREA)
         WRITE(IFLOFF(0),ERR=93) ((ISUB(M,N),M=1,3),N=1,NP_STOC)
         WRITE(IFLOFF(0),ERR=93) ((JSUB(M,N),M=1,3),N=1,NP_STOC)
         WRITE(IFLOFF(0),ERR=93) ((KSUB(M,N),M=1,3),N=1,NP_STOC)
      ENDIF
C
C----------------------------------------
C     オフライン計算用処理
C----------------------------------------
      ELSE
         IFLOFF(0)=100
         OPEN(IFLOFF(0),FILE='./drift/offline_ctl.dat',
     $        STATUS='OLD',FORM='UNFORMATTED',ERR=92)
C
         READ(IFLOFF(0),ERR=94) NSIZE,NP_STOC,NC_STOC,IP_STOC_MAIN
         READ(IFLOFF(0),ERR=94) (IB_SD(N),N=1,NSIZE)
         READ(IFLOFF(0),ERR=94) (IP_STOC(N),N=1,NP_STOC)
         READ(IFLOFF(0),ERR=94) (IA_STOC(N),N=1,NP_STOC)
         READ(IFLOFF(0),ERR=94) ((IC_STOC(M,N),M=1,2),N=1,NC_STOC)
C
         READ(IFLOFF(0),ERR=94) MXAREA,MXNI,MXNJ,MXNK,MXNIJ,MXNIJK
         READ(IFLOFF(0),ERR=94) (NI(N),N=1,MXAREA)
         READ(IFLOFF(0),ERR=94) (NJ(N),N=1,MXAREA)
         READ(IFLOFF(0),ERR=94) (NK(N),N=1,MXAREA)
         READ(IFLOFF(0),ERR=94) ((ISUB(M,N),M=1,3),N=1,NP_STOC)
         READ(IFLOFF(0),ERR=94) ((JSUB(M,N),M=1,3),N=1,NP_STOC)
         READ(IFLOFF(0),ERR=94) ((KSUB(M,N),M=1,3),N=1,NP_STOC)
      ENDIF
C
c ... debug write
      write(*,830)
      do n=1,mxarea
        write(*,840) n,ni(n),nj(n),nk(n)
      enddo
  830 format('### FLUID AREA INFORMATION COMMUNICATING WITH DM',/
     $      ,' AREA,  NI,  NJ,  NK')
  840 format(I5,3(',',I4))
c
      write(*,810)
      do n=1,np_stoc
         write(*,820) n,isub(1:3,n),jsub(1:3,n),ksub(1:3,n)
      enddo
  810 format(/,' N,  IS,  IE,  NI,  JS,  JE,  NJ,  KS,  KE,  NK')
  820 format(I2,9(',',I4))
c
C
      write(IFL,*) 'mxni,mxnj,mxnk=',mxni,mxnj,mxnk
      write(IFL,*) 'mxnij,mxnijk=',mxnij,mxnijk
C
      RETURN
   90 CONTINUE
      WRITE(*,*) 'ERROR: cannot open input.dat'
      CALL ERRMSG('COM_STOC0',-1)
  900 CONTINUE
      WRITE(*,*) 'ERROR: at AREA_DATA in input.dat'
      WRITE(*,*) '       CONFIGURATION OF DEVIDED AREAS MUST BE SAME'
      WRITE(*,*) '       CHECK ELEMENTS: ',NP+1,' AND ',IERR+1
      CALL ERRMSG('COM_STOC0',-2)
  920 CONTINUE
      WRITE(*,*) 'ERROR: at com_stoc0:920'
      CALL ERRMSG('COM_STOC0',-3)
   91 CONTINUE
      WRITE(*,*) 'ERROR: cannot open ./drift/offline_ctl.dat'
      CALL ERRMSG('COM_STOC0',-4)
   92 CONTINUE
      WRITE(*,*) 'ERROR: cannot open ./drift/offline_ctl.dat'
      CALL ERRMSG('COM_STOC0',-5)
   93 CONTINUE
      WRITE(*,*) 'ERROR: write error at ./drift/offline_ctl.dat'
      CALL ERRMSG('COM_STOC0',-6)
   94 CONTINUE
      WRITE(*,*) 'ERROR: read error at ./drift/offline_ctl.dat'
      CALL ERRMSG('COM_STOC0',-7)
      END
