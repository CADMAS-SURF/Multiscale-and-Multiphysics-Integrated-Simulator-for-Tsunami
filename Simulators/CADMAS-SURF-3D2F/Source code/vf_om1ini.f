      SUBROUTINE VF_OM1INI(XX,YY,ZZ,GGV,WK01,NF)

CD=== 概要 ===========================================================

CDT   VF_OM1INI:マルチエージェントファイルに格子数等を出力する

C==== 宣言 ===========================================================

      use mod_comm,only: nrank_all,comm_work_mlicdsmg2fc_mlt
     $                  ,comm_mlicdsmg2fc_mlt
C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      include 'mpif.h'
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI) : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ) : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK) : IN  : R*8 : z方向格子座標等
CD    GGV(@FOR-3D@)  : IN  : R*8 : 空隙率
CD    WK01(@FOR-3D@) : OUT : R*8 : ワーク
CD    NF(@FOR-3D@)   : IN  : I*4 : セルの状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION GGV(NUMI,NUMJ,NUMK)
      DIMENSION NF(NUMI,NUMJ,NUMK)
      REAL WK01(2:NUMI-1,2:NUMJ-1,NUMK)

CD    -- 局所変数 --
      CHARACTER*5 TEXTP
      INTEGER M,N,ITAG,ISTAT(MPI_STATUS_SIZE),IERR
      DOUBLE PRECISION,ALLOCATABLE :: xywk(:)
      INTEGER::ISIZE,IRANK,IWORK

C==== 実行 ===========================================================

C ... マルチエージェント側にマルチエージェントと連成するCADMASのPE番号を連絡する
      ma_rank=-1
      icolor=0
      if( immtyp.gt.0 ) icolor=1
      call mpi_comm_split(comm_work_mlicdsmg2fc_mlt,icolor,nrank_all,
     $                    comm_mlicdsmg2fc_mlt,ierr)
C
CD    -- 出力指定がなければ抜ける --
      IF (IMMTYP.EQ.0) RETURN
C
      call mpi_comm_size(comm_mlicdsmg2fc_mlt,isize,ierr)
      call mpi_comm_rank(comm_mlicdsmg2fc_mlt,irank,ierr)
C     MLT_AGENTのランクを受け取る
      iwork=-1
      call mpi_allreduce(iwork,ma_rank,1,mpi_integer,mpi_max,
     $                   comm_mlicdsmg2fc_mlt,ierr)
C     CADMAS-MGのランクを送る(チェック用)
      if( ma_rank.ge.0 ) then
      itag=80+irank
      call mpi_isend(irank,1,mpi_integer,ma_rank,itag,
     $               comm_mlicdsmg2fc_mlt,ireq,ierr)
      call mpi_wait(ireq,istat,ierr)
!c3qqq
c      write(102,*) 'isize=',isize
c      write(102,*) 'irank=',irank
c      write(102,*) 'ma_rank=',ma_rank
      endif
C
CD    -- 標高の計算 --
      DO 120 J=2,NUMJ-1
        DO 110 I=2,NUMI-1
          HWK=0.0D0
          DO 100 K=2,NUMK-1
            IF (NF(I,J,K).NE.-1) THEN
              HWK=HWK+(1.0D0-GGV(I,J,K))*ZZ(2,K)
            ELSE
              HWK=HWK+ZZ(2,K)
            ENDIF
 100      CONTINUE
          WK01(I,J,1)=ZZ(1,2)+HWK
 110    CONTINUE
 120  CONTINUE
C
C     MPIによる送信
      if( ma_rank.ge.0 ) then
         ig=NUMI-2
         itag=20
         call mpi_isend(ig,1,mpi_integer,ma_rank
     $                 ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
         call mpi_wait(ireq,istat,ierr)
C
         jg=NUMJ-2
         itag=30
         call mpi_isend(jg,1,mpi_integer,ma_rank
     $                 ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
         call mpi_wait(ireq,istat,ierr)
C
         m=max(NUMI-1,NUMJ-1)
         allocate(xywk(m),stat=ierr)
C
         do i=2,NUMI
            xywk(i-1)=XX(1,I)
         enddo
         itag=40
         call mpi_isend(xywk,NUMI-1,mpi_double_precision,ma_rank
     $                 ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
         call mpi_wait(ireq,istat,ierr)
C
         do j=2,NUMJ
            xywk(j-1)=YY(1,J)
         enddo
         itag=50
         call mpi_isend(xywk,NUMJ-1,mpi_double_precision,ma_rank
     $                 ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
         call mpi_wait(ireq,istat,ierr)
C
         m=(NUMI-2)*(NUMJ-2)
         itag=60
         call mpi_isend(WK01,m,mpi_real,ma_rank
     $                 ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
         call mpi_wait(ireq,istat,ierr)
C
         deallocate(xywk,stat=ierr)
C
      endif
C     ファイル出力
c      else
CD    -- マルチエージェントファイルのオープンとメッセージの出力 --
      IMMFIL=0
      IF (NPROCS.EQ.1) THEN
        OPEN(MFILMM,ERR=9010,
     $       FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.ma',
     &       STATUS='NEW',FORM='UNFORMATTED' )
      ELSE
        WRITE(TEXTP,'(I5.5)') MYRANK
        OPEN(MFILMM,ERR=9010,
     $       FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.ma'//TEXTP,
     &       STATUS='NEW',FORM='UNFORMATTED' )
      ENDIF
      IMMFIL=MFILMM
      WRITE(ILPFIL,9510)

CD    -- セル数を出力 --
      WRITE(IMMFIL,ERR=9020) NUMI-2,NUMJ-2

CD    -- 格子座標を出力 --
      WRITE(IMMFIL,ERR=9020) (XX(1,I),I=2,NUMI)
      WRITE(IMMFIL,ERR=9020) (YY(1,J),J=2,NUMJ)

CD    -- 標高の出力 --
      WRITE(IMMFIL,ERR=9020)
     &             ((WK01(I,J,1),I=2,NUMI-1),J=2,NUMJ-1)
c      endif
C     -- 実行文の終了 --
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      CALL VF_A2ERR('VF_OM1INI','CAN NOT OPEN (data.ma).')
      GOTO 9999

 9020 CONTINUE
      CALL VF_A2ERR('VF_OM1INI','WRITE ERROR (data.ma).')
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT( ' ','>> FILE-MAM : OUT : INITIAL')

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
