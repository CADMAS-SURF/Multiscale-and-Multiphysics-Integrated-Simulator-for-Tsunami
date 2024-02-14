      SUBROUTINE COM_MA1(XC,YC,ZC,HDEP)
C-----------------------------------------------------------------------
C
C     マルチエージェントモデルへの通信の初期化と、地形データの送信を行う
C
C-----------------------------------------------------------------------
      use mod_comm,only: comm_work_mlicdsmg2fc_mlt
     $                  ,comm_mlicdsmg2fc_mlt,nrank_all
      IMPLICIT NONE
      include 'mpif.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'AGENT.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'FILEI.h'
C
      REAL(8),INTENT(IN)::XC(8,MX,MY),YC(8,MY),ZC(8,MZ)
      REAL(8),INTENT(IN)::HDEP(MX,MY)
      INTEGER::ISIZE,IRANK,IWORK
      INTEGER::ISTAT(MPI_STATUS_SIZE)
C
      INTEGER:: ICOLOR,IREQ,ITAG,IERR
      INTEGER:: I,J,M,IG,JG
      REAL(8),ALLOCATABLE:: XYWK(:)
      REAL,ALLOCATABLE:: HWK(:,:)
C
C
C ... マルチエージェント側にマルチエージェントと連成するSTOCのPE番号を連絡する
      icolor=0
      if( IMMTYP.gt.0 ) icolor=1
      call mpi_comm_split(comm_work_mlicdsmg2fc_mlt,icolor,nrank_all,
     $                    comm_mlicdsmg2fc_mlt,ierr)
C
C    -- 出力指定がなければ抜ける --
      IF (IMMTYP.EQ.0) RETURN
C
      call mpi_comm_size(comm_mlicdsmg2fc_mlt,isize,ierr)
      call mpi_comm_rank(comm_mlicdsmg2fc_mlt,irank,ierr)
C     MLT_AGENTのランクを受け取る
      iwork=-1
      call mpi_allreduce(iwork,NB_SM,1,mpi_integer,mpi_max,
     $                   comm_mlicdsmg2fc_mlt,ierr)
C     STOCのランクを送る(チェック用)
      if( NB_SM.ge.0 ) then
      itag=80+irank
      call mpi_isend(irank,1,mpi_integer,NB_SM,itag,
     $               comm_mlicdsmg2fc_mlt,ireq,ierr)
      call mpi_wait(ireq,istat,ierr)
      endif
C
C     標高の設定
      allocate(hwk(2:mxm,2:mym),stat=ierr)
      do j=2,MYM
      do i=2,MXM
         hwk(i,j)=hdep(i,j)
      enddo
      enddo
C
C     MPIによる送信
      if( NB_SM.ge.0 ) then
         ig=MX-2
         itag=20
         call mpi_isend(ig,1,mpi_integer,NB_SM
     $                 ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
         call mpi_wait(ireq,istat,ierr)
C
         jg=MY-2
         itag=30
         call mpi_isend(jg,1,mpi_integer,NB_SM
     $                 ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
         call mpi_wait(ireq,istat,ierr)
C
         m=max(MX-1,MY-1)
         allocate(xywk(m),stat=ierr)
C
         j=2
         do i=1,MXM
            xywk(i)=XC(1,I,J)
         enddo
         itag=40
         call mpi_isend(xywk,MXM,mpi_double_precision,NB_SM
     $                 ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
         call mpi_wait(ireq,istat,ierr)
C
         do j=1,MYM
            xywk(j)=YC(1,J)
         enddo
         itag=50
         call mpi_isend(xywk,MYM,mpi_double_precision,NB_SM
     $                 ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
         call mpi_wait(ireq,istat,ierr)
C
         m=(MX-2)*(MY-2)
         itag=60
         call mpi_isend(hwk,m,mpi_real,NB_SM
     $                 ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
         call mpi_wait(ireq,istat,ierr)
C
         deallocate(xywk,stat=ierr)
C
      endif
C     ファイル出力
c      else
CD    -- マルチエージェントファイルのオープンとメッセージの出力 --
      CFLNM(IFLNM-3:IFLNM) = '.ma '
      write(lp,*) CFLNM
      OPEN(IFLMA,FILE=CFLNM(1:IFLNM),STATUS='NEW',
     $       FORM='UNFORMATTED',ERR=9010)
C
      WRITE(LP,9510)

CD    -- セル数を出力 --
      WRITE(IFLMA,ERR=9020) MX-2,MY-2

CD    -- 格子座標を出力 --
      J=2
      WRITE(IFLMA,ERR=9020) (XC(1,I,J),I=1,MXM)
      WRITE(IFLMA,ERR=9020) (YC(1,J),J=1,MYM)

CD    -- 標高の出力 --
      WRITE(IFLMA,ERR=9020)
     &             ((HWK(I,J),I=2,MXM),J=2,MYM)
c      endif
C
      deallocate(hwk,stat=ierr)
C
C     -- 実行文の終了 --
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      WRITE(LP,*) 'COM_MA1: CANNOT OPEN (',CFLNM(1:IFLNM),'.'
      GOTO 9999

 9020 CONTINUE
      WRITE(LP,*) 'COM_MA1: WRITE ERROR (',CFLNM(1:IFLNM),'.'
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT( ' ','>> FILE-MAM : OUT : INITIAL')

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
