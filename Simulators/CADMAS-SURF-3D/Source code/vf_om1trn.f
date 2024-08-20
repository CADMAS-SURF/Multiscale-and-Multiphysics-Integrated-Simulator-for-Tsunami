      SUBROUTINE VF_OM1TRN(DT,ZZ,UU,VV,FF,GGV,WK01,NF,iflag)

CD=== 概要 ===========================================================

CDT   VF_OM1TRN:解析結果をマルチエージェントファイルに出力する

C==== 宣言 ===========================================================

      use mod_comm,only: comm_mlicdsmg2fc_mlt
C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      include 'mpif.h'
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPI.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_AFILER.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APHYSI.h'
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_ATIMER.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    DT             : IN  : R*8 : 次のステップの時間刻み幅
CD    ZZ(MAXG1,NUMK) : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)   : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)   : IN  : R*8 : y方向流速
CD    FF(@FOR-3D@)   : IN  : R*8 : VOF関数F
CD    GGV(@FOR-3D@)  : IN  : R*8 : 空隙率
CD    WK01(@FOR-3D@) : OUT : R*4 : ワーク
CD    NF(@FOR-3D@)   : IN  : I*4 : セルの状態を示すインデックス
      DIMENSION ZZ(MAXG1,NUMK)
      DIMENSION UU(NUMI,NUMJ,NUMK),VV (NUMI,NUMJ,NUMK)
      DIMENSION FF(NUMI,NUMJ,NUMK),GGV(NUMI,NUMJ,NUMK)
      DIMENSION NF(NUMI,NUMJ,NUMK)
      REAL WK01(2:NUMI-1,2:NUMJ-1,NUMK)
      REAL TIMWK
      INTEGER M,N,ITAG,ISTAT(MPI_STATUS_SIZE),IERR

C==== 実行 ===========================================================

      if(iflag.eq.0)then
CD    -- 出力の判定 --
      IO=0
C     * ステップ間隔出力の場合
      IF     (IMMTYP.EQ.1) THEN
        IF (NNOW.GE.IMMTRN(1) .AND. NNOW.LE.IMMTRN(2)) THEN
          IF (MOD(NNOW-IMMTRN(1),IMMTRN(3)).EQ.0) IO=1
        ENDIF
C     * 時間間隔出力の場合
      ELSEIF (IMMTYP.EQ.2) THEN
Cmod20130605        IF (TNOW.GE.RMMTRN(1)-ZERO .AND. TNOW.LE.RMMTRN(2)+ZERO) THEN
        IF (TNOW.GE.RMMTRN(1)-0.5D0*DT .AND.
     $      TNOW.LE.RMMTRN(2)+0.5D0*DT) THEN
          W=(TNOW+0.5D0*DT)-RMMTRN(4)
          IF (W.GE.0.0D0) THEN
            IO=1
            RMMTRN(4)=RMMTRN(4)+DBLE(INT(W/RMMTRN(3))+1)*RMMTRN(3)
          ENDIF
        ENDIF
      ENDIF

CD    -- 非出力ならば抜ける --
      IF (IO.EQ.0) GOTO 9000

CD    -- メッセージの出力 --
      WRITE(ILPFIL,9510) NNOW,TNOW

CD    -- 水深等の計算 --
      DO 120 J=2,NUMJ-1
        DO 110 I=2,NUMI-1
          DWK=0.0D0
          UWK=0.0D0
          VWK=0.0D0
          DO 100 K=2,NUMK-1
            IF (NF(I,J,K).NE.-1) THEN
              ZF=GGV(I,J,K)*FF(I,J,K)*ZZ(2,K)
              DWK=DWK+ZF
              UWK=UWK+(UU(I,J,K)+UU(I+1,J,K))*ZF
              VWK=VWK+(VV(I,J,K)+VV(I,J+1,K))*ZF
            ENDIF
 100      CONTINUE
          WK01(I,J,1)=REAL(DWK)
          WK01(I,J,2)=REAL(UWK*0.5D0)
          WK01(I,J,3)=REAL(VWK*0.5D0)
 110    CONTINUE
 120  CONTINUE

C     MPIによる送信
      if( ma_rank.ge.0 ) then
         TIMWK=REAL(TNOW)
         itag=10
         call mpi_isend(TIMWK,1,mpi_real,ma_rank
     $                 ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
         call mpi_wait(ireq,istat,ierr)

         n=(NUMI-2)*(NUMJ-2)
         itag=20
         call mpi_isend(WK01(2,2,1),n,mpi_real,ma_rank
     $                 ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
         call mpi_wait(ireq,istat,ierr)

         itag=30
         call mpi_isend(WK01(2,2,2),n,mpi_real,ma_rank
     $                 ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
         call mpi_wait(ireq,istat,ierr)

         itag=40
         call mpi_isend(WK01(2,2,3),n,mpi_real,ma_rank
     $                 ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
         call mpi_wait(ireq,istat,ierr)

      endif
C     ファイル出力
c      else
CD    -- 時刻を出力 --
      WRITE(IMMFIL,ERR=9010) REAL(TNOW)

CD    -- 水深等の出力 --
      WRITE(IMMFIL,ERR=9010)
     &             ((WK01(I,J,1),I=2,NUMI-1),J=2,NUMJ-1)
      WRITE(IMMFIL,ERR=9010)
     &             ((WK01(I,J,2),I=2,NUMI-1),J=2,NUMJ-1)
      WRITE(IMMFIL,ERR=9010)
     &             ((WK01(I,J,3),I=2,NUMI-1),J=2,NUMJ-1)
c      endif

C     -- 実行文の終了 --
 9000 CONTINUE

C     -- MA側にCADMASの送信終了を伝える（MPIの場合のみ） --
      elseif(iflag.eq.-1)then
        if( immtyp.gt.0 .and. ma_rank.ge.0 ) then
          TIMWK=-1.0
          itag=50
          call mpi_isend(TIMWK,1,mpi_real,ma_rank
     $                  ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
          call mpi_wait(ireq,istat,ierr)
        endif
      endif

      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      CALL VF_A2ERR('VF_OM1TRN','WRITE ERROR (data.mam).')
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT( ' ','>> FILE-MAM : OUT : STEP=',I6,' : TIME= ',1PE12.5)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
