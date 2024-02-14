      SUBROUTINE COM_MA2(HH,HDEP,HU,HV,iflag)
C-----------------------------------------------------------------------
C
C     解析結果をマルチエージェントファイルに出力する
C
C-----------------------------------------------------------------------
      use mod_comm,only: comm_mlicdsmg2fc_mlt
      IMPLICIT NONE
      include 'mpif.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'AGENT.h'
      INCLUDE 'FILE.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
C
      REAL(8),INTENT(IN)::HH(MX,MY),HDEP(MX,MY)
      REAL(8),INTENT(IN)::HU(MX,MY,MZ),HV(MX,MY,MZ)
      INTEGER,INTENT(IN)::iflag
C
      INTEGER:: I,J,IO
      REAL(8):: W
      REAL(8):: DWK,UWK,VWK
      REAL:: TIMWK
      REAL,ALLOCATABLE:: WK1(:,:),WK2(:,:),WK3(:,:)
      INTEGER:: M,N,ITAG,IREQ,ISTAT(MPI_STATUS_SIZE),IERR
C
C
      if(iflag.eq.0)then
C    -- 出力の判定 --
      IO=0
C     * ステップ間隔出力の場合
      IF     (IMMTYP.EQ.1) THEN
        IF (ISTEP.GE.IMAMS .AND. ISTEP.LE.IMAME) THEN
          IF (MOD(ISTEP-IMAMS,IMAMI).EQ.0) IO=1
        ENDIF
C     * 時間間隔出力の場合                                                             
      ELSEIF (IMMTYP.EQ.2) THEN
        IF (TIME.GE.RMAMS-0.5D0*DT .AND.
     $      TIME.LE.RMAME+0.5D0*DT) THEN
          W=(TIME+0.5D0*DT)-RMAMR
          IF (W.GE.0.0D0) THEN
            IO=1
            RMAMR=RMAMR+DBLE(INT(W/RMAMI)+1)*RMAMI
          ENDIF
        ENDIF
      ENDIF

C     -- 非出力ならば抜ける --
      IF (IO.EQ.0) GOTO 9000
C
C     -- メッセージの出力 --
      WRITE(LP,9510) ISTEP,TIME
C
      allocate(wk1(2:mxm,2:mym),wk2(2:mxm,2:mym),
     $         wk3(2:mxm,2:mym),stat=ierr)
C
C     -- 水深等の計算 --
      DO 120 J=2,MYM
      DO 110 I=2,MXM
         DWK=HH(I,J)-HDEP(I,J)
         UWK=0.5D0*(HU(I-1,J,MZ)+HU(I,J,MZ))
         VWK=0.5D0*(HV(I,J-1,MZ)+HV(I,J,MZ))
C
         WK1(I,J)=REAL(DWK)
         WK2(I,J)=REAL(UWK)
         WK3(I,J)=REAL(VWK)
 110  CONTINUE
 120  CONTINUE

C     MPIによる送信
      if( NB_SM.ge.0 ) then
         TIMWK=REAL(TIME)
         itag=10
         call mpi_isend(TIMWK,1,mpi_real,NB_SM
     $                 ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
         call mpi_wait(ireq,istat,ierr)

         n=(MX-2)*(MY-2)
         itag=20
         call mpi_isend(WK1,n,mpi_real,NB_SM
     $                 ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
         call mpi_wait(ireq,istat,ierr)

         itag=30
         call mpi_isend(WK2,n,mpi_real,NB_SM
     $                 ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
         call mpi_wait(ireq,istat,ierr)

         itag=40
         call mpi_isend(WK3,n,mpi_real,NB_SM
     $                 ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
         call mpi_wait(ireq,istat,ierr)

      endif
C     ファイル出力
c      else
CD    -- 時刻を出力 --
      WRITE(IFLMA,ERR=9010) REAL(TIME)

CD    -- 水深等の出力 --
      WRITE(IFLMA,ERR=9010)
     &             ((WK1(I,J),I=2,MXM),J=2,MYM)
      WRITE(IFLMA,ERR=9010)
     &             ((WK2(I,J),I=2,MXM),J=2,MYM)
      WRITE(IFLMA,ERR=9010)
     &             ((WK3(I,J),I=2,MXM),J=2,MYM)
c      endif
C
      DEALLOCATE(WK1,WK2,WK3)
C
C     -- 実行文の終了 --
 9000 CONTINUE

C     -- MA側にSTOCの送信終了を伝える（MPIの場合のみ） --
      elseif(iflag.eq.-1)then
        if( immtyp.gt.0 .and. NB_SM.ge.0 ) then
          TIMWK=-1.0
          itag=50
          call mpi_isend(TIMWK,1,mpi_real,NB_SM
     $                  ,itag,comm_mlicdsmg2fc_mlt,ireq,ierr)
          call mpi_wait(ireq,istat,ierr)
        endif
      endif
C
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      CALL ERRMS2('COM_MA2',6900)
      WRITE(LP,*) 'WRITE ERROR (data.ma).'
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT( ' ','>> FILE-MAM : OUT : STEP=',I6,' : TIME= ',1PE12.5)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
