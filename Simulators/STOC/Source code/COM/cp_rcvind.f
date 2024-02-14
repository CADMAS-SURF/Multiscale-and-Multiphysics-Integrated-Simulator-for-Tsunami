      SUBROUTINE CP_RCVIND(INDU_ML,INDV_ML,INDW_ML,INDP_ML,IBUF,
     1                     GX_ML,GY_ML,GZ_ML,GV_ML,BUF,
     2                     MX_ML,MY_ML,MZ_ML,
     3                     IEAS,IWES,JSOU,JNOR,KBOT,KTOP,
     4                     NESXM,NESXP,NESYM,NESYP)
C-----------------------------------------------------------------------
C     IPFLF=0 : MLのINDU,INDV,INDW,GX,GY,GZを受信する。
C     IPFLG=1 : MLのINDU,INDV,INDW,GX,GY,GZをファイルから入力する。
C-----------------------------------------------------------------------
C
      use mod_comm,only: comm_model
      IMPLICIT NONE
C
      INCLUDE  'CP_NESTBC.h'
      INCLUDE  'CONNEC.h'
      INCLUDE  'FILE.h'
      INCLUDE  'BOUNDI.h'
      INCLUDE  'mpif.h'
C
      INTEGER,INTENT(INOUT)::MX_ML,MY_ML,MZ_ML
      INTEGER,INTENT(INOUT)::IEAS,IWES,JSOU,JNOR,KBOT,KTOP
      INTEGER,INTENT(INOUT)::NESXM,NESXP,NESYM,NESYP
C
      INTEGER,INTENT(INOUT)::
     $   INDU_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDV_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDW_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   INDP_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1)
      INTEGER,INTENT(INOUT)::IBUF(*)
C
      REAL(8),INTENT(INOUT)::
     $   GX_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   GY_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1),
     $   GZ_ML(IWES-1:IEAS+1,JSOU-1:JNOR+1,KBOT-1:KTOP+1)
C not use gv_ml
      REAL(8),INTENT(IN)::GV_ML(1)
      REAL(8),INTENT(INOUT)::BUF(*)
C
      INTEGER::ISTAT(MPI_STATUS_SIZE)
C
      INTEGER::I,IERROR,IPARNT,IREQ,J,K,NCOUNT,NDATA
C
      IPARNT=IPECON(2,NRANK+1)
C
C ... INDU_MLを受信する。
C
      NDATA=(NESXM+2)*(JNOR-JSOU+3)*(KTOP-KBOT+3)
     &     +(NESXP+2)*(JNOR-JSOU+3)*(KTOP-KBOT+3)
     &     +MAX(0,IEAS-IWES-NESXP-NESXM-1)*(NESYM+2)*(KTOP-KBOT+3)
     &     +MAX(0,IEAS-IWES-NESXP-NESXM-1)*(NESYP+2)*(KTOP-KBOT+3)
C
      IF(IPFLG.EQ.0) THEN
      CALL MPI_IRECV(IBUF,NDATA,MPI_INTEGER,IPARNT,
     &               MPI_ANY_TAG,comm_model,IREQ,IERROR)
      CALL MPI_WAIT(IREQ,ISTAT,IERROR)
      ELSE
      READ(IFLBI) NCOUNT,(IBUF(K),K=1,NDATA)
      END IF
C
      NCOUNT=0
      DO 100 K=KBOT-1,KTOP+1
      DO 100 J=JSOU-1,JNOR+1
      DO 100 I=IWES-1,IWES+NESXM
      NCOUNT=NCOUNT+1
      INDU_ML(I,J,K)=IBUF(NCOUNT)
  100 CONTINUE
C
      DO 110 K=KBOT-1,KTOP+1
      DO 110 J=JSOU-1,JNOR+1
      DO 110 I=IEAS-NESXP,IEAS+1
      NCOUNT=NCOUNT+1
      INDU_ML(I,J,K)=IBUF(NCOUNT)
  110 CONTINUE
C
      DO 120 K=KBOT-1,KTOP+1
      DO 120 J=JSOU-1,JSOU+NESYM
      DO 120 I=IWES+NESXM+1,IEAS-NESXP-1
      NCOUNT=NCOUNT+1
      INDU_ML(I,J,K)=IBUF(NCOUNT)
  120 CONTINUE
C
      DO 130 K=KBOT-1,KTOP+1
      DO 130 J=JNOR-NESYP,JNOR+1
      DO 130 I=IWES+NESXM+1,IEAS-NESXP-1
      NCOUNT=NCOUNT+1
      INDU_ML(I,J,K)=IBUF(NCOUNT)
  130 CONTINUE
C
C ... INDV_MLを受信する。
C
      IF(IPFLG.EQ.0) THEN
      CALL MPI_IRECV(IBUF,NDATA,MPI_INTEGER,IPARNT,
     &               MPI_ANY_TAG,comm_model,IREQ,IERROR)
      CALL MPI_WAIT(IREQ,ISTAT,IERROR)
      ELSE
      READ(IFLBI) NCOUNT,(IBUF(K),K=1,NDATA)
      END IF
C
      NCOUNT=0
      DO 200 K=KBOT-1,KTOP+1
      DO 200 J=JSOU-1,JNOR+1
      DO 200 I=IWES-1,IWES+NESXM
      NCOUNT=NCOUNT+1
      INDV_ML(I,J,K)=IBUF(NCOUNT)
  200 CONTINUE
C
      DO 210 K=KBOT-1,KTOP+1
      DO 210 J=JSOU-1,JNOR+1
      DO 210 I=IEAS-NESXP,IEAS+1
      NCOUNT=NCOUNT+1
      INDV_ML(I,J,K)=IBUF(NCOUNT)
  210 CONTINUE
C
      DO 220 K=KBOT-1,KTOP+1
      DO 220 J=JSOU-1,JSOU+NESYM
      DO 220 I=IWES+NESXM+1,IEAS-NESXP-1
      NCOUNT=NCOUNT+1
      INDV_ML(I,J,K)=IBUF(NCOUNT)
  220 CONTINUE
C
      DO 230 K=KBOT-1,KTOP+1
      DO 230 J=JNOR-NESYP,JNOR+1
      DO 230 I=IWES+NESXM+1,IEAS-NESXP-1
      NCOUNT=NCOUNT+1
      INDV_ML(I,J,K)=IBUF(NCOUNT)
  230 CONTINUE
C
C ... INDW_MLを受信する。
C
      IF(IPFLG.EQ.0) THEN
      CALL MPI_IRECV(IBUF,NDATA,MPI_INTEGER,IPARNT,
     &               MPI_ANY_TAG,comm_model,IREQ,IERROR)
      CALL MPI_WAIT(IREQ,ISTAT,IERROR)
      ELSE
      READ(IFLBI) NCOUNT,(IBUF(K),K=1,NDATA)
      END IF
C
      NCOUNT=0
      DO 300 K=KBOT-1,KTOP+1
      DO 300 J=JSOU-1,JNOR+1
      DO 300 I=IWES-1,IWES+NESXM
      NCOUNT=NCOUNT+1
      INDW_ML(I,J,K)=IBUF(NCOUNT)
  300 CONTINUE
C
      DO 310 K=KBOT-1,KTOP+1
      DO 310 J=JSOU-1,JNOR+1
      DO 310 I=IEAS-NESXP,IEAS+1
      NCOUNT=NCOUNT+1
      INDW_ML(I,J,K)=IBUF(NCOUNT)
  310 CONTINUE
C
      DO 320 K=KBOT-1,KTOP+1
      DO 320 J=JSOU-1,JSOU+NESYM
      DO 320 I=IWES+NESXM+1,IEAS-NESXP-1
      NCOUNT=NCOUNT+1
      INDW_ML(I,J,K)=IBUF(NCOUNT)
  320 CONTINUE
C
      DO 330 K=KBOT-1,KTOP+1
      DO 330 J=JNOR-NESYP,JNOR+1
      DO 330 I=IWES+NESXM+1,IEAS-NESXP-1
      NCOUNT=NCOUNT+1
      INDW_ML(I,J,K)=IBUF(NCOUNT)
  330 CONTINUE
C
C ... INDP_MLを受信する。
C
      IF(IPFLG.EQ.0) THEN
      CALL MPI_IRECV(IBUF,NDATA,MPI_INTEGER,IPARNT,
     &               MPI_ANY_TAG,comm_model,IREQ,IERROR)
      CALL MPI_WAIT(IREQ,ISTAT,IERROR)
      ELSE
      READ(IFLBI) NCOUNT,(IBUF(K),K=1,NDATA)
      END IF
C
      NCOUNT=0
      DO 700 K=KBOT-1,KTOP+1
      DO 700 J=JSOU-1,JNOR+1
      DO 700 I=IWES-1,IWES+NESXM
      NCOUNT=NCOUNT+1
      INDP_ML(I,J,K)=IBUF(NCOUNT)
  700 CONTINUE
C
      DO 710 K=KBOT-1,KTOP+1
      DO 710 J=JSOU-1,JNOR+1
      DO 710 I=IEAS-NESXP,IEAS+1
      NCOUNT=NCOUNT+1
      INDP_ML(I,J,K)=IBUF(NCOUNT)
  710 CONTINUE
C
      DO 720 K=KBOT-1,KTOP+1
      DO 720 J=JSOU-1,JSOU+NESYM
      DO 720 I=IWES+NESXM+1,IEAS-NESXP-1
      NCOUNT=NCOUNT+1
      INDP_ML(I,J,K)=IBUF(NCOUNT)
  720 CONTINUE
C
      DO 730 K=KBOT-1,KTOP+1
      DO 730 J=JNOR-NESYP,JNOR+1
      DO 730 I=IWES+NESXM+1,IEAS-NESXP-1
      NCOUNT=NCOUNT+1
      INDP_ML(I,J,K)=IBUF(NCOUNT)
  730 CONTINUE
C
C ... GX_MLを受信する。
C
      IF(IPFLG.EQ.0) THEN
      CALL MPI_IRECV(BUF,NDATA,MPI_DOUBLE_PRECISION,IPARNT,
     &               MPI_ANY_TAG,comm_model,IREQ,IERROR)
      CALL MPI_WAIT(IREQ,ISTAT,IERROR)
      ELSE
      READ(IFLBI) NCOUNT,(BUF(K),K=1,NDATA)
      END IF
C
      NCOUNT=0
      DO 400 K=KBOT-1,KTOP+1
      DO 400 J=JSOU-1,JNOR+1
      DO 400 I=IWES-1,IWES+NESXM
      NCOUNT=NCOUNT+1
      GX_ML(I,J,K)=BUF(NCOUNT)
  400 CONTINUE
C
      DO 410 K=KBOT-1,KTOP+1
      DO 410 J=JSOU-1,JNOR+1
      DO 410 I=IEAS-NESXP,IEAS+1
      NCOUNT=NCOUNT+1
      GX_ML(I,J,K)=BUF(NCOUNT)
  410 CONTINUE
C
      DO 420 K=KBOT-1,KTOP+1
      DO 420 J=JSOU-1,JSOU+NESYM
      DO 420 I=IWES+NESXM+1,IEAS-NESXP-1
      NCOUNT=NCOUNT+1
      GX_ML(I,J,K)=BUF(NCOUNT)
  420 CONTINUE
C
      DO 430 K=KBOT-1,KTOP+1
      DO 430 J=JNOR-NESYP,JNOR+1
      DO 430 I=IWES+NESXM+1,IEAS-NESXP-1
      NCOUNT=NCOUNT+1
      GX_ML(I,J,K)=BUF(NCOUNT)
  430 CONTINUE
C
C ... GY_MLを受信する。
C
      IF(IPFLG.EQ.0) THEN
      CALL MPI_IRECV(BUF,NDATA,MPI_DOUBLE_PRECISION,IPARNT,
     &               MPI_ANY_TAG,comm_model,IREQ,IERROR)
      CALL MPI_WAIT(IREQ,ISTAT,IERROR)
      ELSE
      READ(IFLBI) NCOUNT,(BUF(K),K=1,NDATA)
      END IF
C
      NCOUNT=0
      DO 500 K=KBOT-1,KTOP+1
      DO 500 J=JSOU-1,JNOR+1
      DO 500 I=IWES-1,IWES+NESXM
      NCOUNT=NCOUNT+1
      GY_ML(I,J,K)=BUF(NCOUNT)
  500 CONTINUE
C
      DO 510 K=KBOT-1,KTOP+1
      DO 510 J=JSOU-1,JNOR+1
      DO 510 I=IEAS-NESXP,IEAS+1
      NCOUNT=NCOUNT+1
      GY_ML(I,J,K)=BUF(NCOUNT)
  510 CONTINUE
C
      DO 520 K=KBOT-1,KTOP+1
      DO 520 J=JSOU-1,JSOU+NESYM
      DO 520 I=IWES+NESXM+1,IEAS-NESXP-1
      NCOUNT=NCOUNT+1
      GY_ML(I,J,K)=BUF(NCOUNT)
  520 CONTINUE
C
      DO 530 K=KBOT-1,KTOP+1
      DO 530 J=JNOR-NESYP,JNOR+1
      DO 530 I=IWES+NESXM+1,IEAS-NESXP-1
      NCOUNT=NCOUNT+1
      GY_ML(I,J,K)=BUF(NCOUNT)
  530 CONTINUE
C
C ... GZ_MLを受信する。
C
      IF(IPFLG.EQ.0) THEN
      CALL MPI_IRECV(BUF,NDATA,MPI_DOUBLE_PRECISION,IPARNT,
     &               MPI_ANY_TAG,comm_model,IREQ,IERROR)
      CALL MPI_WAIT(IREQ,ISTAT,IERROR)
      ELSE
      READ(IFLBI) NCOUNT,(BUF(K),K=1,NDATA)
      END IF
C
      NCOUNT=0
      DO 600 K=KBOT-1,KTOP+1
      DO 600 J=JSOU-1,JNOR+1
      DO 600 I=IWES-1,IWES+NESXM
      NCOUNT=NCOUNT+1
      GZ_ML(I,J,K)=BUF(NCOUNT)
  600 CONTINUE
C
      DO 610 K=KBOT-1,KTOP+1
      DO 610 J=JSOU-1,JNOR+1
      DO 610 I=IEAS-NESXP,IEAS+1
      NCOUNT=NCOUNT+1
      GZ_ML(I,J,K)=BUF(NCOUNT)
  610 CONTINUE
C
      DO 620 K=KBOT-1,KTOP+1
      DO 620 J=JSOU-1,JSOU+NESYM
      DO 620 I=IWES+NESXM+1,IEAS-NESXP-1
      NCOUNT=NCOUNT+1
      GZ_ML(I,J,K)=BUF(NCOUNT)
  620 CONTINUE
C
      DO 630 K=KBOT-1,KTOP+1
      DO 630 J=JNOR-NESYP,JNOR+1
      DO 630 I=IWES+NESXM+1,IEAS-NESXP-1
      NCOUNT=NCOUNT+1
      GZ_ML(I,J,K)=BUF(NCOUNT)
  630 CONTINUE
C
C delete GV_ML process 20120924
C
      RETURN
      END
