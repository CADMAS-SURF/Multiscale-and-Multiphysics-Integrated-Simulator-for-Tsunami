      SUBROUTINE COM_STOC1
C----------------------------------------
C     領域情報・計算条件の通信
C
C     <サブルーチン構造>
C        if( online ) then
C           STOCの格子データの受信
C
C           if( off_output ) then
C              ファイル出力
C           endif
C        else
C           ファイル読み込み
C        endif
C----------------------------------------
      USE M_GRID
      USE M_FLUID
      USE M_COM_STOC,ONLY: NP_STOC,IP_STOC,IP_STOC_MAIN,IA_STOC
      USE M_OUTPUT,ONLY:IFL
      USE M_FILEIN,ONLY:ONLINE,OFFLINE,OFF_OUTPUT,IFLOFF
C
      use mod_comm,only: comm_mlicds_dm
      IMPLICIT NONE
C
      INCLUDE 'mpif.h'
C
      INTEGER::NP,NC
      INTEGER::IERR
      INTEGER::ISTAT(MPI_STATUS_SIZE)
      INTEGER::IREQ1
C
      INTEGER::I,J,K,N
      INTEGER::IWK,JWK,KWK
      REAL(8)::AWK
      REAL(8),ALLOCATABLE::XWK1(:),YWK1(:),ZWK1(:)
      REAL(8),ALLOCATABLE::XWK2(:),YWK2(:),ZWK2(:)
C
C
C----------------------------------------
C     オンライン計算用処理
C----------------------------------------
      IF( ONLINE ) THEN
C
      DO N=1,NP_STOC
         NP=IP_STOC(N)
         NC=IA_STOC(N)
         IWK=ISUB(3,N)
         JWK=JSUB(3,N)
         KWK=KSUB(3,N)
         write(IFL,*) 'com_stoc1:n=',n,' np=',np,' nc=',nc,
     $      ' ip_stoc_main=',IP_STOC_MAIN
C
         ALLOCATE(XWK1(IWK+1))
         ALLOCATE(YWK1(JWK+1))
         ALLOCATE(ZWK1(KWK+1))
         ALLOCATE(XWK2(IWK))
         ALLOCATE(YWK2(JWK))
         ALLOCATE(ZWK2(KWK))
C
         CALL MPI_IRECV(AWK,1,MPI_DOUBLE_PRECISION,NP,MPI_ANY_TAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  DX の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         DX(NC) = AWK
C
         CALL MPI_IRECV(AWK,1,MPI_DOUBLE_PRECISION,NP,MPI_ANY_TAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  DY の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         DY(NC) = AWK
C
         CALL MPI_IRECV(ZWK2,KWK,MPI_DOUBLE_PRECISION,NP,MPI_ANY_TAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  DZ(NK) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         DO K=1,KWK
            DZ(NC,K) = ZWK2(K)
         ENDDO
C
         CALL MPI_IRECV(XWK1,IWK+1,MPI_DOUBLE_PRECISION,NP,MPI_ANY_TAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  XG(0:NI) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         DO I=0,IWK
            XG(NC,ISUB(1,N)-1+I) = XWK1(I+1)
         ENDDO
C
         CALL MPI_IRECV(YWK1,JWK+1,MPI_DOUBLE_PRECISION,NP,MPI_ANY_TAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  YG(0:NJ) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         DO J=0,JWK
            YG(NC,JSUB(1,N)-1+J) = YWK1(J+1)
         ENDDO
C
         CALL MPI_IRECV(ZWK1,KWK+1,MPI_DOUBLE_PRECISION,NP,MPI_ANY_TAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  ZG(0:NK) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         DO K=0,KWK
            ZG(NC,KSUB(1,N)-1+K) = ZWK1(K+1)
         ENDDO
C
         CALL MPI_IRECV(XWK2,IWK,MPI_DOUBLE_PRECISION,NP,MPI_ANY_TAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  XC(1:NI) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         DO I=1,IWK
            XC(NC,ISUB(1,N)-1+I) = XWK2(I)
         ENDDO
C
         CALL MPI_IRECV(YWK2,JWK,MPI_DOUBLE_PRECISION,NP,MPI_ANY_TAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  YC(1:NJ) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         DO J=1,JWK
            YC(NC,JSUB(1,N)-1+J) = YWK2(J)
         ENDDO
C
         CALL MPI_IRECV(ZWK2,KWK,MPI_DOUBLE_PRECISION,NP,MPI_ANY_TAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  ZC(1:NK) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         DO K=1,KWK
            ZC(NC,KSUB(1,N)-1+K) = ZWK2(K)
         ENDDO
C
C
         DEALLOCATE(XWK1)
         DEALLOCATE(YWK1)
         DEALLOCATE(ZWK1)
         DEALLOCATE(XWK2)
         DEALLOCATE(YWK2)
         DEALLOCATE(ZWK2)
C
C
         IF ( NP.EQ.IP_STOC_MAIN ) THEN
            CALL MPI_IRECV(AWK,1,MPI_DOUBLE_PRECISION,NP,MPI_ANY_TAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  RHO の通信
            CALL MPI_WAIT(IREQ1,ISTAT,IERR)
            IF ( RHO.NE.AWK ) THEN
               WRITE (*,*) ' ERROR!! ==> Sub. com_stoc1 (STOC-DM) '
               WRITE (*,*)
     &              ' Different density (RHO) between ML(or IC) and DM '
               WRITE (*,*) RHO,AWK
               CALL ERRMSG('COM_STOC1',-1)
            ENDIF
         ENDIF
      ENDDO
C
C ... オフライン計算用ファイル出力
      IF( OFF_OUTPUT ) THEN
         DO N=1,NP_STOC
            NP=IP_STOC(N)
            NC=IA_STOC(N)
            IWK=ISUB(3,N)
            JWK=JSUB(3,N)
            KWK=KSUB(3,N)
C
            WRITE(IFLOFF(0),ERR=90) DX(NC),DY(NC),(DZ(NC,K),K=1,KWK)
            WRITE(IFLOFF(0),ERR=90) (XG(NC,ISUB(1,N)-1+I),I=0,IWK)
            WRITE(IFLOFF(0),ERR=90) (YG(NC,JSUB(1,N)-1+J),J=0,JWK)
            WRITE(IFLOFF(0),ERR=90) (ZG(NC,KSUB(1,N)-1+K),K=0,KWK)
            WRITE(IFLOFF(0),ERR=90) (XC(NC,ISUB(1,N)-1+I),I=1,IWK)
            WRITE(IFLOFF(0),ERR=90) (YC(NC,JSUB(1,N)-1+J),J=1,JWK)
            WRITE(IFLOFF(0),ERR=90) (ZC(NC,KSUB(1,N)-1+K),K=1,KWK)
         ENDDO
      ENDIF
C
C----------------------------------------
C     オフライン計算用処理
C----------------------------------------
      ELSE
C
         DO N=1,NP_STOC
            NP=IP_STOC(N)
            NC=IA_STOC(N)
            IWK=ISUB(3,N)
            JWK=JSUB(3,N)
            KWK=KSUB(3,N)
c
            write(IFL,*) 'com_stoc1:n=',n,' np=',np,' nc=',nc,
     $         ' ip_stoc_main=',IP_STOC_MAIN
C
            READ(IFLOFF(0),ERR=91) DX(NC),DY(NC),(DZ(NC,K),K=1,KWK)
            READ(IFLOFF(0),ERR=91) (XG(NC,ISUB(1,N)-1+I),I=0,IWK)
            READ(IFLOFF(0),ERR=91) (YG(NC,JSUB(1,N)-1+J),J=0,JWK)
            READ(IFLOFF(0),ERR=91) (ZG(NC,KSUB(1,N)-1+K),K=0,KWK)
            READ(IFLOFF(0),ERR=91) (XC(NC,ISUB(1,N)-1+I),I=1,IWK)
            READ(IFLOFF(0),ERR=91) (YC(NC,JSUB(1,N)-1+J),J=1,JWK)
            READ(IFLOFF(0),ERR=91) (ZC(NC,KSUB(1,N)-1+K),K=1,KWK)
         ENDDO
C
      ENDIF
C
C
      RETURN
   90 CONTINUE
      WRITE(*,*) 'ERROR: write error at ./drift/offline_ctl.dat'
      WRITE(*,*) '       NP,NC=',NP,NC
      CALL ERRMSG('COM_STOC1',-2)
   91 CONTINUE
      WRITE(*,*) 'ERROR: read error at ./drift/offline_ctl.dat'
      WRITE(*,*) '       NP,NC=',NP,NC
      CALL ERRMSG('COM_STOC1',-3)
      END
