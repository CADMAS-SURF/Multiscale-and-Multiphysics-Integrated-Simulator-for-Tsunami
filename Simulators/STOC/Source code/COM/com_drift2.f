C=======================================================================
      SUBROUTINE COM_DRIFT2 (HT,HH,UU,VV,IDST,KBLC)
C=======================================================================
C
C     物理量(HT,HH,UU,VV,IDST,KBLC)の通信 (IDST,KBLCはSTOC-DSのみ)
C
C-----------------------------------------------------------------------
C
      use mod_comm,only: comm_mlicds_dm
      IMPLICIT NONE
C
      INCLUDE 'CONNEC.h'
      INCLUDE 'mpif.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'DRIFT.h'
      INCLUDE 'FILE.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'MODELI.h'
C
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY)
      REAL(8),INTENT(INOUT)::HT(MX,MY)
C
      INTEGER,INTENT(INOUT)::IDST(MX,MY)
      INTEGER,INTENT(INOUT)::KBLC(MX,MY)
C
C
      INTEGER::ITAG
      INTEGER::IERR
      INTEGER::ISTAT(MPI_STATUS_SIZE)
      INTEGER::IREQ1
C
      INTEGER::I,J,K
      INTEGER::IWK,JWK,KWK
      INTEGER::NNN,NDMX
      REAL(8),ALLOCATABLE::AWK(:)
C
C<<<<< (START) STOC-DS&DM VERSION  <<<<<<<
      INTEGER::IDSTFLG
      INTEGER,ALLOCATABLE::IDSTWK(:)
      INTEGER,ALLOCATABLE::KBLCWK(:)
C<<<<<  (END)  STOC-DS&DM VERSION  <<<<<<<
C
      CHARACTER(80)::FILENAME
      CHARACTER(2)::STR
      INTEGER::NNFL
      INTEGER,SAVE::IFIRST
C
C
C======================================================================
C
      NNFL=NRANK+101
C
C ... DMのオフライン計算用ファイルのOPEN
      IF( OFF_INTERVAL.GT.0.0D0.AND.IFIRST.EQ.0 ) THEN
         OFF_NEXT=MAX(OFF_START,TIME)
         IFIRST=1
         WRITE(STR,'(I2.2)') NNFL-100
         FILENAME = './drift/offline_'//STR//'.dat'
c         FILENAME = '/ldsk/PARI/run.online2/offline_'//STR//'.dat'
         OPEN(NNFL,FILE=FILENAME,STATUS='NEW',FORM='UNFORMATTED',
     $        ERR=91)
         write(6,*) 'file open:',FILENAME
      ENDIF
      IF( OFF_INTERVAL.LE.0.0D0 ) OFF_NEXT=TIME+1.0D10
C
         IWK = MX-2
         JWK = MY-2
         KWK = MZ-2
C
         NDMX = IWK * JWK
         ALLOCATE(AWK(NDMX))
C
         NNN=0
         DO J=2,JWK+1
         DO I=2,IWK+1
            NNN = NNN+1
            AWK(NNN) = HT(I,J)
         ENDDO
         ENDDO
C
         IF( .NOT.NOCALDM ) THEN
         ITAG=220
         CALL MPI_ISEND(AWK,NDMX,MPI_DOUBLE_PRECISION,NB_SD,ITAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  HT(NI,NJ) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         ENDIF
         IF( TIME.GE.OFF_NEXT ) WRITE(NNFL,ERR=92) TIME
         IF( TIME.GE.OFF_NEXT ) WRITE(NNFL,ERR=92) REAL(AWK)
         DEALLOCATE(AWK)
C
C
         NDMX = IWK * JWK
         ALLOCATE(AWK(NDMX))
C
         NNN=0
         DO J=2,JWK+1
         DO I=2,IWK+1
            NNN = NNN+1
            AWK(NNN) = HH(I,J)
         ENDDO
         ENDDO
C
         IF( .NOT.NOCALDM ) THEN
         ITAG=230
         CALL MPI_ISEND(AWK,NDMX,MPI_DOUBLE_PRECISION,NB_SD,ITAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  HH(NI,NJ) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         ENDIF
         IF( TIME.GE.OFF_NEXT ) WRITE(NNFL,ERR=92) REAL(AWK)
         DEALLOCATE(AWK)
C
C
         NDMX = IWK * JWK * KWK
         ALLOCATE(AWK(NDMX))
C
         NNN=0
         DO K=2,KWK+1
         DO J=2,JWK+1
         DO I=2,IWK+1
            NNN = NNN+1
            AWK(NNN) = UU(I,J,K)
         ENDDO
         ENDDO
         ENDDO
C
         IF( .NOT.NOCALDM ) THEN
         ITAG=240
         CALL MPI_ISEND(AWK,NDMX,MPI_DOUBLE_PRECISION,NB_SD,ITAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  UU(NI,NJ,NK) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         ENDIF
         IF( TIME.GE.OFF_NEXT ) WRITE(NNFL,ERR=92) REAL(AWK)
         DEALLOCATE(AWK)
C
C
         NDMX = IWK * JWK * KWK
         ALLOCATE(AWK(NDMX))
C
         NNN=0
         DO K=2,KWK+1
         DO J=2,JWK+1
         DO I=2,IWK+1
            NNN = NNN+1
            AWK(NNN) = VV(I,J,K)
         ENDDO
         ENDDO
         ENDDO
C
         IF( .NOT.NOCALDM ) THEN
         ITAG=250
         CALL MPI_ISEND(AWK,NDMX,MPI_DOUBLE_PRECISION,NB_SD,ITAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  VV(NI,NJ,NK) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         ENDIF
         IF( TIME.GE.OFF_NEXT ) WRITE(NNFL,ERR=92) REAL(AWK)
         DEALLOCATE(AWK)
C
C
C
C<<<<< (START) STOC-DS&DM VERSION  <<<<<<<
         IF( LSTOCDS.EQ.1 ) THEN
         IDSTFLG = 1   !  = 0 破壊なし,     = 1 破壊あり
         ELSE
         IDSTFLG = 0
         ENDIF
         IF( .NOT.NOCALDM ) THEN
         ITAG=260
         CALL MPI_ISEND(IDSTFLG,1,MPI_INTEGER,NB_SD,ITAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  IDSTFLG の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         ENDIF
         IF( TIME.GE.OFF_NEXT ) WRITE(NNFL,ERR=92) IDSTFLG
C<<<<<  (END)  STOC-DS&DM VERSION  <<<<<<<
         IF( LSTOCDS.EQ.1 ) THEN
C
C=======  破壊フラグの送信  =============
         NDMX = IWK * JWK
         ALLOCATE(IDSTWK(NDMX))
C
         NNN=0
         DO J=2,JWK+1
         DO I=2,IWK+1
            NNN = NNN+1
            IDSTWK(NNN) = IDST(I,J)
         ENDDO
         ENDDO
C
         IF( .NOT.NOCALDM ) THEN
         ITAG=270
         CALL MPI_ISEND(IDSTWK,NDMX,MPI_INTEGER,NB_SD,ITAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  IDST(NI,NJ) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         ENDIF
         IF( TIME.GE.OFF_NEXT ) WRITE(NNFL,ERR=92) IDSTWK
C
C=======  破壊フラグの受信  =============
         IF( .NOT.NOCALDM ) THEN
         CALL MPI_IRECV(IDSTWK,NDMX,MPI_INTEGER,NB_SD,MPI_ANY_TAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  IDST(NI,NJ) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
C
         NNN=0
         DO J=2,JWK+1
         DO I=2,IWK+1
            NNN = NNN+1
            IDST(I,J) = IDSTWK(NNN)
         ENDDO
         ENDDO
         ENDIF
C
         DEALLOCATE(IDSTWK)
C
C<<<<< (START) STOC-BLC VERSION  <<<<<<<
C=======  閉塞フラグの受信  =============
         NDMX = IWK * JWK
         ALLOCATE(KBLCWK(NDMX))
         IF( .NOT.NOCALDM ) THEN
         CALL MPI_IRECV(KBLCWK,NDMX,MPI_INTEGER,NB_SD,MPI_ANY_TAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  IBLC(NI,NJ) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
C
         NNN=0
         DO J=2,JWK+1
         DO I=2,IWK+1
            NNN = NNN+1
            KBLC(I,J) = KBLCWK(NNN)
         ENDDO
         ENDDO
         ENDIF
C
         DEALLOCATE(KBLCWK)
C<<<<<  (END)  STOC-BLC VERSION  <<<<<<<
C
         ENDIF
C
C-----------------------------------------------------------------------
      IF( TIME.GE.OFF_NEXT ) OFF_NEXT=OFF_NEXT+OFF_INTERVAL
      RETURN
C
   91 CONTINUE
      CALL ERRMSG('COM_DRIFT2',6890)
      WRITE(LP,*) 'FILE OPEN ERROR: ',FILENAME
      WRITE(LP,*) 'FILE NUMBER=',NNFL
      CALL ABORT1('')
C
   92 CONTINUE
      CALL ERRMSG('COM_DRIFT2',6891)
      WRITE(LP,*) 'FILE WRITE ERROR: FILE NUMBER=',NNFL
      CALL ABORT1('')
      END
