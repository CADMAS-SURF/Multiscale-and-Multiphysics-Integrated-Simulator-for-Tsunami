C=======================================================================
      SUBROUTINE COM_STOC2(TIMFL,IEND1)
C=======================================================================
C     物理量(HT,HH,UU,VV,IDST,IBLC)の通信
C=======================================================================
      USE M_GRID
      USE M_GEOM
      USE M_FILEIN
      USE M_COM_STOC,ONLY: NP_STOC,IP_STOC,IA_STOC,IB_SD,NSIZE
      USE M_OUTPUT,ONLY:IFL
C
      use mod_comm,only: comm_mlicds_dm
      IMPLICIT NONE
C
      INCLUDE 'mpif.h'
C
      REAL(8),INTENT(OUT)::TIMFL
      INTEGER,INTENT(OUT)::IEND1
C
      INTEGER::NP,NC
      INTEGER::IERR
      INTEGER::ISTAT(MPI_STATUS_SIZE)
      INTEGER::IREQ1
C
      INTEGER::I,J,K,N,M
      INTEGER::IWK,JWK,KWK
      INTEGER::NNN,NN2,NDMX,NNFL
      REAL(8),ALLOCATABLE::AWK(:)
      REAL(4),ALLOCATABLE::AWK2(:)
      CHARACTER(80)::FILENAME
      CHARACTER(2)::STR
C<<<<< (START) STOC-DS&DM VERSION  <<<<<<<
C<<<<< (START) STOC-BLC VERSION  <<<<<<<
      INTEGER::IDSTFLG
      INTEGER,ALLOCATABLE::IDSTWK(:)
      INTEGER,ALLOCATABLE::KBLCWK(:)
C<<<<<  (END)  STOC-DS&DM VERSION  <<<<<<<
C<<<<<  (END)  STOC-BLC VERSION  <<<<<<<
C
C
C=======================================================================
C
C
C
C ... DMのオフライン計算用ファイルのOPEN
      IF( OFFLINE ) THEN
         DO NNFL=1,NSIZE
            IF( IB_SD(NNFL).GT.0 ) THEN
               IFLOFF(NNFL)=100+NNFL
               WRITE(STR,'(I2.2)') NNFL
               FILENAME='./drift/offline_'//STR//'.dat'
               OPEN(IFLOFF(NNFL),FILE=trim(FILENAME),
     $              STATUS='OLD',FORM='UNFORMATTED',ERR=91)
            ENDIF
         ENDDO
      ENDIF
C
C
      DO N=1,NP_STOC
         NP=IP_STOC(N)
         NC=IA_STOC(N)
         IWK=ISUB(3,N)
         JWK=JSUB(3,N)
         KWK=KSUB(3,N)
C
         M=0
         DO NNFL=1,NSIZE
            IF( IB_SD(NNFL).GT.0 ) THEN
               M=M+1
               IF( M.EQ.N ) EXIT
            ENDIF
         ENDDO
C
         NDMX = IWK * JWK
         ALLOCATE(AWK(NDMX))
         IF( ONLINE ) THEN
         CALL MPI_IRECV(AWK,NDMX,MPI_DOUBLE_PRECISION,NP,MPI_ANY_TAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  HT(NI,NJ) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
C
         ELSE
            READ(IFLOFF(NNFL),ERR=90,END=100) TIMFL
            GOTO 110
C
  100       CONTINUE
            IEND1=1
            WRITE (IFL,*) ' #########   STOC-DM   ######### '
            WRITE (IFL,*) ' Reached end of fluid data file !'
            WRITE (IFL,*) ' #########   STOC-DM   ######### '
            RETURN
C
  110       CONTINUE
            ALLOCATE(AWK2(NDMX))
            READ(IFLOFF(NNFL),ERR=90) AWK2
            AWK(:)=AWK2(:)
            DEALLOCATE(AWK2)
         ENDIF
C
         NNN=0
         DO J=JSUB(1,N),JSUB(2,N)
         DO I=ISUB(1,N),ISUB(2,N)
            NNN = NNN+1
            HT(NC,I,J) = AWK(NNN)
         ENDDO
         ENDDO
C
         DEALLOCATE(AWK)
C
C
         NDMX = IWK * JWK
         ALLOCATE(AWK(NDMX))
         IF( ONLINE ) THEN
         CALL MPI_IRECV(AWK,NDMX,MPI_DOUBLE_PRECISION,NP,MPI_ANY_TAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  HH(NI,NJ) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         ELSE
            ALLOCATE(AWK2(NDMX))
            READ(IFLOFF(NNFL),ERR=90) AWK2
            AWK(:)=AWK2(:)
            DEALLOCATE(AWK2)
         ENDIF
C
         NNN=0
         DO J=JSUB(1,N),JSUB(2,N)
         DO I=ISUB(1,N),ISUB(2,N)
            NNN = NNN+1
            NN2 = NI(NC)*(J-1) + I
            HHAR1(NC,NN2) = AWK(NNN)
         ENDDO
         ENDDO
C
         DEALLOCATE(AWK)
C
C
         NDMX = IWK * JWK * KWK
         ALLOCATE(AWK(NDMX))
         IF( ONLINE ) THEN
         CALL MPI_IRECV(AWK,NDMX,MPI_DOUBLE_PRECISION,NP,MPI_ANY_TAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  UU(NI,NJ,NJ) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         ELSE
            ALLOCATE(AWK2(NDMX))
            READ(IFLOFF(NNFL),ERR=90) AWK2
            AWK(:)=AWK2(:)
            DEALLOCATE(AWK2)
         ENDIF
C
         NNN=0
         DO K=KSUB(1,N),KSUB(2,N)
         DO J=JSUB(1,N),JSUB(2,N)
         DO I=ISUB(1,N),ISUB(2,N)
            NNN = NNN+1
            NN2 = NI(NC)*NJ(NC)*(K-1) + NI(NC)*(J-1) + I
            UUAR1(NC,NN2) = AWK(NNN)
         ENDDO
         ENDDO
         ENDDO
C
         DEALLOCATE(AWK)
C
C
         NDMX = IWK * JWK * KWK
         ALLOCATE(AWK(NDMX))
         IF( ONLINE ) THEN
         CALL MPI_IRECV(AWK,NDMX,MPI_DOUBLE_PRECISION,NP,MPI_ANY_TAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  VV(NI,NJ,NJ) の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
C
         ELSE
            ALLOCATE(AWK2(NDMX))
            READ(IFLOFF(NNFL),ERR=90) AWK2
            AWK(:)=AWK2(:)
            DEALLOCATE(AWK2)
         ENDIF
C
         NNN=0
         DO K=KSUB(1,N),KSUB(2,N)
         DO J=JSUB(1,N),JSUB(2,N)
         DO I=ISUB(1,N),ISUB(2,N)
            NNN = NNN+1
            NN2 = NI(NC)*NJ(NC)*(K-1) + NI(NC)*(J-1) + I
            VVAR1(NC,NN2) = AWK(NNN)
         ENDDO
         ENDDO
         ENDDO
C
         DEALLOCATE(AWK)
C
C
C<<<<< (START) STOC-DS&DM VERSION  <<<<<<<
         IF( ONLINE ) THEN
         CALL MPI_IRECV(IDSTFLG,1,MPI_INTEGER,NP,MPI_ANY_TAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  IDSTFLG の通信
         CALL MPI_WAIT(IREQ1,ISTAT,IERR)
         ELSE
            READ(IFLOFF(NNFL),ERR=90) IDSTFLG
         ENDIF
C
         IF( IDSTFLG.EQ.1 )THEN
C=======  破壊フラグの受信  =============
            NDMX = IWK * JWK
            ALLOCATE(IDSTWK(NDMX))
            IF( ONLINE ) THEN
            CALL MPI_IRECV(IDSTWK,NDMX,MPI_INTEGER,NP,MPI_ANY_TAG,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  IDST(NI,NJ) の通信
            CALL MPI_WAIT(IREQ1,ISTAT,IERR)
            ELSE
               READ(IFLOFF(NNFL),ERR=90) IDSTWK
            ENDIF
C
            NNN=0
            DO J=JSUB(1,N),JSUB(2,N)
            DO I=ISUB(1,N),ISUB(2,N)
               NNN = NNN+1
               IF( IDSTWK(NNN).GE.1 .AND. IDST(NC,I,J).LT.0 )THEN
                  IDST(NC,I,J) = IDST(NC,I,J)   !!!  漂流物衝突による建物破壊を反映
               ELSE
                  IDST(NC,I,J) = IDSTWK(NNN)
               ENDIF
            ENDDO
            ENDDO
            DEALLOCATE(IDSTWK)
C
            IF( ONLINE ) THEN
C=======  破壊フラグの送信  =============
            ALLOCATE(IDSTWK(NDMX))
            NNN=0
            DO J=JSUB(1,N),JSUB(2,N)
            DO I=ISUB(1,N),ISUB(2,N)
               NNN = NNN+1
               IDSTWK(NNN)=IDST(NC,I,J)
            ENDDO
            ENDDO
C
            CALL MPI_ISEND(IDSTWK,NDMX,MPI_INTEGER,NP,NP,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  IDST(NI,NJ) の通信
            CALL MPI_WAIT(IREQ1,ISTAT,IERR)
            DEALLOCATE(IDSTWK)
C
C
C<<<<< (START) STOC-BLC VERSION  <<<<<<<
C=======  閉塞フラグの送信  =============
            NDMX = IWK * JWK
            ALLOCATE(KBLCWK(NDMX))
C
            NNN=0
            DO J=JSUB(1,N),JSUB(2,N)
            DO I=ISUB(1,N),ISUB(2,N)
               NNN = NNN+1
               KBLCWK(NNN)=KBLC(NC,I,J)
            ENDDO
            ENDDO
C
            CALL MPI_ISEND(KBLCWK,NDMX,MPI_INTEGER,NP,NP,
     &                                        comm_mlicds_dm,IREQ1,IERR)    !  IBLC(NI,NJ) の通信
            CALL MPI_WAIT(IREQ1,ISTAT,IERR)
            DEALLOCATE(KBLCWK)
C
C
C<<<<<  (END)  STOC-BLC VERSION  <<<<<<<
            ENDIF
C
C
         ENDIF
C<<<<<  (END)  STOC-DS&DM VERSION  <<<<<<<
C
      ENDDO
C
C=======================================================================
      RETURN
   90 CONTINUE
      WRITE(STR,'(I2.2)') NNFL
      FILENAME='./drift/offline_'//STR//'.dat'
      WRITE(*,*) 'ERROR: read error at '//trim(FILENAME)
      CALL ERRMSG('COM_STOC2',-1)
C
   91 CONTINUE
      WRITE(*,*) 'ERROR: open error at '//trim(FILENAME)
      CALL ERRMSG('COM_STOC2',-2)
      END
