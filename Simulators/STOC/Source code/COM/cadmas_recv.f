      SUBROUTINE CADMAS_RECV
      use mod_comm,only: comm_ic_mg
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'mpif.h'
      INCLUDE 'CADMAS.h'

      INCLUDE 'TIMEI.h'
C
C ... WORK VARIABLES
      INTEGER IERR,M,N,IRANK,ISIZE,ISTAT(MPI_STATUS_SIZE),IREQ,ITAG
C
      INTEGER:: I,J,K,NSFT,NJST1,NIST1,J1,I1
C
C
C ... (1) 西側境界値の受信
C
      IF( IWCAD.GT.0 ) THEN
      DO N=1,NB_CADMAS
         NJST1 = JJCAD(1,N)
         IF( NJST1.EQ.0 ) CYCLE
C
         M     = LB_STOC
         IRANK = IB_CADMAS(N)
         ISIZE = NJST1*NKST*6
         ITAG  = ITAGSC*21+NB_CADMAS*(M-1)+N-1
         CALL MPI_IRECV(CADBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
         M = 0
         NSFT = NJST1*NKST
         DO K=1,NKST
         DO J=1,NJST1
            M = M+1
            IF( J.EQ.1    .AND.JJCAD(5,N).EQ.1 ) CYCLE
            IF( J.EQ.NJST1.AND.JJCAD(6,N).EQ.1 ) CYCLE
            J1 = J-1+JJCAD(3,N)-JSCAD+(JJOFF(1)-1)
            UWCAD(J1,K,1) = CADBUF(M       )
            UWCAD(J1,K,2) = CADBUF(M+NSFT  )
            UWCAD(J1,K,3) = CADBUF(M+NSFT*2)
            UWCAD(J1,K,4) = CADBUF(M+NSFT*3)
            IF(K.EQ.1)
     $      UWCAD(J1,K,5) = CADBUF(M+NSFT*4)
            IF(K.EQ.1)
     $      UWCAD(J1,K,6) = CADBUF(M+NSFT*5)
         ENDDO
         ENDDO
      ENDDO
      ENDIF
C
C ... (2) 東側境界値の受信
C
      IF( IECAD.GT.0 ) THEN
      DO N=1,NB_CADMAS
         NJST1 = JJCAD(2,N)
         IF( NJST1.EQ.0 ) CYCLE
C
         M     = LB_STOC
         IRANK = IB_CADMAS(N)
         ISIZE = NJST1*NKST*6
         ITAG  = ITAGSC*22+NB_CADMAS*(M-1)+N-1
         CALL MPI_IRECV(CADBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
         M = 0
         NSFT = NJST1*NKST
         DO K=1,NKST
         DO J=1,NJST1
            M = M+1
            IF( J.EQ.1    .AND.JJCAD(5,N).EQ.1 ) CYCLE
            IF( J.EQ.NJST1.AND.JJCAD(6,N).EQ.1 ) CYCLE
            J1 = J-1+JJCAD(3,N)-JSCAD+(JJOFF(1)-1)
            UECAD(J1,K,1) = CADBUF(M       )
            UECAD(J1,K,2) = CADBUF(M+NSFT  )
            UECAD(J1,K,3) = CADBUF(M+NSFT*2)
            UECAD(J1,K,4) = CADBUF(M+NSFT*3)
            IF(K.EQ.1)
     $      UECAD(J1,K,5) = CADBUF(M+NSFT*4)
            IF(K.EQ.1)
     $      UECAD(J1,K,6) = CADBUF(M+NSFT*5)
         ENDDO
         ENDDO
      ENDDO
      ENDIF
C
C ... (3) 南側境界値の受信
C
      IF( JSCAD.GT.0 ) THEN
      DO N=1,NB_CADMAS
         NIST1 = IICAD(1,N)
         IF( NIST1.EQ.0 ) CYCLE
C
         M     = LB_STOC
         IRANK = IB_CADMAS(N)
         ISIZE = NIST1*NKST*6
         ITAG  = ITAGSC*23+NB_CADMAS*(M-1)+N-1
         CALL MPI_IRECV(CADBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
         M = 0
         NSFT = NIST1*NKST
         DO K=1,NKST
         DO I=1,NIST1
            M = M+1
            IF( I.EQ.1    .AND.IICAD(5,N).EQ.1 ) CYCLE
            IF( I.EQ.NIST1.AND.IICAD(6,N).EQ.1 ) CYCLE
            I1 = I-1+IICAD(3,N)-IWCAD+(IIOFF(1)-1)
            VSCAD(I1,K,1) = CADBUF(M       )
            VSCAD(I1,K,2) = CADBUF(M+NSFT  )
            VSCAD(I1,K,3) = CADBUF(M+NSFT*2)
            VSCAD(I1,K,4) = CADBUF(M+NSFT*3)
            IF(K.EQ.1)
     $      VSCAD(I1,K,5) = CADBUF(M+NSFT*4)
            IF(K.EQ.1)
     $      VSCAD(I1,K,6) = CADBUF(M+NSFT*5)
         ENDDO
         ENDDO
      ENDDO
      ENDIF
C
C ... (4) 北側境界値の受信
C
      IF( JNCAD.GT.0 ) THEN
      DO N=1,NB_CADMAS
         NIST1 = IICAD(2,N)
         IF( NIST1.EQ.0 ) CYCLE
C
         M     = LB_STOC
         IRANK = IB_CADMAS(N)
         ISIZE = NIST1*NKST*6
         ITAG  = ITAGSC*24+NB_CADMAS*(M-1)+N-1
         CALL MPI_IRECV(CADBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
         M = 0
         NSFT = NIST1*NKST
         DO K=1,NKST
         DO I=1,NIST1
            M = M+1
            IF( I.EQ.1    .AND.IICAD(5,N).EQ.1 ) CYCLE
            IF( I.EQ.NIST1.AND.IICAD(6,N).EQ.1 ) CYCLE
            I1 = I-1+IICAD(3,N)-IWCAD+(IIOFF(1)-1)
            VNCAD(I1,K,1) = CADBUF(M       )
            VNCAD(I1,K,2) = CADBUF(M+NSFT  )
            VNCAD(I1,K,3) = CADBUF(M+NSFT*2)
            VNCAD(I1,K,4) = CADBUF(M+NSFT*3)
            IF(K.EQ.1)
     $      VNCAD(I1,K,5) = CADBUF(M+NSFT*4)
            IF(K.EQ.1)
     $      VNCAD(I1,K,6) = CADBUF(M+NSFT*5)
         ENDDO
         ENDDO
      ENDDO
      ENDIF
C
      RETURN
      END
