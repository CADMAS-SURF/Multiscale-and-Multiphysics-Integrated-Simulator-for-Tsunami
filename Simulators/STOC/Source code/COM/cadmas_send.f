      SUBROUTINE CADMAS_SEND(UU,VV,WW,FF,ZC)
      use mod_comm,only: comm_ic_mg
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'mpif.h'
      INCLUDE 'CADMAS.h'
C
      REAL(8),INTENT(IN)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(IN)::FF(MX,MY,MZ),ZC(8,MZ)
C
C ... WORK VARIABLES
      INTEGER IERR,N,IRANK,ISIZE,ISTAT(MPI_STATUS_SIZE),IREQ,ITAG
C
      REAL(8):: FSUM
      INTEGER:: I,J,K,I1,I2,J1,J2,NSFT,M,M2,NUM,NJST1,NIST1
C
C
C ... (1) 時刻の送受信(デバッグ用)
C
      IF( LB_STOC.EQ.1 ) THEN
      DO N=1,NB_CADMAS
         IRANK = IB_CADMAS(N)
         ISIZE = 1
         M     = 1
         ITAG  = ITAGSC*16+NB_CADMAS*(M-1)+N-1
         CALL MPI_ISEND(TIME,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
C         WRITE(*,*) 'TIME=',TIME
      ENDDO
      ENDIF
C
C
C ... (2) 西側境界値の送信
C
      IF( IWCAD.GT.0 ) THEN
      I1 = IWCAD-1
      I2 = IWCAD
C
      DO N=1,NB_CADMAS
         NJST1 = JJCAD(1,N)
         IF( NJST1.EQ.0 ) CYCLE
C
         M  = 0
         NSFT = NJST1*NKST
         DO K=KBCAD,KTCAD
         DO J=JJCAD(3,N),JJCAD(4,N)
            M = M+1
            CADBUF(M       ) = UU(I1,J,K)
            CADBUF(M+NSFT  ) = 0.25D0*(VV(I1,J-1,K)+VV(I1,J,K)
     $                                +VV(I2,J-1,K)+VV(I2,J,K))
            CADBUF(M+NSFT*2) = 0.25D0*(WW(I1,J,K-1)+WW(I1,J,K)
     $                                +WW(I2,J,K-1)+WW(I2,J,K))
            CADBUF(M+NSFT*3) = 0.5D0*(FF(I1,J,K)+FF(I2,J,K))
         ENDDO
         ENDDO
C
         DO J=1,NJST1
            NUM = 0
            DO K=NKST,1,-1
               M = J + NJST1*(K-1) + NSFT*3
               IF( CADBUF(M).GT.1.0D-12 ) NUM = NUM+1
               IF( NUM.EQ.2 ) THEN
                  IF( CADBUF(M).LT.1.0D0 ) THEN
                     M2 = J + NJST1*K + NSFT*3
                     FSUM = CADBUF(M)*ZC(4,K) + CADBUF(M2)*ZC(4,K+1)
                     IF( FSUM.GT.ZC(4,K) ) THEN
                        CADBUF(M)  = 1.0D0
                        CADBUF(M2) = (FSUM-ZC(4,K))/ZC(4,K+1)
                     ELSE
                        CADBUF(M)  = FSUM/ZC(4,K)
                        CADBUF(M2) = 0.0D0
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
C
         M     = LB_STOC
         IRANK = IB_CADMAS(N)
         ISIZE = NJST1*NKST*4
         ITAG  = ITAGSC*17+NB_CADMAS*(M-1)+N-1
         CALL MPI_ISEND(CADBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
      ENDDO
      ENDIF
C
C
C ... (3) 東側境界値の送信
C
      IF( IECAD.GT.0 ) THEN
      I1 = IECAD
      I2 = IECAD+1
C
      DO N=1,NB_CADMAS
         NJST1 = JJCAD(2,N)
         IF( NJST1.EQ.0 ) CYCLE
C
         M = 0
         NSFT = NJST1*NKST
         DO K=KBCAD,KTCAD
         DO J=JJCAD(3,N),JJCAD(4,N)
            M = M+1
            CADBUF(M       ) = UU(I1,J,K)
            CADBUF(M+NSFT  ) = 0.25D0*(VV(I1,J-1,K)+VV(I1,J,K)
     $                                +VV(I2,J-1,K)+VV(I2,J,K))
            CADBUF(M+NSFT*2) = 0.25D0*(WW(I1,J,K-1)+WW(I1,J,K)
     $                                +WW(I2,J,K-1)+WW(I2,J,K))
            CADBUF(M+NSFT*3) = 0.5D0*(FF(I1,J,K)+FF(I2,J,K))
         ENDDO
         ENDDO
C
         DO J=1,NJST1
            NUM = 0
            DO K=NKST,1,-1
               M = J + NJST1*(K-1) + NSFT*3
               IF( CADBUF(M).GT.1.0D-12 ) NUM = NUM+1
               IF( NUM.EQ.2 ) THEN
                  IF( CADBUF(M).LT.1.0D0 ) THEN
                     M2 = J + NJST1*K + NSFT*3
                     FSUM = CADBUF(M)*ZC(4,K) + CADBUF(M2)*ZC(4,K+1)
                     IF( FSUM.GT.ZC(4,K) ) THEN
                        CADBUF(M)  = 1.0D0
                        CADBUF(M2) = (FSUM-ZC(4,K))/ZC(4,K+1)
                     ELSE
                        CADBUF(M)  = FSUM/ZC(4,K)
                        CADBUF(M2) = 0.0D0
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
C
         M     = LB_STOC
         IRANK = IB_CADMAS(N)
         ISIZE = NJST1*NKST*4
         ITAG  = ITAGSC*18+NB_CADMAS*(M-1)+N-1
         CALL MPI_ISEND(CADBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
      ENDDO
      ENDIF
C
C
C ... (4) 南側境界値の送信
C
      IF( JSCAD.GT.0 ) THEN
      J1 = JSCAD-1
      J2 = JSCAD
C
      DO N=1,NB_CADMAS
         NIST1 = IICAD(1,N)
         IF( NIST1.EQ.0 ) CYCLE
C
         M = 0
         NSFT = NIST1*NKST
         DO K=KBCAD,KTCAD
         DO I=IICAD(3,N),IICAD(4,N)
            M = M+1
            CADBUF(M       ) = 0.25D0*(UU(I-1,J1,K)+UU(I,J1,K)
     $                                +UU(I-1,J2,K)+UU(I,J2,K))
            CADBUF(M+NSFT  ) = VV(I,J1,K)
            CADBUF(M+NSFT*2) = 0.25D0*(WW(I,J1,K-1)+WW(I,J1,K)
     $                                +WW(I,J2,K-1)+WW(I,J2,K))
            CADBUF(M+NSFT*3) = 0.5D0*(FF(I,J1,K)+FF(I,J2,K))
         ENDDO
         ENDDO
C
         DO I=1,NIST1
            NUM = 0
            DO K=NKST,1,-1
               M = I + NIST1*(K-1) + NSFT*3
               IF( CADBUF(M).GT.1.0D-12 ) NUM = NUM+1
               IF( NUM.EQ.2 ) THEN
                  IF( CADBUF(M).LT.1.0D0 ) THEN
                     M2 = I + NIST1*K + NSFT*3
                     FSUM = CADBUF(M)*ZC(4,K) + CADBUF(M2)*ZC(4,K+1)
                     IF( FSUM.GT.ZC(4,K) ) THEN
                        CADBUF(M)  = 1.0D0
                        CADBUF(M2) = (FSUM-ZC(4,K))/ZC(4,K+1)
                     ELSE
                        CADBUF(M)  = FSUM/ZC(4,K)
                        CADBUF(M2) = 0.0D0
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
C
         M     = LB_STOC
         IRANK = IB_CADMAS(N)
         ISIZE = NIST1*NKST*4
         ITAG  = ITAGSC*19+NB_CADMAS*(M-1)+N-1
         CALL MPI_ISEND(CADBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
      ENDDO
      ENDIF
C
C
C ... (5) 北側境界値の送信
C
      IF( JNCAD.GT.0 ) THEN
      J1 = JNCAD
      J2 = JNCAD+1
C
      DO N=1,NB_CADMAS
         NIST1 = IICAD(2,N)
         IF( NIST1.EQ.0 ) CYCLE
C
         M = 0
         NSFT = NIST1*NKST
         DO K=KBCAD,KTCAD
         DO I=IICAD(3,N),IICAD(4,N)
            M = M+1
            CADBUF(M       ) = 0.25D0*(UU(I-1,J1,K)+UU(I,J1,K)
     $                                +UU(I-1,J2,K)+UU(I,J2,K))
            CADBUF(M+NSFT  ) = VV(I,J1,K)
            CADBUF(M+NSFT*2) = 0.25D0*(WW(I,J1,K-1)+WW(I,J1,K)
     $                                +WW(I,J2,K-1)+WW(I,J2,K))
            CADBUF(M+NSFT*3) = 0.5D0*(FF(I,J1,K)+FF(I,J2,K))
         ENDDO
         ENDDO
C
         DO I=1,NIST1
            NUM = 0
            DO K=NKST,1,-1
               M = I + NIST1*(K-1) + NSFT*3
               IF( CADBUF(M).GT.1.0D-12 ) NUM = NUM+1
               IF( NUM.EQ.2 ) THEN
                  IF( CADBUF(M).LT.1.0D0 ) THEN
                     M2 = I + NIST1*K + NSFT*3
                     FSUM = CADBUF(M)*ZC(4,K) + CADBUF(M2)*ZC(4,K+1)
                     IF( FSUM.GT.ZC(4,K) ) THEN
                        CADBUF(M)  = 1.0D0
                        CADBUF(M2) = (FSUM-ZC(4,K))/ZC(4,K+1)
                     ELSE
                        CADBUF(M)  = FSUM/ZC(4,K)
                        CADBUF(M2) = 0.0D0
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
C
         M     = LB_STOC
         IRANK = IB_CADMAS(N)
         ISIZE = NIST1*NKST*4
         ITAG  = ITAGSC*20+NB_CADMAS*(M-1)+N-1
         CALL MPI_ISEND(CADBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
      ENDDO
      ENDIF
C
      RETURN
      END
