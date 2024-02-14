      SUBROUTINE ARCONE
C======================================================================
C     全体領域おける分割領域の計算範囲を求める
C======================================================================
      use mod_comm,only: comm_model
      IMPLICIT NONE
C
      INCLUDE 'mpif.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GRID.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'GLOBAL.h'
C
      INTEGER::IXYZ(3)
      INTEGER::ISTAT(MPI_STATUS_SIZE)
C
      INTEGER::IERR,IERROR,IPARNT,IREQ,ISTOP,ITAG,NCOUNT
      INTEGER::N,L,NN,NS,NE,ICHILN,NNS,NNW,NNE,NNN,NNX,NNY,NNZ
      integer::m
C
      DO 100 N=1,NSIZE
        NN = IPECON(2,N)
        IF(NUMPE(1,NN+1).GT.1) THEN
          NS = NUMPE(2,NN+1)
          NE = NS+NUMPE(1,NN+1)-1
          NUMPE(1,NN+1) = - NUMPE(1,NN+1)
          DO 110 L=NS,NE
            IF(L.EQ.NS) THEN
              ICHILN = NUMCOM(1,L)
              NNE = IPECON(6,ICHILN+1)
              NNN = IPECON(7,ICHILN+1)
              IF(NRANK.EQ.NUMCOM(1,L)) THEN
                NNX = MXM-1
                NNY = MYM-1
                NNZ = MZM-1
                IXYZ(1) = NNX
                IXYZ(2) = NNY
                IXYZ(3) = 0
                INDCOM(1,L-NS+1) = 2
                INDCOM(2,L-NS+1) = MXM
                INDCOM(3,L-NS+1) = 2
                INDCOM(4,L-NS+1) = MYM
                INDCOM(5,L-NS+1) = 2
                INDCOM(6,L-NS+1) = MZM
C
                IF(NNE.GE.0) THEN
                  NCOUNT = 3
                  ITAG = 0
                  IXYZ(2) = 0
                  CALL MPI_ISEND( IXYZ,NCOUNT,MPI_INTEGER,NNE,ITAG,
     &                            comm_model,IREQ,IERR )
                  CALL MPI_WAIT(IREQ,ISTAT,IERROR)
                END IF
C
                IF(NNN.GE.0) THEN
                  NCOUNT = 3
                  ITAG = 0
                  IXYZ(1) = 0
                  IXYZ(2) = NNY
                  CALL MPI_ISEND( IXYZ,NCOUNT,MPI_INTEGER,NNN,ITAG,
     &                            comm_model,IREQ,IERR )
                  CALL MPI_WAIT(IREQ,ISTAT,IERROR)
                END IF
              ENDIF
            ELSE
C
C ... 東方向
              ICHILN = NUMCOM(1,L)
              IF(NNE.GE.0.AND.NNE.EQ.IPECON(1,ICHILN+1)) THEN
                IF(NRANK.EQ.NNE) THEN
                  NNW = IPECON(5,NRANK+1)
                  NCOUNT = 3
                  CALL MPI_IRECV( IXYZ,NCOUNT,MPI_INTEGER,NNW,
     &                         MPI_ANY_TAG,comm_model,IREQ,IERR )
                  CALL MPI_WAIT(IREQ,ISTAT,IERROR)
                  NNX = MXM-1+IXYZ(1)
                  NNY = MYM-1+IXYZ(2)
                  INDCOM(1,L-NS+1) = IXYZ(1)+2
                  INDCOM(2,L-NS+1) = IXYZ(1)+MXM
                  INDCOM(3,L-NS+1) = IXYZ(2)+2
                  INDCOM(4,L-NS+1) = IXYZ(2)+MYM
                  INDCOM(5,L-NS+1) = 2
                  INDCOM(6,L-NS+1) = MZM
                  IXYZ(1) = NNX
C
                  NNE = IPECON(6,NRANK+1)
                  IF(NNE.GE.0) THEN
                    NCOUNT = 3
                    ITAG = 0
                    CALL MPI_ISEND( IXYZ,NCOUNT,MPI_INTEGER,NNE,ITAG,
     &                              comm_model,IREQ,IERR )
                    CALL MPI_WAIT(IREQ,ISTAT,IERROR)
                  END IF
                ELSE
                  NNE = IPECON(6,ICHILN+1)
                END IF
                GO TO 110
              END IF
C
C ... 北方向
              IF(NNN.GE.0.AND.NNN.EQ.IPECON(1,ICHILN+1)) THEN
                IF(NRANK.EQ.NNN) THEN
                  NNS = IPECON(4,NRANK+1)
                  NCOUNT = 3
                  CALL MPI_IRECV( IXYZ,NCOUNT,MPI_INTEGER,NNS,
     &                         MPI_ANY_TAG,comm_model,IREQ,IERR )
                  CALL MPI_WAIT(IREQ,ISTAT,IERROR)
                  NNX = MXM-1+IXYZ(1)
                  NNY = MYM-1+IXYZ(2)
                  INDCOM(1,L-NS+1) = IXYZ(1)+2
                  INDCOM(2,L-NS+1) = IXYZ(1)+MXM
                  INDCOM(3,L-NS+1) = IXYZ(2)+2
                  INDCOM(4,L-NS+1) = IXYZ(2)+MYM
                  INDCOM(5,L-NS+1) = 2
                  INDCOM(6,L-NS+1) = MZM
                  IXYZ(1) = NNX
C
                  NNE = IPECON(6,NRANK+1)
                  IF(NNE.GE.0) THEN
                    NCOUNT = 3
                    ITAG = 0
                    CALL MPI_ISEND( IXYZ,NCOUNT,MPI_INTEGER,NNE,ITAG,
     &                                comm_model,IREQ,IERR )
                    CALL MPI_WAIT(IREQ,ISTAT,IERROR)
                  END IF
C
                  NNN = IPECON(7,NRANK+1)
                  IF(NNN.GE.0) THEN
                    NCOUNT = 3
                    ITAG = 0
                    IXYZ(1) = 0
                    IXYZ(2) = NNY
                    CALL MPI_ISEND( IXYZ,NCOUNT,MPI_INTEGER,NNN,ITAG,
     &                              comm_model,IREQ,IERR )
                    CALL MPI_WAIT(IREQ,ISTAT,IERROR)
                  END IF
                ELSE
                  NNE = IPECON(6,ICHILN+1)
                  NNN = IPECON(7,ICHILN+1)
                END IF
                GO TO 110
              END IF
            END IF
  110     CONTINUE
C
          DO 120 L=NS,NE
            IF(NRANK.EQ.NUMCOM(1,L)) GO TO 130
  120     CONTINUE
          GO TO 150
C
  130     CONTINUE
          NCOUNT = 6
          DO 140 L=NS,NE
            CALL MPI_BCAST( INDCOM(1,L-NS+1),NCOUNT,MPI_INTEGER,L-NS,
     &                      CHILDCOMM,IERR )
  140     CONTINUE
c.....
      do 145 l=ns,ne
      write(6,126) nrank,l-ns+1,(indcom(m,l-ns+1),m=1,6)
  126 format('nrank,l=',2i4,'   indcom=',6i4)
  145 continue
c.....
  150     CONTINUE
        END IF
  100 CONTINUE
C
      DO 160 N=0,NSIZE
        IF(NUMPE(1,N).LT.0) NUMPE(1,N)=-NUMPE(1,N)
  160 CONTINUE
C
      RETURN
      END
