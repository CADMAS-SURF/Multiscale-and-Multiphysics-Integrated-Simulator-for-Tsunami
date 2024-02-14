      SUBROUTINE RDPRESS(KK,RR,IFPRS,FLNAME,NLEN)

      USE MPI_PARAM
      USE M_VAL

      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*256 FLNAME
      LOGICAL IEX
      DIMENSION KK(*),RR(*)

      IF( MYRANK == 0 ) THEN

        INQUIRE(FILE=FLNAME(1:NLEN)//'.prs',EXIST=IEX)

        IF( IEX ) ICPL = 1

        IF( ICPL == 0 ) RETURN

        OPEN(IFPRS,FILE=FLNAME(1:NLEN)//'.prs')

        READ(IFPRS,'(2F15.0)') WLEVEL,ALEVEL

        RR(8) = WLEVEL
        RR(9) = ALEVEL

        READ(IFPRS,'(I10)') NPFC

        KK(81) = NPFC

        ALLOCATE( IPFC(10,NPFC) )
        ALLOCATE( AFC(NPFC) )

        DO I = 1, NPFC
          READ(IFPRS,'(10I10)') IPFC(:,I)
        ENDDO

        DO I = 1, NPFC
          READ(IFPRS,'(F15.0)') AFC(I)
        ENDDO

        NNOD = KK(8)

        ALLOCATE( IPND(NNOD) )

        DO I = 1, NNOD
          READ(IFPRS,'(I1)') IPND(I)
        ENDDO

        IC = 0

        DO

          READ(IFPRS,*,END=10) DUM

          DO I = 1, NNOD
            READ(IFPRS,*) DUM
          ENDDO

          IC = IC + 1

        ENDDO

   10   NPTIM = IC

        KK(82) = NPTIM

        ALLOCATE( PTIM(NPTIM) )
        ALLOCATE( PND(NNOD,NPTIM) )

        REWIND(IFPRS)

        READ(IFPRS,*) DUM

        READ(IFPRS,*) IDUM

        DO I = 1, NPFC
          READ(IFPRS,*) IDUM
        ENDDO

        DO I = 1, NPFC
          READ(IFPRS,*) DUM
        ENDDO

        DO I = 1, NNOD
          READ(IFPRS,*) IDUM
        ENDDO

        DO IT = 1, NPTIM

          READ(IFPRS,'(F15.0)') PTIM(IT)

          DO I = 1, NNOD
            READ(IFPRS,'(F15.0)') PND(I,IT)
          ENDDO

        ENDDO

        CLOSE( IFPRS )

      ELSE

        IF( MYRANK == 1 ) CALL M_MPI_SEND_I(10,1,0)  ! SEND IOP=10 TO GLB_COMM

        CALL M_MPI_BCAST_I(ICPL,1)

        IF( ICPL == 0 ) RETURN

        CALL M_MPI_BCAST_D(RR(8),2)

        CALL M_MPI_RECV_I(NPFC,1,0)

        KK(81) = NPFC

        NNOD = KK(8)

        ALLOCATE( IPFC(10,NPFC) )
        ALLOCATE( AFC(NPFC) )
        ALLOCATE( IPND(NNOD) )

        CALL M_MPI_RECV_I(IPFC,10*NPFC,0)
        CALL M_MPI_RECV_D(AFC,NPFC,0)
        CALL M_MPI_RECV_I(IPND,NNOD,0)

        CALL M_MPI_BCAST_I(NPTIM,1)

        KK(82) = NPTIM

        ALLOCATE( PTIM(NPTIM) )
        ALLOCATE( PND(NNOD,NPTIM) )

        CALL M_MPI_BCAST_D(PTIM,NPTIM)

        DO IT = 1, NPTIM
          CALL M_MPI_RECV_D(PND(1,IT),NNOD,0)
        ENDDO

      ENDIF

      END
