      SUBROUTINE RDPRESS0(KK,RR,IFPRS,FLNAME,NLEN,ICHK)

      USE MPI_PARAM
      USE M_VAL
      USE M_PART

      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*256 FLNAME
      LOGICAL IEX
      DIMENSION KK(*),RR(*)

      INTEGER, POINTER :: IFIL(:),LELM(:),LNOD(:),IPFCW(:,:)

      INQUIRE(FILE=FLNAME(1:NLEN)//'.prs',EXIST=IEX)

      IF( IEX ) ICPL = 1

      CALL M_MPI_BCAST_I(ICPL,1)

      IF( ICPL == 0 ) RETURN

      OPEN(IFPRS,FILE=FLNAME(1:NLEN)//'.prs')

      READ(IFPRS,'(2F15.0)') WLEVEL,ALEVEL

      RR(8) = WLEVEL
      RR(9) = ALEVEL

      CALL M_MPI_BCAST_D(RR(8),2)

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

   10 NPTIM = IC

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

      NPECG = NPROCS - 1

      ALLOCATE( IFIL(NNOD) )

      ALLOCATE( NPF(NPECG) )

      DO IP = 1, NPECG

        IFIL(:) = 0

        IFIL( NOD(1:NN_INT(IP),IP) ) = 1

        IC = 0

        DO I = 1, NPFC
          N = IPFC(2,I)
          DO J = 1, N
            IF( IFIL( IPFC(2+J,I) ) == 1 ) THEN
              IC = IC + 1
              EXIT
            ENDIF
          ENDDO
        ENDDO

        NPF(IP) = IC

      ENDDO

      MPF = MAXVAL( NPF )

      ALLOCATE( IPF(MPF,NPECG) )

      ALLOCATE( LELM(KK(12)) )
      ALLOCATE( LNOD(NNOD) )
      ALLOCATE( IPFCW(10,MPF) )

      DO IP = 1, NPECG

        IFIL(:) = 0

        IFIL( NOD(1:NN_INT(IP),IP) ) = 1

        IC = 0

        DO I = 1, NPFC
          N = IPFC(2,I)
          DO J = 1, N
            IF( IFIL( IPFC(2+J,I) ) == 1 ) THEN
              IC = IC + 1
              IPF(IC,IP) = I
              EXIT
            ENDIF
          ENDDO
        ENDDO

        LELM(:) = 0

        DO I = 1, NE(IP)
          LELM( IEL(I,IP) ) = I
        ENDDO

        LNOD(:) = 0

        DO I = 1, NN_EXT(IP)
          LNOD( NOD(I,IP) ) = I
        ENDDO

        NPFCW = NPF(IP)

        IPFCW(1,1:NPFCW) = LELM( IPFC(1,IPF(1:NPFCW,IP)) )
        IPFCW(2,1:NPFCW) = IPFC(2,IPF(1:NPFCW,IP))
        DO I = 1, NPFCW
          N = IPFCW(2,I)
          IPFCW(3:2+N,I) = LNOD( IPFC(3:2+N,IPF(I,IP)) )
        ENDDO

        CALL M_MPI_SEND_I(NPFCW,1,IP)
        CALL M_MPI_SEND_I(IPFCW,10*NPFCW,IP)

      ENDDO

      DEALLOCATE( IFIL )

      DEALLOCATE( LELM )
      DEALLOCATE( LNOD )
      DEALLOCATE( IPFCW )

      CALL SCATTER_SURFACE(AFC,1)

      CALL SCATTER_NODAL_I(IPND,1)

      IF( ICHK < 2 ) DEALLOCATE( IPFC )
      DEALLOCATE( AFC )
      DEALLOCATE( IPND )

      CALL M_MPI_BCAST_I(NPTIM,1)

      CALL M_MPI_BCAST_D(PTIM,NPTIM)

      DO IT = 1, NPTIM
        CALL SCATTER_NODAL_D(PND(1,IT),1)
      ENDDO

      DEALLOCATE( PTIM )
      DEALLOCATE( PND )

      END
