      SUBROUTINE RECV_SURF0(KK,RR,ICHK)

      USE MPI_PARAM
      USE M_VAL
      USE M_PART

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION KK(*),RR(*)

      INTEGER, POINTER :: IFIL(:),LELM(:),LNOD(:),IPFCW(:,:)

      NNOD = KK(8)

      NPECG = NPROCS - 1

      CALL C_MPI_RECV_D(WLEVEL,1,IROOTC)
      CALL C_MPI_RECV_D(ALEVEL,1,IROOTC)
      CALL C_MPI_RECV_I(NPFC,1,IROOTC)

      RR(8) = WLEVEL
      RR(9) = ALEVEL

      CALL M_MPI_BCAST_D(WLEVEL,1)
      CALL M_MPI_BCAST_D(ALEVEL,1)

      KK(81) = NPFC

      ALLOCATE( IPFC(10,NPFC) )
      ALLOCATE( AFC(NPFC) )
      ALLOCATE( IPND(NNOD) )

      CALL C_MPI_RECV_I(IPFC,10*NPFC,IROOTC)
      CALL C_MPI_RECV_D(AFC,NPFC,IROOTC)
      CALL C_MPI_RECV_I(IPND,NNOD,IROOTC)

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

      IF( ISTM == 0 .AND. ICHK < 2 ) DEALLOCATE( IPFC )
      DEALLOCATE( AFC )
      DEALLOCATE( IPND )

      IF( ISTM == 0 ) RETURN

      ALLOCATE( IRF(NPFC) )
      ALLOCATE( IRND(NNOD) )

      CALL C_MPI_RECV_I(IRF,NPFC,IROOTC)
      CALL C_MPI_RECV_I(IRND,NNOD,IROOTC)

      CALL SCATTER_NODAL_I(IRND,1)

      DEALLOCATE( IRND )
      
      END
