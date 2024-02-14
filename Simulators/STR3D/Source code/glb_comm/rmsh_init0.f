      SUBROUTINE RMSH_INIT0(KK,ICHK)

      USE MPI_PARAM
      USE M_VAL
      USE M_PART

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*)
      INTEGER, POINTER :: IRFACE(:,:),IRMSHW(:,:),IFIL(:),IDZR(:)
      REAL(8), POINTER :: RFACE(:,:,:)
      TYPE WRK
        INTEGER, POINTER :: IDZ(:)
      END TYPE WRK
      TYPE(WRK), POINTER :: WK(:)

      NNOD = KK(8)
      NPFC = KK(81)

      NPECG = NPROCS - 1

      IC = 0

      DO I = 1, NPFC
        IF( IRF(I) > 0 ) IC = IC + 1
      ENDDO

      NRFACE = IC

      ALLOCATE( IRFACE(10,NRFACE) )
      ALLOCATE( RFACE(3,4,NRFACE) )

      IC = 0

      DO I = 1, NPFC
        IF( IRF(I) > 0 ) THEN
          IC = IC + 1
          IRFACE(1,IC) = IRF(I)
          IRFACE(2:10,IC) = IPFC(2:10,I)
          N = IPFC(2,I)
          RFACE(:,1:N,IC) = GRID(:,IPFC(3:2+N,I))
        ENDIF
      ENDDO

      IF( ICHK == 0 ) DEALLOCATE( GRID )
      IF( ICHK < 2 ) DEALLOCATE( IPFC )
      DEALLOCATE( IRF )

      CALL M_MPI_BCAST_I(NRFACE,1)
      CALL M_MPI_BCAST_I(IRFACE,10*NRFACE)
      CALL M_MPI_BCAST_D(RFACE,12*NRFACE)

      DEALLOCATE( IRFACE )
      DEALLOCATE( RFACE )

      ALLOCATE( IRMSHW(6,MAXVAL(NN_EXT)) )
      ALLOCATE( IFIL(NNOD) )
      ALLOCATE( NDZ(NPECG) )
      ALLOCATE( WK(NPECG) )
      ALLOCATE( IDZR(NNOD) )

      DO IP = 1, NPECG

        CALL M_MPI_RECV_I(IRMSHW,6*NN_EXT(IP),IP)

        IFIL(:) = 0

        DO I = 1, NN_EXT(IP)
          N = IRMSHW(1,I)
          IFIL( IRMSHW(2:1+N,I) ) = 1
        ENDDO

        IC = 0

        DO I = 1, NNOD
          IF( IFIL(I) == 1 ) IC = IC + 1
        ENDDO

        NDZ(IP) = IC

        ALLOCATE( WK(IP)%IDZ(NDZ(IP)) )

        IC = 0

        DO I = 1, NNOD
          IF( IFIL(I) == 1 ) THEN
            IC = IC + 1
            WK(IP)%IDZ(IC) = I
          ENDIF
        ENDDO

        DO I = 1, NDZ(IP)
          IDZR( WK(IP)%IDZ(I) ) = I
        ENDDO

        DO I = 1, NN_EXT(IP)
          N = IRMSHW(1,I)
          IRMSHW(2:1+N,I) = IDZR( IRMSHW(2:1+N,I) )
        ENDDO

        CALL M_MPI_SEND_I(IRMSHW,6*NN_EXT(IP),IP)

      ENDDO

      DEALLOCATE( IRMSHW )
      DEALLOCATE( IFIL )
      DEALLOCATE( IDZR )

      ALLOCATE( IDZ(MAXVAL(NDZ),NPECG) )

      DO IP = 1, NPECG
        IDZ(1:NDZ(IP),IP) = WK(IP)%IDZ(:)
        DEALLOCATE( WK(IP)%IDZ )
      ENDDO

      END
