      SUBROUTINE MDPRESS0(KK)

      USE M_VAL
      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*)
      INTEGER, POINTER :: IPM(:),IFD(:),IFND(:)
      REAL(8), POINTER :: PPM(:,:),PFD(:),RFD(:),PFND(:),RMAX(:)

      NNOD = KK(8)

      NPECG = NPROCS - 1

      ALLOCATE( IPND(NNOD) )
      ALLOCATE( POS(3,NNOD) )

      CALL GATHER_NODAL_I(IPND,1)
      CALL GATHER_NODAL_D(POS,3)

      NPM = 0

      DO I = 1, NNOD
        IF( IPND(I) < 0 ) NPM = NPM + 1
      ENDDO

      CALL M_MPI_BCAST_I(NPM,1)

      IF( NPM == 0 ) THEN
        DEALLOCATE( IPND )
        DEALLOCATE( POS )
        RETURN
      ENDIF

      ALLOCATE( IPM(NPM) )
      ALLOCATE( PPM(3,NPM) )

      J = 0

      DO I = 1, NNOD
        IF( IPND(I) < 0 ) THEN
          J = J + 1
          IPM(J) = I
          PPM(:,J) = POS(:,I)
        ENDIF
      ENDDO

      DEALLOCATE( IPND )
      DEALLOCATE( POS )

      CALL M_MPI_BCAST_D(PPM,3*NPM)

      ALLOCATE( IFD(NPM) )
      ALLOCATE( PFD(NPM) )
      ALLOCATE( RFD(NPM) )

      ALLOCATE( IFND(NNOD) )
      ALLOCATE( PFND(NNOD) )
      ALLOCATE( RMAX(NNOD) )

      IFND(:) = 0
      RMAX(:) = -1.D0

      DO IP = 1, NPECG

        CALL M_MPI_RECV_I(IFD,NPM,IP)
        CALL M_MPI_RECV_D(PFD,NPM,IP)
        CALL M_MPI_RECV_D(RFD,NPM,IP)

        DO I = 1, NPM

          J = IPM(I)

          IF( IFND(J) == 2 ) CYCLE

          IF( IFD(I) == 2 ) THEN
            IFND(J) = 2
            PFND(J) = PFD(I)
          ELSEIF( IFD(I) == 1 ) THEN
            IF( RFD(I) > RMAX(J) ) THEN
              RMAX(J) = RFD(I)
              IFND(J) = 1
              PFND(J) = PFD(I)
            ENDIF
          ENDIF

        ENDDO

      ENDDO

      CALL SCATTER_NODAL_I(IFND,1)
      CALL SCATTER_NODAL_D(PFND,1)

      DEALLOCATE( IPM )
      DEALLOCATE( PPM )

      DEALLOCATE( IFD )
      DEALLOCATE( PFD )
      DEALLOCATE( RFD )

      DEALLOCATE( IFND )
      DEALLOCATE( PFND )
      DEALLOCATE( RMAX )

      END
