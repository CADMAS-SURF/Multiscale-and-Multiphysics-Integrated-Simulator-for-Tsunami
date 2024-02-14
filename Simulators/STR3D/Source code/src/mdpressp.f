      SUBROUTINE MDPRESSP(IPND,PRESS,NNOD,NNODI,NIELG,GRID,POS,PG,IELC
     &                   ,IELCB,GELC,IELG,TIM,WLEVEL,ALEVEL,ISEQ)

      USE MPI_PARAM
      IMPLICIT REAL*8 (A-H,O-Z)
      LOGICAL AREAIN
      DIMENSION IPND(NNOD),PRESS(NNOD),POS(3,*),PG(*),IELC(3,*),GELC(*)
     &         ,IELG(NIELG),X(3,4),XG(3),RL(3),RN(3),IELCB(*)
     &         ,GRID(3,NNOD)
      INTEGER, POINTER :: IFD(:),IFND(:)
      REAL(8), POINTER :: PPM(:,:),PFD(:),RFD(:),PFND(:)
      DATA G /9.8D0/

      DO I = 1, NNOD
        IF( IPND(I) == 2 ) THEN
          IF( ISEQ == 0 .AND. TIM == 0.D0 ) THEN
            PRESS(I) = ( WLEVEL - GRID(3,I) ) * G * 1.D3
     &               + ( ALEVEL - WLEVEL ) * G
          ELSE
            PRESS(I) = PG(I)
          ENDIF
        ENDIF
      ENDDO

      IF( MYRANK == 1 ) CALL M_MPI_SEND_I(25,1,0)  ! SEND IOP=25 TO GLB_COMM

      CALL M_MPI_SEND_I(IPND,NNODI,0)
      CALL M_MPI_SEND_D(POS,3*NNODI,0)

      CALL M_MPI_BCAST_I(NPM,1)

      IF( NPM == 0 ) RETURN

      ALLOCATE( PPM(3,NPM) )

      CALL M_MPI_BCAST_D(PPM,3*NPM)

      ALLOCATE( IFD(NPM) )
      ALLOCATE( PFD(NPM) )
      ALLOCATE( RFD(NPM) )

      IFD(:) = 0

      DO I = 1, NPM

        RL_MIN = -1.D0

        X(:,4) = PPM(:,I)

        DO J = 1, NIELG

          JP = IELG(J)

          IF( IELCB(JP) == 0 ) CYCLE

          X(:,1:3) = POS(:,IELC(:,JP))

          CALL MEAN3(XG,X,3,3)

          CALL LENGTH(DIST,X(1,4),XG,3)

          IF( DIST > 2.D0*GELC(JP) ) CYCLE

          CALL AREACD(RL,RN,H,X)

          IF( DABS(H) >= GELC(JP) ) CYCLE

          IF( AREAIN(RL,1.D-2) ) THEN
            IFD(I) = 2
            IF( ISEQ == 0 .AND. TIM == 0.D0 ) THEN
              PFD(I) = ( WLEVEL - GRID(3,I) ) * G * 1.D3
     &               + ( ALEVEL - WLEVEL ) * G
            ELSE
              PFD(I) = RL(1) * PG( IELC(1,JP) )
     &               + RL(2) * PG( IELC(2,JP) )
     &               + RL(3) * PG( IELC(3,JP) )
            ENDIF
            EXIT
          ELSEIF( AREAIN(RL,1.D-1) ) THEN
            IF( MINVAL(RL) > RL_MIN ) THEN
              RL_MIN = MINVAL(RL)
              RFD(I) = RL_MIN
              IFD(I) = 1
              IF( ISEQ == 0 .AND. TIM == 0.D0 ) THEN
                PFD(I) = ( WLEVEL - GRID(3,I) ) * G * 1.D3
     &                 + ( ALEVEL - WLEVEL ) * G
              ELSE
                PFD(I) = RL(1) * PG( IELC(1,JP) )
     &                 + RL(2) * PG( IELC(2,JP) )
     &                 + RL(3) * PG( IELC(3,JP) )
              ENDIF
            ENDIF
          ENDIF

        ENDDO

      ENDDO

      DEALLOCATE( PPM )

      CALL M_MPI_SEND_I(IFD,NPM,0)
      CALL M_MPI_SEND_D(PFD,NPM,0)
      CALL M_MPI_SEND_D(RFD,NPM,0)

      DEALLOCATE( IFD )
      DEALLOCATE( PFD )
      DEALLOCATE( RFD )

      ALLOCATE( IFND(NNOD) )
      ALLOCATE( PFND(NNOD) )

      CALL M_MPI_RECV_I(IFND,NNOD,0)
      CALL M_MPI_RECV_D(PFND,NNOD,0)

      DO I = 1, NNOD
        IF( IFND(I) > 0 ) THEN
          IPND(I) = 2
          PRESS(I) = PFND(I)
        ENDIF
      ENDDO

      DEALLOCATE( IFND )
      DEALLOCATE( PFND )

      END
