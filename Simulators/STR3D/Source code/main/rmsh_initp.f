      SUBROUTINE RMSH_INITP(IRMSH,RMSH,NNOD,IRND,GRID,ITO)

      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL AREAIN
      DIMENSION IRND(NNOD),GRID(3,NNOD),IP(5),X(2,4),RL(3),RL0(3)
     &         ,IRMSH(6,NNOD),RMSH(4,NNOD),Z(2)
      INTEGER, POINTER :: IRFACE(:,:),IFB(:,:,:)
      REAL(8), POINTER :: RFACE(:,:,:),RLB(:,:,:)

      DATA IP / 1, 2, 3, 4, 1 /

      IF( MYRANK == 1 ) CALL M_MPI_SEND_I(15,1,0)  ! SEND IOP=15 TO GLB_COMM

      CALL M_MPI_BCAST_I(NRFACE,1)

      ALLOCATE( IRFACE(10,NRFACE) )
      ALLOCATE( RFACE(3,4,NRFACE) )

      CALL M_MPI_BCAST_I(IRFACE,10*NRFACE)
      CALL M_MPI_BCAST_D(RFACE,12*NRFACE)

      ALLOCATE( IFB(2,2,NNOD) )
      ALLOCATE( RLB(3,2,NNOD) )

      IFB(:,:,:) = 0
      RLB(:,:,:) = -1.D0

      DO I = 1, NNOD

        IF( IRND(I) /= -1 ) CYCLE

        DO J = 1, 2

          DO K = 1, NRFACE

            IF( IRFACE(1,K) /= J ) CYCLE

            N = IRFACE(2,K)

            X(:,4) = GRID(1:2,I)

            SELECT CASE( N )
            CASE( 3 )

              X(:,1:3) = RFACE(1:2,1:3,K)

              CALL AREACD2(RL,X)

              IF( AREAIN(RL,1.D-2) ) THEN
                IFB(1,J,I) = K
                RLB(:,J,I) = RL(:)
                EXIT
              ELSEIF( AREAIN(RL,1.D-1) ) THEN
                RL0(:) = RLB(:,J,I)
                IF( MINVAL(RL) > MINVAL(RL0) ) THEN
                  IFB(1,J,I) = K
                  RLB(:,J,I) = RL(:)
                ENDIF
              ENDIF

            CASE( 4 )

              X(:,3) = .25D0 * ( RFACE(1:2,1,K) + RFACE(1:2,2,K) +
     &                           RFACE(1:2,3,K) + RFACE(1:2,4,K) )

              DO L = 1, 4

                X(:,1:2) = RFACE(1:2,IP(L:L+1),K)

                CALL AREACD2(RL,X)

                IF( AREAIN(RL,1.D-2) ) THEN
                  IFB(1,J,I) = K
                  IFB(2,J,I) = L
                  RLB(:,J,I) = RL(:)
                  GOTO 10
                ELSEIF( AREAIN(RL,1.D-1) ) THEN
                  RL0(:) = RLB(:,J,I)
                  IF( MINVAL(RL) > MINVAL(RL0) ) THEN
                    IFB(1,J,I) = K
                    IFB(2,J,I) = L
                    RLB(:,J,I) = RL(:)
                  ENDIF
                ENDIF

              ENDDO

            END SELECT

          ENDDO

   10     IF( IFB(1,J,I) == 0 ) THEN
            WRITE(ITO,'(/A)') 'STOP IN SUB. RMSH_INIT!'
            CALL ERRSTP(90,ITO)
          ENDIF

        ENDDO

      ENDDO

      IRMSH(:,:) = 0
      RMSH(:,:) = 0.D0

      DO I = 1, NNOD

        IF( IRND(I) /= -1 ) CYCLE

        DO J = 1, 2

          K = IFB(1,J,I)

          N = IRFACE(2,K)

          RL(:) = RLB(:,J,I)

          SELECT CASE( N )
          CASE( 3 )
            Z(J) = RL(1) * RFACE(3,1,K) + RL(2) * RFACE(3,2,K)
     &           + RL(3) * RFACE(3,3,K)
          CASE( 4 ) 
            L = IFB(2,J,I)
            Z(J) = RL(1) * RFACE(3,IP(L),K)
     &           + RL(2) * RFACE(3,IP(L+1),K)
     &           + RL(3) * .25D0 * ( RFACE(3,1,K) + RFACE(3,2,K)
     &                             + RFACE(3,3,K) + RFACE(3,4,K) )
          END SELECT

        ENDDO

        RZ = ( GRID(3,I) - Z(1) ) / ( Z(2) - Z(1) )

        IRMSH(1,I) = N
        IRMSH(2:1+N,I) = IRFACE(3:2+N,K)
        IRMSH(6,I) = L
        RMSH(1:3,I) = RL(:)
        RMSH(4,I) = RZ

      ENDDO

      DEALLOCATE( IRFACE )
      DEALLOCATE( RFACE )

      DEALLOCATE( IFB )
      DEALLOCATE( RLB )

      CALL M_MPI_SEND_I(IRMSH,6*NNOD,0)

      CALL M_MPI_RECV_I(IRMSH,6*NNOD,0)

      END
