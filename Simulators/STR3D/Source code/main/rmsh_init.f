      SUBROUTINE RMSH_INIT(IRMSH,RMSH,NNOD,NPFC,IRF,IRND,IPFC,GRID,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL AREAIN
      DIMENSION IRF(NPFC),IRND(NNOD),IPFC(10,NPFC),GRID(3,NNOD),NP(4)
     &         ,IP(5),X(2,4),RL(3),RL0(3),IRMSH(6,NNOD),RMSH(4,NNOD)
     &         ,Z(2)
      INTEGER, POINTER :: IFB(:,:,:)
      REAL(8), POINTER :: RLB(:,:,:)

      DATA IP / 1, 2, 3, 4, 1 /

      ALLOCATE( IFB(2,2,NNOD) )
      ALLOCATE( RLB(3,2,NNOD) )

      IFB(:,:,:) = 0
      RLB(:,:,:) = -1.D0

      DO I = 1, NNOD

        IF( IRND(I) /= -1 ) CYCLE

        DO J = 1, 2

          DO K = 1, NPFC

            IF( IRF(K) /= J ) CYCLE

            N = IPFC(2,K)
            NP(1:N) = IPFC(3:2+N,K)

            X(:,4) = GRID(1:2,I)

            SELECT CASE( N )
            CASE( 3 )

              X(:,1:3) = GRID(1:2,NP(1:3))

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

              X(:,3) = .25D0 * ( GRID(1:2,NP(1)) + GRID(1:2,NP(2)) +
     &                           GRID(1:2,NP(3)) + GRID(1:2,NP(4)) )

              DO L = 1, 4

                X(:,1:2) = GRID(1:2,NP(IP(L:L+1)))

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

          N = IPFC(2,K)
          NP(1:N) = IPFC(3:2+N,K)

          RL(:) = RLB(:,J,I)

          SELECT CASE( N )
          CASE( 3 )
            Z(J) = RL(1) * GRID(3,NP(1)) + RL(2) * GRID(3,NP(2))
     &           + RL(3) * GRID(3,NP(3))
          CASE( 4 ) 
            L = IFB(2,J,I)
            Z(J) = RL(1) * GRID(3,NP(IP(L)))
     &           + RL(2) * GRID(3,NP(IP(L+1)))
     &           + RL(3) * .25D0 * ( GRID(3,NP(1)) + GRID(3,NP(2))
     &                             + GRID(3,NP(3)) + GRID(3,NP(4)) )
          END SELECT

        ENDDO

        RZ = ( GRID(3,I) - Z(1) ) / ( Z(2) - Z(1) )

        IRMSH(1,I) = N
        IRMSH(2:1+N,I) = NP(1:N)
        IRMSH(6,I) = L
        RMSH(1:3,I) = RL(:)
        RMSH(4,I) = RZ

      ENDDO

      DEALLOCATE( IFB )
      DEALLOCATE( RLB )

      END
