      SUBROUTINE SF_PFACE1()

      USE VF_A2ARRAY
      USE SF_ARRAY

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION IP(4)

      REAL(8), POINTER :: PFG(:,:)
      INTEGER, POINTER :: ISEND(:)
      INTEGER, ALLOCATABLE :: IRECV(:)

      DATA EPS / 1.D-3 /
!-----------------------------------------------------------------------
      ALLOCATE( PFG(3,NPFC) )

      PFG(:,:) = 0.

      IPFACE(1,:) = 0

      DO 50 IPFC = 1, NPFC

        NIP = IPFACE(4,IPFC)

        IP(1:NIP) = IPFACE(5:4+NIP,IPFC)

        DO I = 1, NIP
          PFG(:,IPFC) = PFG(:,IPFC) + POS(:,IP(I))
        ENDDO

        PFG(:,IPFC) = PFG(:,IPFC) / DBLE(NIP)  ! 面重心

        DO I = MYIS, MYIE
          IF( PFG(1,IPFC) >= XX(1,I) .AND.
     &        PFG(1,IPFC) < XX(1,I+1) ) THEN
            DO J = MYJS, MYJE
              IF( PFG(2,IPFC) >= YY(1,J) .AND.
     &            PFG(2,IPFC) < YY(1,J+1) ) THEN
                DO K = 2, NUMK - 1
                  IF( PFG(3,IPFC) >= ZZ(1,K) .AND.
     &                PFG(3,IPFC) < ZZ(1,K+1) ) THEN
                    IPFACE(1,IPFC) = I + NUMI*(J-1) + NUMI*NUMJ*(K-1)  ! 面重心が属するセル
                    GOTO 50
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDIF
        ENDDO

   50 CONTINUE

      DO IPFC = 1, NPFC
        
        IF( IPFACE(1,IPFC) == 0 ) CYCLE

        IJK = IPFACE(1,IPFC)
        CALL SF_IJK(I,J,K,IJK)

        IF( NF(I,J,K) == -1 ) THEN
          IPFACE(1,IPFC) = 0
          CYCLE
        ENDIF

        ! 面重心が，属するセルの充分内部 -> 調べない

        IF( PFG(1,IPFC) >= XX(1,I  ) + EPS .AND. 
     &      PFG(1,IPFC) <= XX(1,I+1) - EPS .AND.
     &      PFG(2,IPFC) >= YY(1,J  ) + EPS .AND.
     &      PFG(2,IPFC) <= YY(1,J+1) - EPS .AND.
     &      PFG(3,IPFC) >= ZZ(1,K  ) + EPS .AND.
     &      PFG(3,IPFC) <= ZZ(1,K+1) - EPS ) CYCLE

        DO L = 1, NUMB0

          IJKB = INDB0(1,L)

          CALL SF_IJK(IB,JB,KB,IJKB)

          NS = INDB0(2,L)

          ! 面重心が境界面近傍 -> IPFACE(1,IPFC) = 0 とする
 
          SELECT CASE( NS )
          CASE( 1 )
            IF( IB == I .AND. JB == J .AND. KB == K .AND.
     &          PFG(1,IPFC) < XX(1,IB) + EPS ) IPFACE(1,IPFC) = 0
          CASE( 2 )
            IF( IB == I + 1 .AND. JB == J .AND. KB == K .AND.
     &          PFG(1,IPFC) > XX(1,IB) - EPS ) IPFACE(1,IPFC) = 0
          CASE( 3 )
            IF( IB == I .AND. JB == J .AND. KB == K .AND.
     &          PFG(2,IPFC) < YY(1,JB) + EPS ) IPFACE(1,IPFC) = 0
          CASE( 4 )
            IF( IB == I .AND. JB == J + 1 .AND. KB == K .AND.
     &          PFG(2,IPFC) > YY(1,JB) - EPS ) IPFACE(1,IPFC) = 0
          CASE( 5 )
            IF( IB == I .AND. JB == J .AND. KB == K .AND.
     &          PFG(3,IPFC) < ZZ(1,KB) + EPS ) IPFACE(1,IPFC) = 0
          CASE( 6 )
            IF( IB == I .AND. JB == J .AND. KB == K + 1 .AND.
     &          PFG(3,IPFC) > ZZ(1,KB) - EPS ) IPFACE(1,IPFC) = 0
          END SELECT

          IF( IPFACE(1,IPFC) == 0 ) EXIT

        ENDDO

      ENDDO

      DEALLOCATE( PFG )

      ! IPAFCE(1,:) 集約 -> IPFACE0(1,:)

      ALLOCATE( ISEND(NPFC) )
      IF( MYRANK == 0 ) ALLOCATE( IRECV(NPFC0) )

      ISEND(:) = IPFACE(1,:)

      IF( IPART == 0 ) THEN
        CALL SF_MPI_REDUCE_I(ISEND,IRECV,NPFC,0)
      ELSEIF( IPART == 1 ) THEN
        CALL SF_REDUCE_I(IRECV,NPFC0,ISEND,IPFNO,NPFC)
      ENDIF

      IF( MYRANK == 0 ) IPFACE0(1,:) = IRECV(:)

      DEALLOCATE( ISEND )
      IF( MYRANK == 0 ) DEALLOCATE( IRECV )

      END