      SUBROUTINE SF_PFACE2()

      USE VF_A2ARRAY
      USE SF_ARRAY

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION IP(7),JP(7),KP(7)

      INTEGER, POINTER :: ISEND(:)
      INTEGER, ALLOCATABLE :: IRECV(:)

      DATA EPS / 1.D-1 /

      ALLOCATE( ISEND(NNOD) )
      IF( MYRANK == 0 ) ALLOCATE( IRECV(NNOD0) )

      IF( IPART == 0 .AND. MYRANK == 0 ) THEN

        IRECV(:) = 0

        DO I = 1, NELM
          IF( IELM(2,I) == 2 ) THEN
            N = IELM(4,I)
            IRECV( IELM(5:4+N,I) ) = 1
          ENDIF
        ENDDO

      ELSEIF( IPART == 1 ) THEN

        ISEND(:) = 0

        DO I = 1, NELM
          IF( IELM(2,I) == 2 ) THEN
            N = IELM(4,I)
            ISEND( IELM(5:4+N,I) ) = 1
          ENDIF
        ENDDO

        CALL SF_REDUCE_I(IRECV,NNOD0,ISEND,IGNO,NNOD)

      ENDIF

      IF( MYRANK == 0 ) THEN

        IPGRID0(:,:) = 0

        DO I = 1, NNOD0
          IF( IRECV(I) > 0 ) IPGRID0(1,I) = 1  ! 石材料要素の構成節点
        ENDDO

        DO I = 1, NPFC0
          N = IPFACE0(4,I)
          IF( IPFACE0(1,I) > 0 ) IPGRID0( 1, IPFACE0(5:4+N,I) ) = 1   ! 有効面の構成節点
          IF( IPFACE0(2,I) == 1 ) IPGRID0( 2, IPFACE0(5:4+N,I) ) = 1  ! 地盤面の構成節点
        ENDDO

      ENDIF

      IF( IPART == 0 ) THEN
        IF( MYRANK == 0 ) IPGRID(:,:) = IPGRID0(:,:)
        CALL VF_P1BCSI(IPGRID,2*NNOD,0)
      ELSEIF( IPART == 1 ) THEN
        CALL SF_BCAST_I(IPGRID,IGNO,NNOD,IPGRID0,NNOD0,2)
      ENDIF

      DO 50 IG = 1, NNOD

        IF( IPGRID(1,IG) == 0 ) CYCLE

        DO I = MYIS, MYIE

          IF( I == MYIS .AND. MYMIS == 1 ) THEN
            XS = XX(1,I) - XX(2,I) * EPS
          ELSE
            XS = XX(1,I)
          ENDIF

          IF( I == MYIE .AND. MYMIE == 1 ) THEN
            XE = XX(1,I+1) + XX(2,I) * EPS
          ELSE
            XE = XX(1,I+1)
          ENDIF

          IF( POS(1,IG) >= XS .AND. POS(1,IG) < XE ) THEN

            DO J = MYJS, MYJE

              IF( J == MYJS .AND. MYMJS == 1 ) THEN
                YS = YY(1,J) - YY(2,J) * EPS
              ELSE
                YS = YY(1,J)
              ENDIF

              IF( J == MYJE .AND. MYMJE == 1 ) THEN
                YE = YY(1,J+1) + YY(2,J) * EPS
              ELSE
                YE = YY(1,J+1)
              ENDIF

              IF( POS(2,IG) >= YS .AND. POS(2,IG) < YE ) THEN

                DO K = 2, NUMK - 1

                  IF( K == 2 ) THEN
                    ZS = ZZ(1,K) - ZZ(2,K) * EPS
                  ELSE
                    ZS = ZZ(1,K)
                  ENDIF

                  IF( K == NUMK - 1 ) THEN
                    ZE = ZZ(1,K+1) + ZZ(2,K) * EPS
                  ELSE
                    ZE = ZZ(1,K+1)
                  ENDIF

                  IF( POS(3,IG) >= ZS .AND. POS(3,IG) < ZE ) THEN
                    IPGRID(1,IG) = I + NUMI*(J-1) + NUMI*NUMJ*(K-1)  ! 節点が属するセル
                    GOTO 50
                  ENDIF

                ENDDO

              ENDIF

            ENDDO

          ENDIF

        ENDDO

        IPGRID(1,IG) = 0  ! 領域外

   50 CONTINUE

      DO IG = 1, NNOD
        
        IF( IPGRID(1,IG) == 0 ) CYCLE

        IJK = IPGRID(1,IG)
        CALL SF_IJK(I,J,K,IJK)

        IF( NF(I,J,K) >= 0 ) CYCLE

        ! 属するセルが障害物なら近くを探す

        CALL SF_ADJOIN(IP,JP,KP,POS(1,IG),XX(1,I),XX(1,I+1),YY(1,J)
     &                ,YY(1,J+1),ZZ(1,K),ZZ(1,K+1))

        DO L = 1, 7
          ID = I + IP(L)
          JD = J + JP(L)
          KD = K + KP(L)
          IF( NF(ID,JD,KD) >= 0 ) EXIT  ! NF >= 0 で１番近いセル
        ENDDO

        IF( L == 8 ) THEN
          IPGRID(1,IG) = -1
        ELSE
          IPGRID(1,IG) = ID + NUMI*(JD-1) + NUMI*NUMJ*(KD-1)
        ENDIF

      ENDDO

      ! IPGRID(1,:) 集約 -> IPGRID0(1,:)

      ISEND(:) = IPGRID(1,:)

      IF( IPART == 0 ) THEN
        CALL SF_MPI_REDUCE_I(ISEND,IRECV,NNOD,0)
      ELSEIF( IPART == 1 ) THEN
        CALL SF_REDUCE_I(IRECV,NNOD0,ISEND,IGNO,NNOD)
      ENDIF

      IF( MYRANK == 0 ) IPGRID0(1,:) = IRECV(:)

      DEALLOCATE( ISEND )
      IF( MYRANK == 0 ) DEALLOCATE( IRECV )

      END