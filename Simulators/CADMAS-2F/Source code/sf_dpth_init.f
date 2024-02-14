      SUBROUTINE SF_DPTH_INIT(XX,YY,ZZ,FF,GGW,GGR,NF)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
     &         ,FF(NUMI,NUMJ,NUMK),GGW(NUMI,NUMJ,NUMK)
     &         ,GGR(NUMI,NUMJ,NUMK),NF(NUMI,NUMJ,NUMK)

      REAL(8), POINTER :: H(:,:),HC(:,:),DP(:,:)
      REAL(8), ALLOCATABLE :: DSEND(:,:)

      IF( MYRANK == 0 ) THEN
        CALL SF_STM_C_MPI_SEND_I(ICPL,1,IROOTSTM)
        CALL SF_STM_C_MPI_RECV_D(PORS,1,IROOTSTM)
      ENDIF
      CALL VF_P1BCSD(PORS,1,0)
      PORS = DMAX1(PORS,2.D-2)

      ALLOCATE( H(NUMI,NUMJ) )
      IF( ICPL == 2 ) ALLOCATE( HC(NUMI,NUMJ) )
      ALLOCATE( DP(NUMI,NUMJ) )

      DO I = MYIS, MYIE
        DO J = MYJS, MYJE
          S = XX(2,I) * YY(2,J)
          VS = 0.D0
          DO K = 2, NUMK - 1
            IF( NF(I,J,K) < 0 ) THEN
              FAC = 1.D0
            ELSE
              FAC = 1.D0 - GGW(I,J,K)
            ENDIF
            VS = VS + S * ZZ(2,K) * FAC
          ENDDO
          H(I,J) = WVLVL - ( ZZ(1,2) + VS / S )
        ENDDO
      ENDDO

      IF( ICPL == 2 ) THEN
        DO I = MYIS, MYIE
          DO J = MYJS, MYJE
            S = XX(2,I) * YY(2,J)
            VC = 0.D0
            DO K = 2, NUMK - 1
              VC = VC + S * ZZ(2,K) * GGR(I,J,K)
            ENDDO
            HC(I,J) = VC / S
          ENDDO
        ENDDO
      ENDIF

      DO I = MYIS, MYIE
        DO J = MYJS, MYJE
          S = XX(2,I) * YY(2,J)
          VF = 0.D0
          DO K = 2, NUMK - 1
            IF( NF(I,J,K) >= 0 .AND. GGW(I,J,K) > PLOWER ) THEN
              VF = VF + S * ZZ(2,K) * GGW(I,J,K) * FF(I,J,K)
            ENDIF
          ENDDO
          DP(I,J) = VF / S
        ENDDO
      ENDDO

      IF( MYRANK == 0 ) THEN
        ALLOCATE( DSEND(NUMI0,NUMJ0) )
        CALL SF_STM_GATHER_D(DSEND,H,0)
        CALL SF_STM_C_MPI_SEND_D(DSEND,NUMI0*NUMJ0,IROOTSTM)
        IF( ICPL == 2 ) THEN
          CALL SF_STM_GATHER_D(DSEND,HC,0)
          CALL SF_STM_C_MPI_SEND_D(DSEND,NUMI0*NUMJ0,IROOTSTM)
        ENDIF
        CALL SF_STM_GATHER_D(DSEND,DP,0)
        CALL SF_STM_C_MPI_SEND_D(DSEND,NUMI0*NUMJ0,IROOTSTM)
        DEALLOCATE( DSEND )
      ELSE
        CALL SF_STM_GATHER_D(DSEND,H,0)
        IF( ICPL == 2 ) CALL SF_STM_GATHER_D(DSEND,HC,0)
        CALL SF_STM_GATHER_D(DSEND,DP,0)
      ENDIF

      DEALLOCATE( H )
      IF( ICPL == 2 ) DEALLOCATE( HC )
      DEALLOCATE( DP )

      END