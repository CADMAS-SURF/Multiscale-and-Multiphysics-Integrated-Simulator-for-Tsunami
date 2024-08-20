      SUBROUTINE SF_DPTH_UPDATE(DZ,SUMZ,XX,YY,ZZ,UU,VV,FF,FX,FY,GGX,GGY
     &                         ,GGW,GGR,LNDC,NF,INDX,INDY,FLFU,FLFV,DT
     &                         ,DTO)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_ATIMER.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION DZ(NUMI,NUMJ),XX(MAXG1,NUMI),YY(MAXG1,NUMJ)
     &         ,ZZ(MAXG1,NUMK),UU(NUMI,NUMJ,NUMK),VV(NUMI,NUMJ,NUMK)
     &         ,FF(NUMI,NUMJ,NUMK),FX(NUMI,NUMJ,NUMK),FY(NUMI,NUMJ,NUMK)
     &         ,GGX(NUMI,NUMJ,NUMK),GGY(NUMI,NUMJ,NUMK)
     &         ,GGW(NUMI,NUMJ,NUMK),GGR(NUMI,NUMJ,NUMK)
     &         ,NF(NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
     &         ,INDY(NUMI,NUMJ,NUMK),LNDC(NUMI,NUMJ),SUMZ(NUMI,NUMJ)
     &         ,FLFU(NUMI,NUMJ,NUMK),FLFV(NUMI,NUMJ,NUMK)

      REAL(8), POINTER :: DDZ(:,:),HC(:,:),DP(:,:),FLX(:,:),FLY(:,:)
      INTEGER, ALLOCATABLE :: ISEND(:,:)
      REAL(8), ALLOCATABLE :: DRECV(:,:),DSEND(:,:)

      ALLOCATE( DDZ(NUMI,NUMJ) )

      IF( MYRANK == 0 ) THEN
        ALLOCATE( DRECV(NUMI0,NUMJ0) )
        CALL SF_STM_C_MPI_RECV_D(DRECV,NUMI0*NUMJ0,IROOTSTM)
        CALL SF_STM_SCATTER_D(DDZ,DRECV)
        DEALLOCATE( DRECV )
      ELSE
        CALL SF_STM_SCATTER_D(DDZ,DRECV)
      ENDIF

      DDZ(:,:) = DDZ(:,:) * DT / DTO

      IF( ICPL == 2 ) DZ(:,:) = DZ(:,:) + DDZ(:,:)

      SUMZ(:,:) = SUMZ(:,:) + DDZ(:,:)

      DEALLOCATE( DDZ )

      IF( ICPL == 2 ) ALLOCATE( HC(NUMI,NUMJ) )
      ALLOCATE( DP(NUMI,NUMJ) )
      ALLOCATE( FLX(NUMI,NUMJ) )
      ALLOCATE( FLY(NUMI,NUMJ) )

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

      IA = MYIS
      IB = MYIE
      JA = MYJS
      JB = MYJE
      IF( MYMIE == 1 ) IB = IB + 1
      IF( MYMJE == 1 ) JB = JB + 1

      DO I = IA, IB
        DO J = MYJS, MYJE
          FLX(I,J) = 0.D0
          DO K = 2, NUMK - 1
            IF( INDX(I,J,K) >= 0 .AND.
     &          GGW(I-1,J,K) > PLOWER .AND. GGW(I,J,K) > PLOWER ) THEN
!              IF( MYMIS == 1 .AND. I == IA ) THEN
!                F = FF(I,J,K)
!              ELSEIF( MYMIE == 1 .AND. I == IB ) THEN
!                F = FF(I-1,J,K)
!              ELSE
!                F = FX(I,J,K)
!              ENDIF
!              FLX(I,J) = FLX(I,J) + UU(I,J,K) * GGX(I,J,K) * ZZ(2,K) * F
              FLX(I,J) = FLX(I,J) + FLFU(I,J,K) * ZZ(2,K)
            ENDIF
          ENDDO
        ENDDO
      ENDDO

      DO I = MYIS, MYIE
        DO J = JA, JB
          FLY(I,J) = 0.D0
          DO K = 2, NUMK - 1
            IF( INDY(I,J,K) >= 0 .AND.
     &          GGW(I,J-1,K) > PLOWER .AND. GGW(I,J,K) > PLOWER ) THEN
!              IF( MYMJS == 1 .AND. J == JA ) THEN
!                F = FF(I,J,K)
!              ELSEIF( MYMJE == 1 .AND. J == JB ) THEN
!                F = FF(I,J-1,K)
!              ELSE
!                F = FY(I,J,K)
!              ENDIF
!              FLY(I,J) = FLY(I,J) + VV(I,J,K) * GGY(I,J,K) * ZZ(2,K) * F
              FLY(I,J) = FLY(I,J) + FLFV(I,J,K) * ZZ(2,K)
            ENDIF
          ENDDO
        ENDDO
      ENDDO

      IF( MYRANK == 0 ) THEN
        CALL SF_STM_C_MPI_SEND_I(0,1,IROOTSTM)
        CALL SF_STM_C_MPI_SEND_D(TNOW+DT,1,IROOTSTM)
        ALLOCATE( ISEND(NUMI0,NUMJ0) )
        ALLOCATE( DSEND(NUMI0,NUMJ0) )
        IF( ICPL == 2 ) THEN
          CALL SF_STM_GATHER_I(ISEND,LNDC,0)
          CALL SF_STM_C_MPI_SEND_I(ISEND,NUMI0*NUMJ0,IROOTSTM)
          CALL SF_STM_GATHER_D(DSEND,HC,0)
          CALL SF_STM_C_MPI_SEND_D(DSEND,NUMI0*NUMJ0,IROOTSTM)
        ENDIF
        CALL SF_STM_GATHER_D(DSEND,DP,0)
        CALL SF_STM_C_MPI_SEND_D(DSEND,NUMI0*NUMJ0,IROOTSTM)
        CALL SF_STM_GATHER_D(DSEND,FLX,1)
        DSEND(1:NUMI0-1,:) = DSEND(2:NUMI0,:)
        CALL SF_STM_C_MPI_SEND_D(DSEND,NUMI0*NUMJ0,IROOTSTM)
        CALL SF_STM_GATHER_D(DSEND,FLY,2)
        DSEND(:,1:NUMJ0-1) = DSEND(:,2:NUMJ0)
        CALL SF_STM_C_MPI_SEND_D(DSEND,NUMI0*NUMJ0,IROOTSTM)
        DEALLOCATE( ISEND )
        DEALLOCATE( DSEND )
      ELSE
        IF( ICPL == 2 ) THEN
          CALL SF_STM_GATHER_I(ISEND,LNDC,0)
          CALL SF_STM_GATHER_D(DSEND,HC,0)
        ENDIF
        CALL SF_STM_GATHER_D(DSEND,DP,0)
        CALL SF_STM_GATHER_D(DSEND,FLX,1)
        CALL SF_STM_GATHER_D(DSEND,FLY,2)
      ENDIF

      IF( ICPL == 2 ) DEALLOCATE( HC )
      DEALLOCATE( DP )
      DEALLOCATE( FLX )
      DEALLOCATE( FLY )

      END