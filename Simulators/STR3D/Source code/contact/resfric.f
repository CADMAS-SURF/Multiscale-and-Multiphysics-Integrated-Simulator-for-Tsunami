      SUBROUTINE RESFRIC(IFRIC,NNODI,NBDY,NINDC,INDOF,INDC,RES,FNRM1
     &                  ,RR,ISTEP,ITER2,ITER)

      USE MPI_PARAM
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION RNRM(NBDY,NBDY),IFRIC(10,NINDC),INDC(NINDC),INDOF(6,*)
     &         ,RES(*),NCONV(NBDY,NBDY),DSUM(NBDY,NBDY),RR(*)
      DATA ICHK / 0 /

      EPSA = RR(3) * 1.D-1
      EPSR = RR(6) * 1.D-1

      FNRM = FNRM1

      RNRM(:,:) = 0.D0

      DO I = 1, NINDC
        IF( IFRIC(1,I) == 0 ) CYCLE
        NS = INDC(I)
        IF( NS > NNODI ) CYCLE
        IBDY = IFRIC(5,I)
        JBDY = IFRIC(6,I)
        DO J = 1, 3
          IFR = INDOF(J,NS)
          IF( IFR > 0 ) 
     &      RNRM(IBDY,JBDY) = RNRM(IBDY,JBDY) + RES(IFR) * RES(IFR)
        ENDDO
      ENDDO

      IF( MYRANK > 0 ) THEN
        FNRM = FNRM * FNRM
        CALL CG_MPI_ALLREDUCE_D(FNRM,DUM,1,0)
        FNRM = DSQRT(DUM)
        CALL CG_MPI_ALLREDUCE_D(RNRM,DSUM,NBDY*NBDY,0)
        RNRM(:,:) = DSUM(:,:)
      ENDIF

      IF( ITER > 1 ) THEN

        DO I = 1, NBDY
          DO J = 1, NBDY
            RNRM(I,J) = DSQRT( RNRM(I,J) )
            IF( RNRM(I,J) / FNRM < EPSR .OR. RNRM(I,J) < EPSA ) THEN
              NCONV(I,J) = 0
            ELSE
              NCONV(I,J) = 1
            ENDIF
          ENDDO
        ENDDO

      ENDIF

      DO I = 1, NINDC
        IF( IFRIC(1,I) == 0 ) CYCLE
        IBDY = IFRIC(5,I)
        JBDY = IFRIC(6,I)
        IFRIC(7,I) = NCONV(IBDY,JBDY)
      ENDDO

      IF( ICHK == 0 ) RETURN

      WRITE(53,'(A,I6,2I3)') 'STEP, ITER2, ITER = ',ISTEP,ITER2,ITER

      DO I = 1, NBDY
        DO J = 1, NBDY
          IF( RNRM(I,J) > 0.D0 )
     &      WRITE(53,'(2I3,1PE10.2)') I,J,RNRM(I,J)
        ENDDO
      ENDDO

      END