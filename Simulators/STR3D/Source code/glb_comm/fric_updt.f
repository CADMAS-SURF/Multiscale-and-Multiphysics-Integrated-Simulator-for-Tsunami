      SUBROUTINE FRIC_UPDT(IFRIC,FRIC,U0,RL0,NP,MINC,NINC,INC,INDCP)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION IFRIC(10,*),FRIC(10,*),NINC(NP),INC(MINC,NP),INDCP(2,*)
     &         ,U0(3,4,*),RL0(3,*)

      INTEGER, POINTER :: IFRICW(:,:)
      REAL(8), POINTER :: FRICW(:,:),U0W(:,:,:),RL0W(:,:)

      ALLOCATE( IFRICW(10,MAXVAL(NINC)) )
      ALLOCATE( FRICW(10,MAXVAL(NINC)) )
      ALLOCATE( U0W(3,4,MAXVAL(NINC)) )
      ALLOCATE( RL0W(3,MAXVAL(NINC)) )

      DO IP = 1, NP

        N = NINC(IP)

        IF( N == 0 ) CYCLE

        CALL M_MPI_RECV_I(IFRICW,10*N,IP)
        CALL M_MPI_RECV_D(FRICW,10*N,IP)
        CALL M_MPI_RECV_D(U0W,12*N,IP)
        CALL M_MPI_RECV_D(RL0W,3*N,IP)

        DO I = 1, N
          J = INC(I,IP)
          IF( INDCP(1,J) == IP ) THEN
            IFRIC(:,J) = IFRICW(:,I)
            FRIC(:,J) = FRICW(:,I)
            U0(:,:,J) = U0W(:,:,I)
            RL0(:,J) = RL0W(:,I)
          ENDIF
        ENDDO

      ENDDO

      DEALLOCATE( IFRICW )
      DEALLOCATE( FRICW )
      DEALLOCATE( U0W )
      DEALLOCATE( RL0W )

      END
