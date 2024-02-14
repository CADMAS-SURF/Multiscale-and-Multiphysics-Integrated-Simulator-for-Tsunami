      SUBROUTINE TSTEPSET(ISID,KK)

      USE M_VAL

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION KK(*)

      DO I = 1, KK(78)
        IF( ISTP(1,I) == ISID ) THEN
          CALL ADDSET3(IS,IE,ISTP,I)
          NSTEP = 0
          DO J = IS, IE
            NSTEP = NSTEP + NSTP(1,J)
          ENDDO
          EXIT
        ENDIF
      ENDDO

      KK(7) = NSTEP

      ALLOCATE( D_T(NSTEP) )
      ALLOCATE( IOUT(NSTEP) )

      IOUT(:) = 0

      IP = 0

      DO J = IS, IE
        NI = NSTP(1,J)
        NOI = NSTP(2,J)
        DO K = 1, NI
          IP = IP + 1
          D_T(IP) = DELT(J)
          IF( MOD(K,NOI) == 0 .OR. K == NI ) IOUT(IP) = 1
        ENDDO
      ENDDO
          
      KK(6) = 50

      END
