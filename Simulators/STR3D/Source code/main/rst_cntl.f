      SUBROUTINE RST_CNTL(KK)

      USE M_VAL

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION KK(*)

      REAL(8), POINTER :: TIME(:)

      NSTEP = KK(7)
      NRST = KK(83)

      ALLOCATE( TIME(NSTEP) )

      TIME(1) = D_T(1)

      DO I = 2, NSTEP
        TIME(I) = TIME(I-1) + D_T(I)
      ENDDO

      IF( RTIM(1) > 0. ) THEN

        RTIME = RTIM(1)

        EPS_MIN = DABS( RTIME - TIME(1) )

        DO J = 2, NSTEP
          EPS = DABS( RTIME - TIME(J) )
          IF( EPS < EPS_MIN ) THEN
            EPS_MIN = EPS
          ELSE
            EXIT
          ENDIF
        ENDDO

        KK(84) = J - 1

      ENDIF

      ALLOCATE( IROUT(NSTEP) )

      IROUT(:) = 0

      IF( RTIM(2) > 0. ) THEN

        RDT = RTIM(2)

        RTIME = 0.

        JS = 1

        DO

          RTIME = RTIME + RDT

          EPS_MIN = DABS( RTIME - TIME(JS) )

          DO J = JS + 1, NSTEP
            EPS = DABS( RTIME - TIME(J) )
            IF( EPS < EPS_MIN ) THEN
              EPS_MIN = EPS
            ELSE
              EXIT
            ENDIF
          ENDDO

          IROUT( J - 1 ) = 1

          IF( J == NSTEP + 1 ) EXIT

          JS = J

        ENDDO

        DO J = 1, NSTEP
          IF( IROUT(J) == 1 ) KK(83) = KK(83) + 1
        ENDDO

      ELSEIF( NRST > 0 ) THEN

        JS = 1

        DO I = 1, NRST

          RTIME = RTIM(2+I)

          EPS_MIN = DABS( RTIME - TIME(JS) )

          DO J = JS + 1, NSTEP
            EPS = DABS( RTIME - TIME(J) )
            IF( EPS < EPS_MIN ) THEN
              EPS_MIN = EPS
            ELSE
              EXIT
            ENDIF
          ENDDO

          IROUT( J - 1 ) = 1

          JS = J

        ENDDO

      ENDIF

      DEALLOCATE( TIME )

      END
