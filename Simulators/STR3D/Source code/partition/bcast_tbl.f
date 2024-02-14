      SUBROUTINE BCAST_TBL(KK,RR,ICK)

      USE M_VAL
      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION KK(200),RR(50),ICK(20)

      CALL M_MPI_BCAST_I(KK,200)
      CALL M_MPI_BCAST_D(RR,50)
      CALL M_MPI_BCAST_I(ICK,20)

      IF( MYRANK > 0 ) THEN

        ALLOCATE( ISUB(13,KK(4)) )

        ALLOCATE( RTIM(2+KK(83)) )

        IF( KK(15) > 0 ) ALLOCATE( RODA(KK(15)) )

        ALLOCATE( MAT(2,KK(11)) )
        ALLOCATE( AMAT(33,KK(11)) )

        IF( KK(40) > 0 ) THEN
          ALLOCATE( LOAD(2,KK(40)) )
          ALLOCATE( SLOD(KK(40)) )
          ALLOCATE( SILD(KK(41)) )
          ALLOCATE( LILD(KK(41)) )
        ENDIF

        IF( KK(60) > 0 ) THEN
          ALLOCATE( IGRV(KK(60)) )
          ALLOCATE( GRAV(3,KK(60)) )
        ENDIF

        IF( KK(46) > 0 ) THEN
          ALLOCATE( ISPA(2,KK(46)) )
          ALLOCATE( NSPA(KK(47)) )
        ENDIF

        IF( KK(66) > 0 ) THEN
          ALLOCATE( IDLD(2,KK(66)) )
          ALLOCATE( SDLD(KK(66)) )
          ALLOCATE( SIDL(KK(67)) )
          ALLOCATE( LIDL(KK(67)) )
        ENDIF

        IF( KK(70) > 0 ) THEN
          ALLOCATE( ITD1(KK(70)) )
          ALLOCATE( TBD1(2,KK(71)) )
        ENDIF

        IF( KK(78) > 0 ) THEN
          ALLOCATE( ISTP(2,KK(78)) )
          ALLOCATE( NSTP(2,KK(79)) )
          ALLOCATE( DELT(KK(79)) )
        ENDIF

        IF( KK(77) > 0 ) ALLOCATE( ITL1(4,KK(77)) )

      ENDIF

      CALL M_MPI_BCAST_I(ISUB,13*KK(4))

      CALL M_MPI_BCAST_D(RTIM,2+KK(83))

      IF( KK(15) > 0 ) CALL M_MPI_BCAST_D(RODA,KK(15))

      CALL M_MPI_BCAST_I(MAT,2*KK(11))
      CALL M_MPI_BCAST_D(AMAT,33*KK(11))

      IF( KK(40) > 0 ) THEN
        CALL M_MPI_BCAST_I(LOAD,2*KK(40))
        CALL M_MPI_BCAST_D(SLOD,KK(40))
        CALL M_MPI_BCAST_D(SILD,KK(41))
        CALL M_MPI_BCAST_I(LILD,KK(41))
      ENDIF

      IF( KK(60) > 0 ) THEN
        CALL M_MPI_BCAST_I(IGRV,KK(60))
        CALL M_MPI_BCAST_D(GRAV,3*KK(60))
      ENDIF

      IF( KK(46) > 0 ) THEN
        CALL M_MPI_BCAST_I(ISPA,2*KK(46))
        CALL M_MPI_BCAST_I(NSPA,KK(47))
      ENDIF

      IF( KK(66) > 0 ) THEN
        CALL M_MPI_BCAST_I(IDLD,2*KK(66))
        CALL M_MPI_BCAST_D(SDLD,KK(66))
        CALL M_MPI_BCAST_D(SIDL,KK(67))
        CALL M_MPI_BCAST_I(LIDL,KK(67))
      ENDIF

      IF( KK(70) > 0 ) THEN
        CALL M_MPI_BCAST_I(ITD1,KK(70))
        CALL M_MPI_BCAST_D(TBD1,2*KK(71))
      ENDIF

      IF( KK(78) > 0 ) THEN
        CALL M_MPI_BCAST_I(ISTP,2*KK(78))
        CALL M_MPI_BCAST_I(NSTP,2*KK(79))
        CALL M_MPI_BCAST_D(DELT,KK(79))
      ENDIF

      IF( KK(77) > 0 ) CALL M_MPI_BCAST_I(ITL1,4*KK(77))

      IF( KK(92) == 0 ) RETURN

      IF( MYRANK > 0 ) THEN

        IF( KK(88) > 0 ) THEN
          ALLOCATE( ICPA(2,KK(88)) )
          ALLOCATE( NCPA(KK(89)) )
        ENDIF

        ALLOCATE( ICPR(2,KK(90)) )
        ALLOCATE( NCPR(2,KK(91)) )

        ALLOCATE( CPR(2,KK(90)) )

        ALLOCATE( ICRG(2,KK(92)) )

      ENDIF

      IF( KK(88) > 0 ) THEN
        CALL M_MPI_BCAST_I(ICPA,2*KK(88))
        CALL M_MPI_BCAST_I(NCPA,KK(89))
      ENDIF

      CALL M_MPI_BCAST_I(ICPR,2*KK(90))
      CALL M_MPI_BCAST_I(NCPR,2*KK(91))

      CALL M_MPI_BCAST_D(CPR,2*KK(90))

      CALL M_MPI_BCAST_I(ICRG,2*KK(92))

      END
