      PROGRAM MAIN

      USE MOD_COMM, ONLY: INIT_MPMD
      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*256 FLNAME

      DIMENSION KK(200),RR(50),IFL(40),ICK(20)

      ICK(:) = 0

      ICK(1) = 0  ! INPUT DATA
      ICK(2) = 0  ! TEST MODE
      ICK(3) = 0  ! ENSIGHT OUTPUT
      ICK(4) = 0  ! PARTITION

      DO I = 1, 40
        IFL(I) = I
      ENDDO

      CALL GETPRM( FLNAME, MEM, ISLV, IPRE, ICK(2) )
      CALL FLNSET1( FLNAME, NLEN0, NLEN )

      CALL INIT_MPMD()

      CALL C_MPI_INIT()

      IF( MYRANK == 0 ) THEN

        KK(:) = 0

        KK(3)   = MEM         ! MEMORY(MB)
        KK(21)  = ISLV        ! MATRIX SOLVER
        KK(23)  = IPRE        ! PRE-CONDITIONING
        KK(110) = 4           ! MITER0
        KK(111) = 15          ! MITER1
        KK(112) = 10          ! MITERD0
        KK(113) = 25          ! MITERD1

        RR(:) = 0.

        RR(1)  = 1.D-9          ! CG TOLERANCE
        RR(3)  = 1.D-1          ! EPSA
        RR(4)  = 1.D0/3.D0      ! BETA
        RR(5)  = 1.D0           ! W4
        RR(6)  = 1.D-3          ! EPSR
        RR(7)  = 1.D0           ! ALPHA
        RR(10) = 1.D-4          ! EPS0
        RR(11) = 1.D-3          ! EPSD
        RR(12) = RR(11) * 1.D-1 ! EPSS
        RR(13) = 1.D-1          ! TOLA
        RR(14) = 1.D-6          ! EPSC
        RR(15) = 1.D-2          ! EPSF

        ITI  = IFL(10)
        ITO  = IFL(11)
        IFMP = IFL(15)

        OPEN( ITI, FILE = FLNAME(1:NLEN) )
        OPEN( ITO, FILE = FLNAME(1:NLEN0)//'.log' )

        CALL INPUT( KK, RR, ICK(1), ITI, ITO )

        CLOSE( ITI )

        IF( ICPL == 2 ) CALL C_MPI_SEND_I(IBDF,1,IROOTC)

        OPEN( IFMP, FILE = FLNAME(1:NLEN0)//'.neu' )

        CALL FEMAP_GEOM( KK, IFMP )

      ENDIF

!      IF( ICPL == 2 ) CALL C_MPI_BARRIER()

      IF( MOD(ISLV,10) == 1 .AND. NPROCS > 1 ) THEN

        IF( MYRANK == 0 .AND. KK(92) > 0 ) CALL CONT_TBL_G(KK,ITO)

        CALL PARTITION( KK, RR, IFL, ICK )

        IF( MYRANK == 0 ) THEN
          CALL GLB_COMM( KK, RR, IFL, FLNAME, NLEN0, ICK(3) )
        ELSE
          CALL ASTEA_MECHANICAL( KK, RR, IFL, FLNAME, NLEN0, ICK )
        ENDIF

      ELSE

        IF( MYRANK == 0 ) THEN
          CALL ASTEA_MECHANICAL( KK, RR, IFL, FLNAME, NLEN0, ICK )
        ELSE
          CALL MUMPS_PARA()
        ENDIF

      ENDIF

      IF( MYRANK == 0 ) THEN
        CLOSE( ITO )
        CLOSE( IFMP )
      ENDIF

      CALL MPI_FINALIZE( IERR )

      END
