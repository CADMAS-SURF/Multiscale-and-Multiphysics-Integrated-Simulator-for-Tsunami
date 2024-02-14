      SUBROUTINE NPFLOW_S(FLI,FLO,FCP,VELG,MGP,VELE,RES,KK,RR,GRID,IELM
     &                   ,NM,AMAT,INDOP,UG1,UG2,UG3,PG1,PG3,DT1,DT2,IDYN
     &                   ,ICHK,ITO)

      USE MPI_PARAM
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION FLI(*),FLO(*),FCP(3,*),VELG(3,MGP,*),VELE(3,*),GRID(3,*)
     &         ,UG1(6,*),UG2(6,*),UG3(6,*),PG1(*),PG3(*),AMAT(33,*)
     &         ,GRAV(3),IELM(NM,*),KK(*),RR(*),RES(*),INDOP(*)
      DATA GRAV / 0.D0, 0.D0, -9.8D0 /

      NNOD = KK(8)
      NELM = KK(12)
      NNODI = KK(26)

      FLI(1:NNOD) = 0.
      FLO(1:NNOD) = 0.
      FCP(:,1:NNOD) = 0.

      DO I = 1, NELM

        ITYP = IELM(2,I)
        ND   = IELM(3,I)
        IMAT = IELM(4,I)

        IF( ITYP == 6 )
     &    CALL  GNPFLW_S(FLI,FLO,FCP,VELG(1,1,I),VELE(1,I),GRID,UG1,UG2
     &                  ,UG3,PG1,PG3,IELM(8,I),ND,AMAT(1,IMAT),GRAV,DT1
     &                  ,DT2,RR(7),IDYN,ITO)

      ENDDO

      DO I = 1, NNODI
        IFR = INDOP(I)
        IF( IFR > 0 ) RES(IFR) = FLO(I) - FLI(I)
      ENDDO

      IF( ICHK == 0 ) RETURN

      RNRM = 0.

      DFI = 0.
      DFO = 0.

      DO I = 1, NNODI
        IFR = INDOP(I)
        IF( IFR > 0 ) THEN
          RNRM = RNRM + RES(IFR) * RES(IFR)
        ELSEIF( IFR == -1 ) THEN
          DF = FLI(I) - FLO(I)
          IF( DF >= 0 ) THEN
            DFI = DFI + DF
          ELSE
            DFO = DFO - DF
          ENDIF
        ENDIF
      ENDDO

      IF( MYRANK == 0 ) THEN

        RNRM = DSQRT( RNRM )

        WRITE(ITO,'(/A//A)') 
     &    '   * FLOW BALANCE',
     &    '        RNRM       INFLOW      OUTFLOW    IN - OUT'

        WRITE(ITO,'(3X,1P4E12.4)') RNRM,DFI,DFO,DFI-DFO

      ELSE

        IF( MYRANK == 1 ) CALL M_MPI_SEND_I(4,1,0)  ! SEND IOP=4 TO GLB_COMM

        CALL M_MPI_REDUCE_D(RNRM,DUM,1,0)
        CALL M_MPI_REDUCE_D(DFI,DUM,1,0)
        CALL M_MPI_REDUCE_D(DFO,DUM,1,0)

      ENDIF

      END
