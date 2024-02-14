      SUBROUTINE SF_RD_OBST()

      USE SF_ARRAY

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      CHARACTER*80 CHAR
      CHARACTER*8 BLK /'        '/

      INTEGER, POINTER :: INDGR(:), IDP(:), IPSOL(:,:)
      REAL(8), POINTER :: RMAT(:,:)

!      IF( ICPL == 2 ) CALL SF_C_MPI_BARRIER()

      IF( MYRANK > 0 ) RETURN

      IF( ICPL == 2 ) CALL SF_C_MPI_RECV_I(IBDF,1,IROOTS)

      OPEN( MFILPR, FILE = 'data.bdf', IOSTAT = IERR )
      IF( IERR > 0 ) 
     &  CALL VF_A2ERR('SF_RD_OBST','CAN NOT OPEN (data.bdf).')
      IPRFIL = MFILPR

      NNOD = 0
      MNOD = 0
      NELM = 0
      NPSOL = 0
      MMAT = 0

      DO

        READ(IPRFIL,'(A80)',IOSTAT=IERR) CHAR

        IF( IERR < 0 ) THEN
          EXIT
        ELSEIF( IERR > 0 ) THEN
          CALL VF_A2ERR('SF_RD_OBST','I/O ERROR.')
        ENDIF

        IF( CHAR(1:7) == 'ENDDATA' ) THEN
          EXIT
        ELSEIF( CHAR(1:8) == 'GRID    ' ) THEN
          NNOD = NNOD + 1
          READ(CHAR,'(BN,8X,I8)') NO
          IF( NO > MNOD ) MNOD = NO
        ELSEIF( CHAR(1:8) == 'GRID*   ' ) THEN
          NNOD = NNOD + 1
          READ(CHAR,'(BN,8X,I16)') NO
          IF( NO > MNOD ) MNOD = NO
        ELSEIF( CHAR(1:8) == 'CTETRA  ' .OR. 
     &          CHAR(1:8) == 'CPENTA  ' .OR.
     &          CHAR(1:8) == 'CHEXA   ' ) THEN
          NELM = NELM + 1
        ELSEIF( CHAR(1:8) == 'PSOLID  ' ) THEN
          NPSOL = NPSOL + 1
        ELSEIF( CHAR(1:8) == 'MAT1    ' ) THEN
          READ(CHAR,'(BN,8X,I8)') NO
          IF( NO > MMAT ) MMAT = NO
        ENDIF

      ENDDO

      NNOD0 = NNOD
      NELM0 = NELM

      ALLOCATE( INDG(NNOD) )
      ALLOCATE( GRID(3,NNOD) )
      ALLOCATE( GRDL(NNOD) )
      ALLOCATE( IELM(24,NELM) )
      ALLOCATE( POR(NELM) )

      ALLOCATE( POS0(3,NNOD0) )
      ALLOCATE( POS10(3,NNOD0) )
      ALLOCATE( POS20(3,NNOD0) )

      ALLOCATE( DVEL10(3,NELM0) )
      ALLOCATE( DVEL20(3,NELM0) )

      REWIND(IPRFIL)

      ALLOCATE( IDP(NELM) )
      ALLOCATE( IPSOL(2,NPSOL) )
      ALLOCATE( RMAT(2,MMAT) )

      IG = 0
      IE = 0
      IP = 0

      IELM(:,:) = 0

      DO

        READ(IPRFIL,'(A80)',IOSTAT=IERR) CHAR

        IF( IERR < 0 ) THEN
          EXIT
        ELSEIF( IERR > 0 ) THEN
          CALL VF_A2ERR('SF_RD_OBST','I/O ERROR.')
        ENDIF

        IF( CHAR(1:7) == 'ENDDATA' ) THEN
          EXIT
        ELSEIF( CHAR(1:8) == 'GRID    ' ) THEN
          IG = IG + 1
          READ(CHAR,'(BN,8X,I8,8X,3F8.0)') INDG(IG), GRID(:,IG) 
        ELSEIF( CHAR(1:8) == 'GRID*   ' ) THEN
          IG = IG + 1
          READ(CHAR,'(BN,8X,I16,16X,2F16.0)') INDG(IG), GRID(1:2,IG)
          READ(IPRFIL,'(BN,8X,F16.0)') GRID(3,IG)
        ELSEIF( CHAR(1:8) == 'CTETRA  ' ) THEN
          IE = IE + 1
          READ(CHAR,'(BN,8X,8I8)') IELM(1,IE), IDP(IE), IELM(5:10,IE)
          READ(IPRFIL,'(A80)') CHAR
          IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
            READ(CHAR,'(BN,8X,4I8)') IELM(11:14,IE)
            IELM(4,IE) = 10
          ELSE
            BACKSPACE(IPRFIL)
            IELM(4,IE) = 4
          ENDIF
        ELSEIF( CHAR(1:8) == 'CPENTA  ' ) THEN
          IE = IE + 1
          READ(CHAR,'(BN,8X,8I8)') IELM(1,IE), IDP(IE), IELM(5:10,IE)
          READ(IPRFIL,'(A80)') CHAR
          IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
            READ(CHAR,'(BN,8X,8I8)') IELM(11:18,IE)
            READ(IPRFIL,'(BN,8X,I8)') IELM(19,IE)
            IELM(4,IE) = 15
          ELSE
            BACKSPACE(IPRFIL)
            IELM(4,IE) = 6
          ENDIF
        ELSEIF( CHAR(1:8) == 'CHEXA   ' ) THEN
          IE = IE + 1
          READ(CHAR,'(BN,8X,8I8)') IELM(1,IE), IDP(IE), IELM(5:10,IE)
          READ(IPRFIL,'(BN,8X,8I8)') IELM(11:18,IE)
          READ(IPRFIL,'(A80)') CHAR
          IF( CHAR(1:8) == BLK .OR. CHAR(1:1) == '+' ) THEN
            READ(CHAR,'(BN,8X,6I8)') IELM(19:24,IE)
            IELM(4,IE) = 20
          ELSE
            BACKSPACE(IPRFIL)
            IELM(4,IE) = 8
          ENDIF
        ELSEIF( CHAR(1:8) == 'PSOLID  ' ) THEN
          IP = IP + 1
          READ(CHAR,'(BN,8X,2I8)') IPSOL(:,IP)
        ELSEIF( CHAR(1:8) == 'MAT1    ' ) THEN
          READ(CHAR,'(BN,8X,I8,8X,F8.0,24X,F8.0)') IMAT,RMAT(:,IMAT)
        ELSEIF( CHAR(1:6) == 'CADMAS' ) THEN
          IF( CHAR(8:11) == 'PART' ) THEN
            IF( NPROCS > 1 ) IPART = 1
          ELSEIF( CHAR(8:14) == 'PLOWER2' ) THEN
            READ(CHAR(16:),*) PLOWER2
          ELSEIF( CHAR(8:11) == 'GMIN' ) THEN
            READ(CHAR(13:),*) GMIN
          ELSEIF( CHAR(8:11) == 'FMIN' ) THEN
            READ(CHAR(13:),*) FMIN
          ENDIF
        ENDIF

      ENDDO

      ALLOCATE( INDGR(MNOD) )

      DO I = 1, NNOD
        INDGR( INDG(I) ) = I
      ENDDO

      IGEO = 0

      POR(:) = 0.

      DO I = 1, NELM

        N = IELM(4,I)
        IELM(5:4+N,I) = INDGR( IELM(5:4+N,I) )

        DO J = 1, NPSOL
          IF( IDP(I) == IPSOL(1,J) ) THEN
            IMAT = IPSOL(2,J)
            IELM(3,I) = IDINT( RMAT(1,IMAT) )
            IF( IELM(3,I) /= 1 .AND. IELM(3,I) /= 2 ) IELM(3,I) = 0
            POR(I) = RMAT(2,IMAT)
            IF( IMAT >= 100 ) THEN
              IELM(2,I) = 1
              IGEO = 1
            ELSEIF( RMAT(2,IMAT) > 0. ) THEN
              IELM(2,I) = 2
            ENDIF
            EXIT
          ENDIF
        ENDDO

      ENDDO

      DEALLOCATE( IDP )
      DEALLOCATE( IPSOL )
      DEALLOCATE( RMAT )
      DEALLOCATE( INDGR )

      GRDL(:) = 1.D20

      DO I = 1, NELM
        CALL SF_GRDL(GRDL,GRID,IELM(5,I),IELM(4,I))
      ENDDO

      POS0 = GRID

      END