      SUBROUTINE SF_RD_RESTART(DT)

      USE VF_A2ARRAY
      USE SF_ARRAY

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_ATIMER.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      CHARACTER*5 IP

      IF( NPROCS == 1 ) THEN
        OPEN(MFILRE,FILE=TRIM(MGNAME(MGRANK+1))//'.res'
     &      ,FORM='UNFORMATTED',IOSTAT=IERR)
        IF( IERR > 0 )
     &    CALL VF_A2ERR('SF_WT_RESTART','CAN NOT OPEN('
     &                  //TRIM(MGNAME(MGRANK+1))//'.res).')
      ELSE
        WRITE(IP,'(I5.5)') MYRANK
        OPEN(MFILRE,FILE=TRIM(MGNAME(MGRANK+1))//'.res'//IP
     &      ,FORM='UNFORMATTED',IOSTAT=IERR)
        IF( IERR > 0 )
     &    CALL VF_A2ERR('SF_WT_RESTART','CAN NOT OPEN('
     &                  //TRIM(MGNAME(MGRANK+1))//'.res'//IP//').')
      ENDIF

      IREFIL = MFILRE

      DO

        READ(IREFIL) NUMS,NNOW,TNOW,DTNOW

        READ(IREFIL) UU,VV,WW,PP,FF,FX,FY,FZ,ANU,GGV,GGX,GGY,GGZ,GLV
     &              ,GLX,GLY,GLZ,TBUB,DROPTX,DROPTY,DROPTZ,DROPUU
     &              ,DROPVV,DROPWW,NF,INDX,INDY,INDZ,INDC,INDS

        IF( LEQK /= 0 ) READ(IREFIL) ANUT,AK,AE

        IF( ICPL == 1 ) THEN

          READ(IREFIL) BCU,BCV,BCW,BCF

          IF( LEQK /= 0 ) READ(IREFIL) BCK,BCE

        ENDIF

        IF( ICPL == 2 ) THEN

          READ(IREFIL) TSTR1,TSTR2

          IF( IPART == 1 ) THEN
            READ(IREFIL) NELM,NNOD,NPFC
            CALL SF_REALLOC(NELM,NNOD,NPFC)
            IF( NELM > 0 ) READ(IREFIL) IELM,IENO
            IF( NNOD > 0 ) READ(IREFIL) INDG,GRID,IGFC,GRDL,IGNO
            IF( NPFC > 0 ) READ(IREFIL) IPFACE,IPFNO
          ENDIF

          IF( MYRANK == 0 ) THEN
            IF( ICON == 1 ) READ(IREFIL) AFC
            READ(IREFIL) IPGRID0,POS10,POS20,DVEL10,DVEL20
          ENDIF

          IF( NNOD > 0 ) READ(IREFIL) IPGRID,POS

          DO I = 1, NELM
            READ(IREFIL) SPC(I)%IFIX, SPC(I)%N
            IF( ALLOCATED( SPC(I)%ID ) ) DEALLOCATE( SPC(I)%ID )
            IF( ALLOCATED( SPC(I)%P ) ) DEALLOCATE( SPC(I)%P )
            IF( SPC(I)%N > 0 ) THEN
              ALLOCATE( SPC(I)%ID(3,SPC(I)%N) )
              ALLOCATE( SPC(I)%P(6,SPC(I)%N) )
              READ(IREFIL) SPC(I)%ID,SPC(I)%P
            ENDIF
          ENDDO
       
        ENDIF

        IF( ISTM == 1 )
     &    READ(IREFIL) GGW,GGR,FLFU,FLFV,LNDC,LNDC0,DZ,SUMZ,IRGRID

        IF( NNOW == IRETYP ) EXIT

      ENDDO

      DT = DTNOW

      IF( ICPL == 2 ) THEN

        IF( IPART == 0 ) THEN

          IF( MYRANK == 0 ) THEN
            POS1 = POS10
            POS2 = POS20
            DVEL1 = DVEL10
            DVEL2 = DVEL20
          ENDIF

          CALL VF_P1BCSD(POS1,NNOD*3,0)
          CALL VF_P1BCSD(POS2,NNOD*3,0)
          CALL VF_P1BCSD(DVEL1,NELM*3,0)
          CALL VF_P1BCSD(DVEL2,NELM*3,0)

        ELSEIF( IPART == 1 ) THEN

          CALL SF_BCAST_D(POS1,IGNO,NNOD,POS10,NNOD0,3)
          CALL SF_BCAST_D(POS2,IGNO,NNOD,POS20,NNOD0,3)
          CALL SF_BCAST_D(DVEL1,IENO,NELM,DVEL10,NELM0,3)
          CALL SF_BCAST_D(DVEL2,IENO,NELM,DVEL20,NELM0,3)

        ENDIF

      ENDIF

      END
