      SUBROUTINE SF_WT_RESTART(DT)

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

      DIMENSION NNOWR(100),TNOWR(100)

      DATA INIT / 0 /
      DATA NR / 0 /

      IF( NNOW == 0 ) RETURN

      IF( INIT == 0 ) THEN

        INIT = 1

        IF( IRSTYP > 0 ) THEN

          IF( NPROCS == 1 ) THEN
            OPEN(MFILRS,FILE=TRIM(MGNAME(MGRANK+1))//'.rsl'
     &          ,FORM='UNFORMATTED',IOSTAT=IERR)
            IF( IERR > 0 )
     &        CALL VF_A2ERR('SF_WT_RESTART','CAN NOT OPEN('
     &                      //TRIM(MGNAME(MGRANK+1))//'.rsl).')
          ELSE
            WRITE(IP,'(I5.5)') MYRANK
            OPEN(MFILRS,FILE=TRIM(MGNAME(MGRANK+1))//'.rsl'//IP
     &          ,FORM='UNFORMATTED',IOSTAT=IERR)
            IF( IERR > 0 )
     &        CALL VF_A2ERR('SF_WT_RESTART','CAN NOT OPEN('
     &                      //TRIM(MGNAME(MGRANK+1))//'.rsl'//IP//').')
          ENDIF

          IRSFIL = MFILRS

        ENDIF

      ENDIF

      IF( IOR == 1 ) THEN

        WRITE(IRSFIL) NUMS,NNOW,TNOW,DTNOW

        WRITE(IRSFIL) UU,VV,WW,PP,FF,FX,FY,FZ,ANU,GGV,GGX,GGY,GGZ,GLV
     &               ,GLX,GLY,GLZ,TBUB,DROPTX,DROPTY,DROPTZ,DROPUU
     &               ,DROPVV,DROPWW,NF,INDX,INDY,INDZ,INDC,INDS

        IF( LEQK /= 0 ) WRITE(IRSFIL) ANUT,AK,AE

        IF( ICPL == 1 ) THEN

          WRITE(IRSFIL) BCU,BCV,BCW,BCF

          IF( LEQK /= 0 ) WRITE(IRSFIL) BCK,BCE

        ENDIF

        IF( ICPL == 2 ) THEN

          WRITE(IRSFIL) TSTR1,TSTR2

          IF( IPART == 1 ) THEN
            WRITE(IRSFIL) NELM,NNOD,NPFC
            IF( NELM > 0 ) WRITE(IRSFIL) IELM,IENO
            IF( NNOD > 0 ) WRITE(IRSFIL) INDG,GRID,IGFC,GRDL,IGNO
            IF( NPFC > 0 ) WRITE(IRSFIL) IPFACE,IPFNO
          ENDIF

          IF( MYRANK == 0 ) THEN
            IF( ICON == 1 ) WRITE(IRSFIL) AFC
            WRITE(IRSFIL) IPGRID0,POS10,POS20,DVEL10,DVEL20
          ENDIF

          IF( NNOD > 0 ) WRITE(IRSFIL) IPGRID,POS

          DO I = 1, NELM
            WRITE(IRSFIL) SPC(I)%IFIX, SPC(I)%N
            IF( SPC(I)%N > 0 ) WRITE(IRSFIL) SPC(I)%ID,SPC(I)%P
          ENDDO
       
        ENDIF

        IF( ISTM == 1 )
     &    WRITE(IRSFIL) GGW,GGR,FLFU,FLFV,LNDC,LNDC0,DZ,SUMZ,IRGRID

        NR = NR + 1

        NNOWR(NR) = NNOW
        TNOWR(NR) = TNOW

      ENDIF

      IF( ISTM == 1 .AND. NNOW > IRETYP ) THEN
        IF( MYRANK == 0 ) CALL SF_STM_C_MPI_SEND_I(NNOW*IOR,1,IROOTSTM)
      ENDIF

      IF( ICPL == 2 .AND. TNOW + DT > TSTR2 ) THEN
        IF( MYRANK == 0 ) THEN
          CALL SF_C_MPI_SEND_I(NR,1,IROOTS)
          IF( NR > 0 ) THEN
            CALL SF_C_MPI_SEND_I(NNOWR,NR,IROOTS)
            CALL SF_C_MPI_SEND_D(TNOWR,NR,IROOTS)
          ENDIF
        ENDIF
        NR = 0
      ENDIF

      END
