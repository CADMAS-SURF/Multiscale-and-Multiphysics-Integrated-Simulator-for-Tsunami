      SUBROUTINE MKINDEX_S(KK,I_BCT,ISEEP,FLNAME,NLEN,IFMTX1,IFMTX2,ITO)
C
      USE M_VAL
      USE M_MUMPS
      USE MPI_PARAM
C
      IMPLICIT REAL*8(A-H,O-Z)
C
      CHARACTER FLNAME*256
C
      INTEGER*8 MUSE, MAVBL, MAVBL0, MREQ, MREQ0, MSYM, MNUMI
C
      DIMENSION KK(*),MSG(0:1)
C
      INTEGER, POINTER :: IDCGWK(:), NJ(:), IPREV(:), NEXT(:), LAST(:)
C
      DATA MSG / 2*0 /
C
      MUSE = KK(3) * 1.D6 / 4.D0
      NNOD = KK(8)
      NNODI = KK(26)
      NELM = KK(12)
      ISOLV = KK(21)
C
      SELECT CASE( ISEEP )
      CASE( 0 )
C
        NCGMAX = KK(24)
        NNODC = KK(28)
        NELMC = KK(29)
        NNODX = KK(31)
        NELMX = KK(32)
        NIGSF = KK(94)
        NIGSFC = KK(108)
C
C       ----- INDOF0 --> INDOF -----
C
        INDOF(:,:) = INDOF0(:,:)
C
C       ----- INDOF, INDMPC, (ALLOCATE) MPCF, RMPC -----
C       ----- NCRMAX, NRAMAX, NESTF -----
C
        IF( I_BCT > 0 ) THEN
          CALL MPCSET(KK,ITO)
        ELSE
          CALL NCRSET2(KK,IELM,KK(37))
        ENDIF
C
C       ----- NEQ, INDOF -----
C
        NEQI = 0
C
        DO I = 1, NNODI
          DO J = 1, 6
            IF( INDOF(J,I) == 0 ) THEN
              NEQI = NEQI + 1
              INDOF(J,I) = NEQI
            ELSEIF( INDOF(J,I) == 1 ) THEN
              INDOF(J,I) = 0
            ENDIF
          ENDDO
        ENDDO
C
        KK(27) = NEQI
C
        NEQ = NEQI
C
        DO I = NNODI + 1, NNOD + NNODC + NIGSF + NIGSFC + NNODX
          IF( I > NNOD .AND. I <= NNOD + NNODC + NIGSF + NIGSFC ) CYCLE
          DO J = 1, 6
            IF( INDOF(J,I) == 0 ) THEN
              NEQ = NEQ + 1
              INDOF(J,I) = NEQ
            ELSEIF( INDOF(J,I) == 1 ) THEN
              INDOF(J,I) = 0
            ENDIF
          ENDDO
        ENDDO
C
        KK(19) = NEQ
C
      CASE( 1 )
C
        NELMC = 0
        NELMX = 0
        NCGMAX = KK(25)
C
C       ----- INDOP0 --> INDOP -----
C
        INDOP(:) = INDOP0(:)
        DO I = 1, NNOD
          IF( INDOP(I) == 0 .AND. IPND(I) == 1 ) INDOP(I) = -1
        ENDDO
C
C       ----- NCRMAX -----
C
        KK(34) = 20
C
C       ----- NEQ, INDOP -----
C
        NEQI = 0
C
        DO I = 1, NNODI
          IF( INDOP(I) == 0 ) THEN
            NEQI = NEQI + 1
            INDOP(I) = NEQI
          ELSEIF( INDOP(I) == 1 ) THEN
            INDOP(I) = 0
          ENDIF
        ENDDO
C
        KK(27) = NEQI
C
        NEQ = NEQI
C
        DO I = NNODI + 1, NNOD
          IF( INDOP(I) == 0 ) THEN
            NEQ = NEQ + 1
            INDOP(I) = NEQ
          ELSEIF( INDOP(I) == 1 ) THEN
            INDOP(I) = 0
          ENDIF
        ENDDO
C
        KK(19) = NEQ
C
      END SELECT
C
C     ----- (ALLOCATE) RHV, X -----
C
      ALLOCATE( RHV(NEQ) )
      ALLOCATE( X(NEQ) )
C
C     ----- NCGSPC, (ALLOCATE) IDSK, IDCG -----
C
      MAVBL0 = MAVBL(MUSE)
C
      MREQ = NEQ * ( NCGMAX + 5 ) * 2.5
      IF( MREQ > MAVBL(MUSE) ) THEN
        WRITE(ITO,100) (MREQ - MAVBL(MUSE))*4.E-6
  100   FORMAT(F8.2,'(MB) MORE MEMORY IS NECESSARY FOR NEXT PROCESS'
     &        ,' OF MATRIX SOLVER.')
        CALL ERRSTP(20,ITO)
      ENDIF
C
      MIDX = MREQ * 0.8 / ( NCGMAX + 5 )
C
      ALLOCATE( IDCGWK( NCGMAX*MIDX ) )
      ALLOCATE( NJ(MIDX) )
      ALLOCATE( IPREV(MIDX) )
      ALLOCATE( NEXT(MIDX) )
      ALLOCATE( LAST(NEQ) )
C
      CALL CGINDX2_S(NCGSPC,IDCGWK,NJ,IPREV,NEXT,LAST,IELM,KK(37),NELM
     &              ,NELMC,NELMX,NEQ,KK(34),NCGMAX,MIDX,INDOF,INDMPC
     &              ,MPCF,I_BCT,INDOP,ISOLV,ISEEP,ITO)
C
      KK(20) = NCGSPC
C
      ALLOCATE( IDSK(NEQ+1) )
      ALLOCATE( IDCG(NCGSPC) )
C
      CALL CGINDX3(IDSK,IDCG,IDCGWK,NCGMAX,NJ,NEXT,NEQ,ISOLV)
C
      DEALLOCATE( IDCGWK )
      DEALLOCATE( NJ )
      DEALLOCATE( IPREV )
      DEALLOCATE( NEXT )
      DEALLOCATE( LAST )
C
      SELECT CASE( ISOLV )
      CASE( 1 )
C
C       ----- (ALLOCATE) CGWK, STF, LOW -----
C
        MREQ = ( NEQ*6 + NCGSPC*2 )*2
        IF( MREQ > MAVBL(MUSE) ) THEN
          WRITE(ITO,100) (MREQ - MAVBL(MUSE))*4.E-6
          CALL ERRSTP(20,ITO)
        ENDIF
C
        ALLOCATE( CGWK(NEQ,6) )
        ALLOCATE( STF(NCGSPC) )
        ALLOCATE( LOW(NCGSPC) )
C
        IF( MSG(ISEEP) == 0 ) THEN
          MREQ0  = MREQ  + ( NEQ + 1 ) + NCGSPC
          CALL PRNCG(NEQ,MUSE,MAVBL0,MREQ0,ITO)
          MSG(ISEEP) = 1
        ENDIF
C
      CASE( 2 )
C
C       ----- (ALLOCATE) IASEWK, ASEWK, STF -----
C
        MREQ = NEQ*3 + NCGSPC*2
        IF( MREQ > MAVBL(MUSE) ) THEN
          WRITE(ITO,100) (MREQ - MAVBL(MUSE))*4.E-6
          CALL ERRSTP(20,ITO)
        ENDIF
C
        ALLOCATE( IASEWK(NEQ) )
        ALLOCATE( ASEWK(NEQ) )
        ALLOCATE( STF(NCGSPC) )
C
C       ----- LSYM, LNUMI, LNUMO, (ALLOCATE) SYM, NUM -----
C
C        CALL ASEINDX(MAVBL(MUSE),NEQ,IDSK,IDCG,IASEWK,LSYM,LNUMI
C     &              ,LNUMO,1,ITO)
C
        IF( MSG(ISEEP) == 0 ) THEN
          MREQ0 = MREQ  + (NEQ + 1) + NCGSPC + LSYM
          CALL PRNASE(NEQ,MUSE,MREQ0,LNUMI,LNUMO,MAVBL0,1.D0,ITO)
          MSG(ISEEP) = 1
        ENDIF
C
        NRECL=1024*2
        OPEN(IFMTX1,FILE=FLNAME(1:NLEN)//'.wk1',ACCESS='DIRECT'
     &      ,FORM='UNFORMATTED',RECL=NRECL)
        OPEN(IFMTX2,FILE=FLNAME(1:NLEN)//'.wk2',ACCESS='DIRECT'
     &      ,FORM='UNFORMATTED',RECL=NRECL)
C
      CASE( 3 )
C
C       ----- (ALLOCATE) STF -----
C
        MREQ = NCGSPC*2
        IF( MREQ > MAVBL(MUSE) ) THEN
          WRITE(ITO,100) (MREQ - MAVBL(MUSE))*4.D-6
          CALL ERRSTP(20,ITO)
        ENDIF
C
        ALLOCATE( STF(NCGSPC) )
C
C       ----- MSYM, MNUMI -----
C
        PT(:) = 0
        IPARM(:) = 0
C
!        CALL PARDISO(PT,1,1,-2,11,NEQ,DDUM,IDSK,IDCG,IDUM,1,IPARM
!     &              ,0,DDUM,DDUM,IERR)
C
        IF( IERR == -2 ) THEN
          WRITE(ITO,'(A)') 'NOT ENOUGH MEMORY FOR REORDERING.'
          CALL ERRSTP(20,ITO)
        ENDIF
C
        MSYM  = IPARM(16) * 1.D3 / 4.D0
        MNUMI = IPARM(17) * 1.D3 / 4.D0
C
        IF( MSYM + MNUMI > MAVBL(MUSE) ) THEN
          WRITE(ITO,100) (MSYM + MNUMI - MAVBL(MUSE))*4.D-6
          CALL ERRSTP(20,ITO)
        ENDIF
C
        IF( MSG(ISEEP) == 0 ) THEN
          MREQ0 = MREQ + (NEQ + 1) + NCGSPC + MSYM
     &            + MNUMI
          CALL PRNPAR(NEQ,MUSE,MAVBL0,MREQ0,ITO)
          MSG(ISEEP) = 1
        ENDIF
C
      CASE( 4 )
C
        MUMPS_PAR%COMM = MYWORLD
        MUMPS_PAR%SYM = 1
        MUMPS_PAR%PAR = 1
C
        MUMPS_PAR%JOB = -1
C
        CALL M_MPI_BCAST_I(MUMPS_PAR%JOB,1)
C
        CALL DMUMPS(MUMPS_PAR)
C
        MREQ = NCGSPC*3
        IF( MREQ > MAVBL(MUSE) ) THEN
          WRITE(ITO,100) (MREQ - MAVBL(MUSE))*4.E-6
          CALL ERRSTP(20,ITO)
        ENDIF
C
        MUMPS_PAR%N = NEQ
        MUMPS_PAR%NZ = NCGSPC
C
        ALLOCATE( MUMPS_PAR%IRN(MUMPS_PAR%NZ) )
C
        DO I = 1, MUMPS_PAR%N
          MUMPS_PAR%IRN(IDSK(I):IDSK(I+1)-1) = I
        ENDDO
C
        MUMPS_PAR%JCN => IDCG
C
        ALLOCATE( STF(NCGSPC) )
        MUMPS_PAR%A => STF
C
        MUMPS_PAR%RHS => RHV
C
        MUMPS_PAR%ICNTL(1) = ITO
        MUMPS_PAR%ICNTL(2:3) = 0
        MUMPS_PAR%ICNTL(4) = 1
        MUMPS_PAR%ICNTL(6) = 0
        MUMPS_PAR%ICNTL(14) = 100
C
        MUMPS_PAR%JOB = 1
C
        CALL M_MPI_BCAST_I(MUMPS_PAR%JOB,1)
C
        CALL DMUMPS(MUMPS_PAR)
C
        MNUMI = MUMPS_PAR%INFOG(17) * 1.D6 / 4.D0
C
        IF( MNUMI > MAVBL(MUSE) ) THEN
          WRITE(ITO,100) (MNUMI - MAVBL(MUSE))*4.D-6
          CALL ERRSTP(20,ITO)
        ENDIF
C
        IF( MSG(ISEEP) == 0 ) THEN
          MREQ0 = MREQ + (NEQ + 1) + NCGSPC + MNUMI
          CALL PRNMUMPS(NEQ,MUSE,MAVBL0,MREQ0,ITO)
          MSG(ISEEP) = 1
        ENDIF
C
      END SELECT
C
      END
