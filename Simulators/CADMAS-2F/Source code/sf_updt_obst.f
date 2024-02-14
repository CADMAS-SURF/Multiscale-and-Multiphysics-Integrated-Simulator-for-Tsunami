      SUBROUTINE SF_UPDT_OBST()

      USE VF_A2ARRAY
      USE SF_ARRAY

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION RNG(2,3),IPE(8),NE(8),NG(8),NPF(8)

      INTEGER, POINTER :: ISKIP(:),IBING(:),IEBING(:),IGBING(:)
      INTEGER, POINTER :: IPFBING(:)
      INTEGER, POINTER :: IELMS(:,:),IENOS(:),INDGS(:),IGFCS(:)
      INTEGER, POINTER :: IGNOS(:),IPFACES(:,:),IPFNOS(:)
      REAL(8), POINTER :: PORS(:),GRIDS(:,:),GRDLS(:),POSS(:,:)
      INTEGER, POINTER :: IELMW(:,:),IENOW(:),INDGW(:),IGFCW(:)
      INTEGER, POINTER :: IGNOW(:),IPFACEW(:,:),IPFNOW(:)
      REAL(8), POINTER :: PORW(:),GRIDW(:,:),GRDLW(:),POSW(:,:)

      TYPE(SPACE), POINTER :: SPCS(:),SPCW(:)

!     自身の領域のデータを *S にセット
!     -> 双方向通信により自身を含む隣接領域のデータを *W にセット
!     -> マージしてデータを完成

!     --- NPE, IPE ---  隣接する領域の数，RANK

      IPE(:) = 0

      IP = 0

      DO I = -1, 1
        DO J = -1, 1
          IF( I == 0 .AND. J == 0 ) CYCLE
          II = MYRI + I
          JJ = MYRJ + J
          IF( II > 0 .AND. II <= NUMNPI .AND.
     &        JJ > 0 .AND. JJ <= NUMNPJ ) THEN
            IP = IP + 1
            IRANK = ( JJ - 1 ) * NUMNPI + II - 1
            IPE(IP) = IRANK
          ENDIF
        ENDDO
      ENDDO

      NPE = IP

!     --- NELMS, IELMS, PORS, IENOS, SPCS, NNODS, INDGS, GRIDS, IGFCS, GRDLS, IGNOS, POSS ---

      ALLOCATE( ISKIP(NELM) )
      ALLOCATE( IBING(NNOD) )

      RNG(1,1) = XX(1,1)
      RNG(2,1) = XX(1,NUMI)
      RNG(1,2) = YY(1,1)
      RNG(2,2) = YY(1,NUMJ)

      IE = 0

      DO I = 1, NELM
        CALL SF_ELEM_SKIP(ISKIP(I),RNG(1,1),RNG(2,1),RNG(1,2)
     &                   ,RNG(2,2),IELM(4,I),IELM(5,I),POS)
        IF( ISKIP(I) == 0 ) IE = IE + 1
      ENDDO

      NELMS = IE

      ALLOCATE( IELMS(24,NELMS) )
      ALLOCATE( PORS(NELMS) )
      ALLOCATE( IENOS(NELMS) )
      ALLOCATE( SPCS(NELMS) )

      IBING(:) = 0

      IE = 0

      DO I = 1, NELM
        IF( ISKIP(I) == 1 ) CYCLE
        IE = IE + 1
        IELMS(:,IE) = IELM(:,I)
        PORS(IE) = POR(I)
        IENOS(IE) = IENO(I)
        SPCS(IE)%IFIX = SPC(I)%IFIX
        N = SPC(I)%N
        SPCS(IE)%N = N
        IF( N > 0 ) THEN
          ALLOCATE( SPCS(IE)%ID(3,N) )
          ALLOCATE( SPCS(IE)%P(6,N) )
          SPCS(IE)%ID(:,:) = SPC(I)%ID(:,:)
          SPCS(IE)%P(:,:) = SPC(I)%P(:,:)
        ENDIF
        N = IELM(4,I)
        IBING( IELM(5:4+N,I) ) = 1
      ENDDO

      IG = 0

      DO I = 1, NNOD
        IF( IBING(I) == 1 ) IG = IG + 1
      ENDDO

      NNODS = IG

      ALLOCATE( INDGS(NNODS) )
      ALLOCATE( GRIDS(3,NNODS) )
      ALLOCATE( IGFCS(NNODS) )
      ALLOCATE( GRDLS(NNODS) )
      ALLOCATE( IGNOS(NNODS) )
      ALLOCATE( POSS(3,NNODS) )

      IG = 0

      DO I = 1, NNOD
        IF( IBING(I) == 0 ) CYCLE
        IG = IG + 1
        INDGS(IG) = INDG(I)
        GRIDS(:,IG) = GRID(:,I)
        IGFCS(IG) = IGFC(I)
        GRDLS(IG) = GRDL(I)
        IGNOS(IG) = IGNO(I)
        POSS(:,IG) = POS(:,I)
      ENDDO

      DO I = 1, NELMS
        N = IELMS(4,I)
        IELMS(5:4+N,I) = IGNO( IELMS(5:4+N,I) )
      ENDDO

      DEALLOCATE( ISKIP )
      DEALLOCATE( IBING )

!     --- NPFCS, IPFACES, IPFNOS ---

      ALLOCATE( ISKIP(NPFC) )

      RNG(1,1) = XX(1,MYIS)
      RNG(2,1) = XX(1,MYIE+1)
      RNG(1,2) = YY(1,MYJS)
      RNG(2,2) = YY(1,MYJE+1)
      RNG(1,3) = ZZ(1,2)
      RNG(2,3) = ZZ(1,NUMK)

      IPFC = 0

      DO I = 1, NPFC
        CALL SF_PFC_SKIP(ISKIP(I),RNG,IPFACE(4,I),IPFACE(5,I),POS)
        IF( ISKIP(I) == 0 ) IPFC = IPFC + 1
      ENDDO

      NPFCS = IPFC

      ALLOCATE( IPFACES(12,NPFCS) )
      ALLOCATE( IPFNOS(NPFCS) )

      IPFC = 0

      DO I = 1, NPFC
        IF( ISKIP(I) == 1 ) CYCLE
        IPFC = IPFC + 1
        IPFACES(:,IPFC) = IPFACE(:,I)
        IPFNOS(IPFC) = IPFNO(I)
      ENDDO

      DO I = 1, NPFCS
        N = IPFACES(4,I)
        IPFACES(5:4+N,I) = IGNO( IPFACES(5:4+N,I) )
      ENDDO

      DEALLOCATE( ISKIP )

!     --- NE, NELMW ---

      NE(:) = 0

      CALL SF_COMM_I_1(NE,NELMS,IPE,NPE)

      NELMW = NELMS + SUM(NE)

!     --- IELMW, PORW, IENOW ---

      ALLOCATE( IELMW(24,NELMW) )
      ALLOCATE( PORW(NELMW) )
      ALLOCATE( IENOW(NELMW) )

      CALL SF_COMM_I_2(IELMW,NE,IELMS,24,NELMS,IPE,NPE)

      CALL SF_COMM_D_2(PORW,NE,PORS,1,NELMS,IPE,NPE)

      CALL SF_COMM_I_2(IENOW,NE,IENOS,1,NELMS,IPE,NPE)

      DEALLOCATE( IELMS )
      DEALLOCATE( PORS )
      DEALLOCATE( IENOS )

!     --- SPCW ---

      ALLOCATE( SPCW(NELMW) )

      DO I = 1, NELMS
        SPCW(I)%IFIX = SPCS(I)%IFIX
        N = SPCS(I)%N
        SPCW(I)%N = N
        IF( N == 0 ) CYCLE
        ALLOCATE( SPCW(I)%ID(3,N) )
        ALLOCATE( SPCW(I)%P(6,N) )
        SPCW(I)%ID(:,:) = SPCS(I)%ID(:,:)
        SPCW(I)%P(:,:) = SPCS(I)%P(:,:)
      ENDDO

      SPCW(NELMS+1:NELMW)%IFIX = 0
      SPCW(NELMS+1:NELMW)%N = 0

      DEALLOCATE( SPCS )

!     --- IEBING, NELM ---

      IF( NELMW > 0 ) THEN
        MIN_E = MINVAL(IENOW)
        MAX_E = MAXVAL(IENOW)
      ELSE
        MIN_E = 0
        MAX_E = 0
      ENDIF

      ALLOCATE( IEBING(MIN_E:MAX_E) )

      IEBING(:) = 0

      IE = 0

      DO I = 1, NELMW
        IEN = IENOW(I)
        IF( IEBING(IEN) == 0 ) THEN
          IE = IE + 1
          IEBING(IEN) = I
        ENDIF
      ENDDO

      NELM = IE

!     --- IELM, POR, IENO, SPC ---

      DEALLOCATE( IELM )
      DEALLOCATE( POR )
      DEALLOCATE( IENO )
      DEALLOCATE( SPC )

      ALLOCATE( IELM(24,NELM) )
      ALLOCATE( POR(NELM) )
      ALLOCATE( IENO(NELM) )
      ALLOCATE( SPC(NELM) )

      IE = 0

      DO IEN = MIN_E, MAX_E
        IF( IEBING(IEN) == 0 ) CYCLE
        IE = IE + 1
        I = IEBING(IEN)
        IELM(:,IE) = IELMW(:,I)
        POR(IE) = PORW(I)
        IENO(IE) = IENOW(I)
        SPC(IE)%IFIX = SPCW(I)%IFIX
        N = SPCW(I)%N
        SPC(IE)%N = N
        IF( N > 0 ) THEN
          ALLOCATE( SPC(IE)%ID(3,N) )
          ALLOCATE( SPC(IE)%P(6,N) )
          SPC(IE)%ID(:,:) = SPCW(I)%ID(:,:)
          SPC(IE)%P(:,:) = SPCW(I)%P(:,:)
        ENDIF
      ENDDO

      DEALLOCATE( IELMW )
      DEALLOCATE( PORW )
      DEALLOCATE( IENOW )
      DEALLOCATE( SPCW )
      DEALLOCATE( IEBING )

!     --- NPF, NPFCW ---

      NPF(:) = 0

      CALL SF_COMM_I_1(NPF,NPFCS,IPE,NPE)

      NPFCW = NPFCS + SUM(NPF)

!     --- IPFACEW, IPFNOW ---

      ALLOCATE( IPFACEW(12,NPFCW) )
      ALLOCATE( IPFNOW(NPFCW) )

      CALL SF_COMM_I_2(IPFACEW,NPF,IPFACES,12,NPFCS,IPE,NPE)

      CALL SF_COMM_I_2(IPFNOW,NPF,IPFNOS,1,NPFCS,IPE,NPE)

      DEALLOCATE( IPFACES )
      DEALLOCATE( IPFNOS )

!     --- IPFBING, NPFC ---

      IF( NPFCW > 0 ) THEN
        MIN_PF = MINVAL(IPFNOW)
        MAX_PF = MAXVAL(IPFNOW)
      ELSE
        MIN_PF = 0
        MAX_PF = 0
      ENDIF

      ALLOCATE( IPFBING(MIN_PF:MAX_PF) )

      IPFBING(:) = 0

      IPFC = 0

      DO I = 1, NPFCW
        IPFN = IPFNOW(I)
        IF( IPFBING(IPFN) == 0 ) THEN
          IPFC = IPFC + 1
          IPFBING(IPFN) = I
        ENDIF
      ENDDO

      NPFC = IPFC

!     --- IPFACE, IPFNO ---

      DEALLOCATE( IPFACE )
      DEALLOCATE( IPFNO )

      ALLOCATE( IPFACE(12,NPFC) )
      ALLOCATE( IPFNO(NPFC) )

      IPFC = 0

      DO IPFN = MIN_PF, MAX_PF
        IF( IPFBING(IPFN) == 0 ) CYCLE
        IPFC = IPFC + 1
        I = IPFBING(IPFN)
        IPFACE(:,IPFC) = IPFACEW(:,I)
        IPFNO(IPFC) = IPFNOW(I)
      ENDDO

      DEALLOCATE( IPFACEW )
      DEALLOCATE( IPFNOW )
      DEALLOCATE( IPFBING )

!     --- NG, NNODW ---

      NG(:) = 0

      CALL SF_COMM_I_1(NG,NNODS,IPE,NPE)

      NNODW = NNODS + SUM(NG)

!     --- INDGW, GRIDW, IGFCW, GRDLW, IGNOW, POSW ---

      ALLOCATE( INDGW(NNODW) )
      ALLOCATE( GRIDW(3,NNODW) )
      ALLOCATE( IGFCW(NNODW) )
      ALLOCATE( GRDLW(NNODW) )
      ALLOCATE( IGNOW(NNODW) )
      ALLOCATE( POSW(3,NNODW) )

      CALL SF_COMM_I_2(INDGW,NG,INDGS,1,NNODS,IPE,NPE)
      CALL SF_COMM_D_2(GRIDW,NG,GRIDS,3,NNODS,IPE,NPE)
      CALL SF_COMM_I_2(IGFCW,NG,IGFCS,1,NNODS,IPE,NPE)
      CALL SF_COMM_D_2(GRDLW,NG,GRDLS,1,NNODS,IPE,NPE)
      CALL SF_COMM_I_2(IGNOW,NG,IGNOS,1,NNODS,IPE,NPE)
      CALL SF_COMM_D_2(POSW,NG,POSS,3,NNODS,IPE,NPE)

      DEALLOCATE( INDGS )
      DEALLOCATE( GRIDS )
      DEALLOCATE( IGFCS )
      DEALLOCATE( GRDLS )
      DEALLOCATE( IGNOS )
      DEALLOCATE( POSS )

!     --- IGBING, NNOD ---

      IF( NNODW > 0 ) THEN
        MIN_G = MINVAL(IGNOW)
        MAX_G = MAXVAL(IGNOW)
      ELSE
        MIN_G = 0
        MAX_G = 0
      ENDIF

      ALLOCATE( IGBING(MIN_G:MAX_G) )

      IGBING(:) = 0

      IG = 0

      DO I = 1, NNODW
        IGN = IGNOW(I)
        IF( IGBING(IGN) == 0 ) THEN
          IG = IG + 1
          IGBING(IGN) = I
        ENDIF
      ENDDO

      NNOD = IG

!     --- INDG, GRID, IGFC, GRDL, IGNO, POS ---

      DEALLOCATE( INDG )
      DEALLOCATE( GRID )
      DEALLOCATE( IGFC )
      DEALLOCATE( GRDL )
      DEALLOCATE( IGNO )
      DEALLOCATE( POS )

      ALLOCATE( INDG(NNOD) )
      ALLOCATE( GRID(3,NNOD) )
      ALLOCATE( IGFC(NNOD) )
      ALLOCATE( GRDL(NNOD) )
      ALLOCATE( IGNO(NNOD) )
      ALLOCATE( POS(3,NNOD) )

      IG = 0

      DO IGN = MIN_G, MAX_G
        IF( IGBING(IGN) == 0 ) CYCLE
        IG = IG + 1
        I = IGBING(IGN)
        INDG(IG) = INDGW(I)
        GRID(:,IG) = GRIDW(:,I)
        IGFC(IG) = IGFCW(I)
        GRDL(IG) = GRDLW(I)
        IGNO(IG) = IGNOW(I)
        POS(:,IG) = POSW(:,I)
        IGBING(IGN) = IG
      ENDDO

      DEALLOCATE( INDGW )
      DEALLOCATE( GRIDW )
      DEALLOCATE( IGFCW )
      DEALLOCATE( GRDLW )
      DEALLOCATE( IGNOW )
      DEALLOCATE( POSW )

!     --- NODE NO. EXCHANGE ---

      DO I = 1, NELM
        N = IELM(4,I)
        IELM(5:4+N,I) = IGBING( IELM(5:4+N,I) )
      ENDDO

      DO I = 1, NPFC
        N = IPFACE(4,I)
        IPFACE(5:4+N,I) = IGBING( IPFACE(5:4+N,I) )
      ENDDO

      DEALLOCATE( IGBING )

!     --- REALLOCATE POS1, POS2, DVEL, DVEL1, DVEL2, IPGRID, IRGRID ---

      DEALLOCATE( POS1 )
      DEALLOCATE( POS2 )
      DEALLOCATE( DVEL )
      DEALLOCATE( DVEL1 )
      DEALLOCATE( DVEL2 )

      DEALLOCATE( IPGRID )
      IF( ISTM == 1 ) DEALLOCATE( IRGRID )

      ALLOCATE( POS1(3,NNOD) )
      ALLOCATE( POS2(3,NNOD) )
      ALLOCATE( DVEL(3,NELM) )
      ALLOCATE( DVEL1(3,NELM) )
      ALLOCATE( DVEL2(3,NELM) )

      ALLOCATE( IPGRID(2,NNOD) )
      IF( ISTM == 1 ) ALLOCATE( IRGRID(NNOD) )

      END
