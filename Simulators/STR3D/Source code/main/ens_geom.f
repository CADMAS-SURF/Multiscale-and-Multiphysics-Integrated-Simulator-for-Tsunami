      SUBROUTINE ENS_GEOM( KK, IFG, FLNAME, NLEN, ICHK )
C
      USE M_VAL
C
      CHARACTER*256 FLNAME
      CHARACTER*7 FLD /'str_out'/
C
      DIMENSION KK(*)
C
      INTEGER, POINTER :: NUG(:), LG(:), INDGW(:)
      REAL(8), POINTER :: GRIDW(:,:)
C
      CALL MK_DIR(FLD)
C
      OPEN( IFG, FILE = FLD//'/'//FLNAME(1:NLEN)//'.geom' )
C
      WRITE(IFG,'(A)') 'ASTEA MECHANICAL'
      WRITE(IFG,'(A)') 'EnSight Gold geometry file'
      WRITE(IFG,'(A)') 'node id given'
      WRITE(IFG,'(A)') 'element id given'
C
      NNOD = KK(8)
      NSHL = KK(9)
      NSOL = KK(10)
      NELM = KK(12)
      NROD = KK(14)
      NBAR = KK(16)
C
      ALLOCATE( NP_ENS(6) )
      ALLOCATE( IP_ENS(NELM) )
C
      CALL PART_RENUM( N_PART, NP_ENS, IP_ENS, KK, IELM, KK(37) )
C
      ALLOCATE( NG_ENS(N_PART) )
      ALLOCATE( IG_ENS(NNOD,N_PART) )
C
      ALLOCATE( NE_ENS(6,N_PART) )
      ALLOCATE( IE_ENS(6*NELM,N_PART) )
C
      ALLOCATE( NUG(NNOD) )
      ALLOCATE( LG(NNOD) )
C
      IPART = 0
C
      DO JPART = 1, NP_ENS(1)
        IPART = IPART + 1
        CALL SHELL_GEOM( NG_ENS(IPART), IG_ENS(1,IPART), 
     &                   NE_ENS(1,IPART), IE_ENS(1,IPART), IP_ENS,
     &                   IPART, JPART, INDG, GRID, IELM, KK(37),
     &                   NNOD, NSHL, NUG, LG, IFG )
      ENDDO
C
      ISOL = NSHL + 1
C
      DO JPART = 1, NP_ENS(2)
        IPART = IPART + 1
        CALL SOLID_GEOM( NG_ENS(IPART), IG_ENS(1,IPART),
     &                   NE_ENS(1,IPART), IE_ENS(1,IPART), IP_ENS(ISOL),
     &                   IPART, JPART, 1, INDG, GRID, IELM(1,ISOL),
     &                   KK(37), NNOD, NSOL, NUG, LG, IFG )
      ENDDO
C
      IROD = NSHL + NSOL + 1
C
      DO JPART = 1, NP_ENS(3)
        IPART = IPART + 1
        CALL ROD_GEOM( NG_ENS(IPART), IG_ENS(1,IPART), NE_ENS(1,IPART),
     &                 IE_ENS(1,IPART), IP_ENS(IROD), IPART, JPART,
     &                 INDG, GRID, IELM(1,IROD), KK(37), NNOD, NROD,
     &                 NUG, LG, IFG )
      ENDDO
C
      IBAR = NSHL + NSOL + NROD + 1
C
      DO JPART = 1, NP_ENS(4)
        IPART = IPART + 1
        CALL BAR_GEOM( NG_ENS(IPART), IG_ENS(1,IPART), NE_ENS(1,IPART),
     &                 IE_ENS(1,IPART), IP_ENS(IBAR), IPART, JPART,
     &                 INDG, GRID, IELM(1,IBAR), KK(37), NNOD, NBAR,
     &                 NUG, LG, IFG )
      ENDDO
C
      DO JPART = 1, NP_ENS(6)
        IPART = IPART + 1
        CALL SOLID_GEOM( NG_ENS(IPART), IG_ENS(1,IPART),
     &                   NE_ENS(1,IPART), IE_ENS(1,IPART), IP_ENS(ISOL),
     &                   IPART, JPART, 2, INDG, GRID, IELM(1,ISOL),
     &                   KK(37), NNOD, NSOL, NUG, LG, IFG )
      ENDDO
C
      DEALLOCATE( IP_ENS )
C
      DEALLOCATE( NUG )
      DEALLOCATE( LG )
C
      IF( ICHK < 2 ) RETURN
C
      NMAT  = KK(11)
      NPFC  = KK(81)
      NICRG = KK(92)
      NIGSF = KK(94)
C
      ALLOCATE( NG_ENSW(NMAT+NICRG) )
      ALLOCATE( IG_ENSW(NNOD+NIGSF,NMAT+NICRG) )
C
      ALLOCATE( NE_ENSW(4,NMAT) )
      ALLOCATE( IE_ENSW(4*NPFC,NMAT) )
C
      ALLOCATE( NUG(NNOD+NIGSF) )
      ALLOCATE( LG(NNOD+NIGSF) )
C
      IPART = 0
C
      IF( NPFC > 0 ) THEN
C
        ALLOCATE( IP_ENS(NPFC) )
C
        CALL PART_SURF( IP_ENS, IELM, IPFC, KK(37), NPFC )
C
        DO JPART = 1, NMAT
          IPART = IPART + 1
          CALL SURF_GEOM( NG_ENSW(IPART), IG_ENSW(1,IPART),
     &                    NE_ENSW(1,IPART), IE_ENSW(1,IPART), IP_ENS,
     &                    N_PART+IPART, JPART, INDG, GRID, IPFC, NNOD,
     &                    NPFC, NUG, LG, IFG )
        ENDDO
C
        DEALLOCATE( IP_ENS )
C
      ENDIF
C
      IF( NICRG > 0 ) THEN
C
        ALLOCATE( INDGW(NNOD+NIGSF) )
        ALLOCATE( GRIDW(3,NNOD+NIGSF) )
C
        INDGW(1:NNOD) = INDG(:)
C
        MAX_INDG = MAXVAL( INDG )
        DO I = 1, NIGSF
          INDGW(NNOD+I) = MAX_INDG + I
        ENDDO
C
        GRIDW(:,1:NNOD) = GRID(:,:)
C
        CALL GSURF(GRIDW(1,NNOD+1),GRID,IELM,KK(37),NELM,IBEL,3)
C
        DO JPART = 1, NICRG
          IPART = IPART + 1
          CALL CONTACT_GEOM( NG_ENSW(IPART), IG_ENSW(1,IPART),
     &                       N_PART+IPART, JPART, INDGW, GRIDW, ICBD,
     &                       IELC, NNOD+NIGSF, NUG, LG, IFG )
        ENDDO
C
        DEALLOCATE( INDGW )
        DEALLOCATE( GRIDW )
C
      ENDIF
C
      DEALLOCATE( NUG )
      DEALLOCATE( LG )
C
      CLOSE( IFG )
C
      END
