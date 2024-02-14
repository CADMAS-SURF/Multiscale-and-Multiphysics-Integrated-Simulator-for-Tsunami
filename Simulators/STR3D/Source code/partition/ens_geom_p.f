      SUBROUTINE ENS_GEOM_P(KK,NPART,IEP,IFCAS,IFG)
C
      USE M_VAL
C
      DIMENSION KK(*),IEP(*)
      INTEGER, POINTER :: IE(:), NUG(:), IG(:), LG(:)
C
      NNOD = KK(8)
      NSOL = KK(10)
C
      OPEN( IFCAS, FILE = 'str_p.case')
C
      WRITE(IFCAS,'(A)') 'FORMAT'
C
      WRITE(IFCAS,'(A)') 'type: ensight gold'
C
      WRITE(IFCAS,'(A)') 'GEOMETRY'
C
      WRITE(IFCAS,'(2A)') 'model: ', 'part.geom'
C
      CLOSE( IFCAS )
C
      OPEN( IFG , FILE = 'part.geom' )
C
      ALLOCATE( IE(NSOL*6) )
      ALLOCATE( NUG(NNOD) )
      ALLOCATE( IG(NNOD) )
      ALLOCATE( LG(NNOD) )
C
      WRITE(IFG,'(A)') 'ASTEA MECHANICAL'
      WRITE(IFG,'(A)') 'EnSight Gold geometry file'
      WRITE(IFG,'(A)') 'node id given'
      WRITE(IFG,'(A)') 'element id given'
C
      DO IPART = 1, NPART
        CALL PART_GEOM( INDG, GRID, IELM, KK(37), NNOD, NSOL, IE,
     &                  NUG, IG, LG, IFG, IEP, IPART )
      ENDDO
C
      DEALLOCATE( IE )
      DEALLOCATE( NUG )
      DEALLOCATE( IG )
      DEALLOCATE( LG )
C
      CLOSE( IFG )
C
      END
