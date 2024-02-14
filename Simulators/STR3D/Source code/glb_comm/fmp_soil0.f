      SUBROUTINE FMP_SOIL0(IOUT,INDG,NNOD,IFL)

      CHARACTER*1 C /','/
      DIMENSION INDG(NNOD)
      REAL(8), POINTER :: TVELP(:),VELP(:,:)

      ALLOCATE( TVELP(NNOD) )

      CALL GATHER_NODAL_D(TVELP,1)

      WRITE(IFL,'(3(I0,A))') IOUT,C,71,C,1,C

      WRITE(IFL,'(A)') 'Total Pore Water Flux'

      CALL WT_VEC(72,73,74,7,INDG,TVELP,1,1,NNOD,IFL)

      DEALLOCATE( TVELP )

      ALLOCATE( VELP(3,NNOD) )

      CALL GATHER_NODAL_D(VELP,3)

      WRITE(IFL,'(3(I0,A))') IOUT,C,72,C,1,C

      WRITE(IFL,'(A)') 'X Pore Water Flux'

      CALL WT_VEC(72,0,0,7,INDG,VELP,1,3,NNOD,IFL)

      WRITE(IFL,'(3(I0,A))') IOUT,C,73,C,1,C

      WRITE(IFL,'(A)') 'Y Pore Water Flux'

      CALL WT_VEC(0,73,0,7,INDG,VELP,2,3,NNOD,IFL)

      WRITE(IFL,'(3(I0,A))') IOUT,C,74,C,1,C

      WRITE(IFL,'(A)') 'Z Pore Water Flux'

      CALL WT_VEC(0,0,74,7,INDG,VELP,3,3,NNOD,IFL)

      DEALLOCATE( VELP )

      END
