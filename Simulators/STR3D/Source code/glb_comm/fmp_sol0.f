      SUBROUTINE FMP_SOL0(IOUT,INDG,NNOD,IFL)
C
      CHARACTER*1 C /','/
      DIMENSION INDG(NNOD)
      REAL(8), POINTER :: SIGP(:,:),PRNSIGP(:,:)
C
      ALLOCATE( SIGP(6,NNOD) )
C
      CALL GATHER_NODAL_D(SIGP,6)
C
      WRITE(IFL,'(3(I0,A))') IOUT,C,30,C,1,C
C
      WRITE(IFL,'(A)') 'X Normal Stress'
C
      CALL WT_VEC(0,0,0,7,INDG,SIGP,1,6,NNOD,IFL)
C
      WRITE(IFL,'(3(I0,A))') IOUT,C,31,C,1,C
C
      WRITE(IFL,'(A)') 'Y Normal Stress'
C
      CALL WT_VEC(0,0,0,7,INDG,SIGP,2,6,NNOD,IFL)
C
      WRITE(IFL,'(3(I0,A))') IOUT,C,32,C,1,C
C
      WRITE(IFL,'(A)') 'Z Normal Stress'
C
      CALL WT_VEC(0,0,0,7,INDG,SIGP,3,6,NNOD,IFL)
C
      WRITE(IFL,'(3(I0,A))') IOUT,C,33,C,1,C
C
      WRITE(IFL,'(A)') 'XY Shear Stress'
C
      CALL WT_VEC(0,0,0,7,INDG,SIGP,4,6,NNOD,IFL)
C
      WRITE(IFL,'(3(I0,A))') IOUT,C,34,C,1,C
C
      WRITE(IFL,'(A)') 'YZ Shear Stress'
C
      CALL WT_VEC(0,0,0,7,INDG,SIGP,5,6,NNOD,IFL)
C
      WRITE(IFL,'(3(I0,A))') IOUT,C,35,C,1,C
C
      WRITE(IFL,'(A)') 'ZX Shear Stress'
C
      CALL WT_VEC(0,0,0,7,INDG,SIGP,6,6,NNOD,IFL)
C
      DEALLOCATE( SIGP )
C
      ALLOCATE( PRNSIGP(3,NNOD) )
C
      CALL GATHER_NODAL_D(PRNSIGP,3)
C
      WRITE(IFL,'(3(I0,A))') IOUT,C,36,C,1,C
C
      WRITE(IFL,'(A)') 'Max Prin Stress'
C
      CALL WT_VEC(0,0,0,7,INDG,PRNSIGP,1,3,NNOD,IFL)
C
      WRITE(IFL,'(3(I0,A))') IOUT,C,37,C,1,C
C
      WRITE(IFL,'(A)') 'Min Prin Stress'
C
      CALL WT_VEC(0,0,0,7,INDG,PRNSIGP,3,3,NNOD,IFL)
C
      DEALLOCATE( PRNSIGP )
C
      END
