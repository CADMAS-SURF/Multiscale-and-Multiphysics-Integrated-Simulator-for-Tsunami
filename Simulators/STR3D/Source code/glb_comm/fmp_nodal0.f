      SUBROUTINE FMP_NODAL0(IOUT,INDG,NNOD,NPFC,IGEO,IFL)

      CHARACTER*1 C /','/
      DIMENSION INDG(NNOD)
      REAL(8), POINTER :: TUG(:),UG(:,:),TVEL(:),VEL(:,:),TACC(:)
     &                   ,ACC(:,:),TRFCO(:),RFCO(:,:),PPND(:),PG3(:)

      ALLOCATE( TUG(NNOD) )

      CALL GATHER_NODAL_D(TUG,1)

      WRITE(IFL,'(3(I0,A))') IOUT,C,1,C,1,C

      WRITE(IFL,'(A)') 'Total Displacement'

      CALL WT_VEC(2,3,4,7,INDG,TUG,1,1,NNOD,IFL)

      DEALLOCATE( TUG )

      ALLOCATE( UG(6,NNOD) )

      CALL GATHER_NODAL_D(UG,6)

      WRITE(IFL,'(3(I0,A))') IOUT,C,2,C,1,C

      WRITE(IFL,'(A)') 'X Displacement'

      CALL WT_VEC(2,0,0,7,INDG,UG,1,6,NNOD,IFL)

      WRITE(IFL,'(3(I0,A))') IOUT,C,3,C,1,C

      WRITE(IFL,'(A)') 'Y Displacement'

      CALL WT_VEC(0,3,0,7,INDG,UG,2,6,NNOD,IFL)

      WRITE(IFL,'(3(I0,A))') IOUT,C,4,C,1,C

      WRITE(IFL,'(A)') 'Z Displacement'

      CALL WT_VEC(0,0,4,7,INDG,UG,3,6,NNOD,IFL)

      DEALLOCATE( UG )

      ALLOCATE( TVEL(NNOD) )

      CALL GATHER_NODAL_D(TVEL,1)

      WRITE(IFL,'(3(I0,A))') IOUT,C,5,C,1,C

      WRITE(IFL,'(A)') 'Total Velocity'

      CALL WT_VEC(6,7,8,7,INDG,TVEL,1,1,NNOD,IFL)

      DEALLOCATE( TVEL )

      ALLOCATE( VEL(3,NNOD) )

      CALL GATHER_NODAL_D(VEL,3)

      WRITE(IFL,'(3(I0,A))') IOUT,C,6,C,1,C

      WRITE(IFL,'(A)') 'X Velocity'

      CALL WT_VEC(6,0,0,7,INDG,VEL,1,3,NNOD,IFL)

      WRITE(IFL,'(3(I0,A))') IOUT,C,7,C,1,C

      WRITE(IFL,'(A)') 'Y Velocity'

      CALL WT_VEC(0,7,0,7,INDG,VEL,2,3,NNOD,IFL)

      WRITE(IFL,'(3(I0,A))') IOUT,C,8,C,1,C

      WRITE(IFL,'(A)') 'Z Velocity'

      CALL WT_VEC(0,0,8,7,INDG,VEL,3,3,NNOD,IFL)

      DEALLOCATE( VEL )

      ALLOCATE( TACC(NNOD) )

      CALL GATHER_NODAL_D(TACC,1)

      WRITE(IFL,'(3(I0,A))') IOUT,C,9,C,1,C

      WRITE(IFL,'(A)') 'Total Acceleration'

      CALL WT_VEC(10,11,12,7,INDG,TACC,1,1,NNOD,IFL)

      DEALLOCATE( TACC )

      ALLOCATE( ACC(3,NNOD) )

      CALL GATHER_NODAL_D(ACC,3)

      WRITE(IFL,'(3(I0,A))') IOUT,C,10,C,1,C

      WRITE(IFL,'(A)') 'X Acceleration'

      CALL WT_VEC(10,0,0,7,INDG,ACC,1,3,NNOD,IFL)

      WRITE(IFL,'(3(I0,A))') IOUT,C,11,C,1,C

      WRITE(IFL,'(A)') 'Y Acceleration'

      CALL WT_VEC(0,11,0,7,INDG,ACC,2,3,NNOD,IFL)

      WRITE(IFL,'(3(I0,A))') IOUT,C,12,C,1,C

      WRITE(IFL,'(A)') 'Z Acceleration'

      CALL WT_VEC(0,0,12,7,INDG,ACC,3,3,NNOD,IFL)

      DEALLOCATE( ACC )

      ALLOCATE( TRFCO(NNOD) )

      CALL GATHER_NODAL_D(TRFCO,1)

      WRITE(IFL,'(3(I0,A))') IOUT,C,13,C,1,C

      WRITE(IFL,'(A)') 'Total Reactive Force'

      CALL WT_VEC(14,15,16,7,INDG,TRFCO,1,1,NNOD,IFL)

      DEALLOCATE( TRFCO )

      ALLOCATE( RFCO(6,NNOD) )

      CALL GATHER_NODAL_D(RFCO,6)

      WRITE(IFL,'(3(I0,A))') IOUT,C,14,C,1,C

      WRITE(IFL,'(A)') 'X Reactive Force'

      CALL WT_VEC(14,0,0,7,INDG,RFCO,1,6,NNOD,IFL)

      WRITE(IFL,'(3(I0,A))') IOUT,C,15,C,1,C

      WRITE(IFL,'(A)') 'Y Reactive Force'

      CALL WT_VEC(0,15,0,7,INDG,RFCO,2,6,NNOD,IFL)

      WRITE(IFL,'(3(I0,A))') IOUT,C,16,C,1,C

      WRITE(IFL,'(A)') 'Z Reactive Force'

      CALL WT_VEC(0,0,16,7,INDG,RFCO,3,6,NNOD,IFL)

      DEALLOCATE( RFCO )

      IF( NPFC > 0 ) THEN

        ALLOCATE( PPND(NNOD) )

        CALL GATHER_NODAL_D(PPND,1)

        WRITE(IFL,'(3(I0,A))') IOUT,C,17,C,1,C

        WRITE(IFL,'(A)') 'Surface Pressure'

        CALL WT_VEC(0,0,0,7,INDG,PPND,1,1,NNOD,IFL)

        DEALLOCATE( PPND )

      ENDIF

      IF( IGEO > 0 ) THEN

        ALLOCATE( PG3(NNOD) )

        CALL GATHER_NODAL_D(PG3,1)

        WRITE(IFL,'(3(I0,A))') IOUT,C,70,C,1,C

        WRITE(IFL,'(A)') 'Pore Water Pressure'

        CALL WT_VEC(0,0,0,7,INDG,PG3,1,1,NNOD,IFL)

        DEALLOCATE( PG3 )

      ENDIF

      END
