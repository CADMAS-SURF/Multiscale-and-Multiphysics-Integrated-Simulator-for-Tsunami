      SUBROUTINE FMP_NODAL(IOUT,DT1,DT2,INDG,UG1,UG2,UG3,DZ,UG,VEL,ACC
     &                    ,RFCO,PPND,PG3,NNOD,NNODI,NPFC,IGEO,IDYN,IFL)

      USE MPI_PARAM
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*1 C /','/
      DIMENSION UG1(6,NNOD),UG2(6,NNOD),UG3(6,NNOD),DZ(NNOD),UG(6,NNOD)
     &         ,RFCO(6,NNOD),VEL(3,NNOD),ACC(3,NNOD),INDG(NNOD)
     &         ,PG3(NNOD),PPND(NNOD)
      REAL(8), POINTER :: TUG(:),TVEL(:),TACC(:),TRFCO(:)

      UG(:,:) = UG3(:,:)
      IF( ISTM == 1 ) UG(3,:) = UG(3,:) + DZ(:)

      IF( IDYN == 1 ) THEN

        VEL(:,:) = ( UG3(1:3,:) - UG1(1:3,:) ) / ( DT1 + DT2 )

        ACC(:,:) = ( ( UG3(1:3,:) - UG2(1:3,:) ) / DT2 
     &               - ( UG2(1:3,:) - UG1(1:3,:) ) / DT1 ) 
     &             / ( DT1 + DT2 ) * 2.D0

      ELSEIF( IDYN == 0 ) THEN

        VEL(:,:) = 0.

        ACC(:,:) = 0.

      ENDIF

      ALLOCATE( TUG(NNOD) )
      ALLOCATE( TVEL(NNOD) )
      ALLOCATE( TACC(NNOD) )
      ALLOCATE( TRFCO(NNOD) )

      DO I = 1, NNOD
        CALL VECML2(TUG(I),UG(1,I),3)
        CALL VECML2(TVEL(I),VEL(1,I),3)
        CALL VECML2(TACC(I),ACC(1,I),3)
        CALL VECML2(TRFCO(I),RFCO(1,I),3)
      ENDDO

      IF( MYRANK == 0 ) THEN

        WRITE(IFL,'(3(I0,A))') IOUT,C,1,C,1,C

        WRITE(IFL,'(A)') 'Total Displacement'

        CALL WT_VEC(2,3,4,7,INDG,TUG,1,1,NNOD,IFL)

        WRITE(IFL,'(3(I0,A))') IOUT,C,2,C,1,C

        WRITE(IFL,'(A)') 'X Displacement'

        CALL WT_VEC(2,0,0,7,INDG,UG,1,6,NNOD,IFL)

        WRITE(IFL,'(3(I0,A))') IOUT,C,3,C,1,C

        WRITE(IFL,'(A)') 'Y Displacement'

        CALL WT_VEC(0,3,0,7,INDG,UG,2,6,NNOD,IFL)

        WRITE(IFL,'(3(I0,A))') IOUT,C,4,C,1,C

        WRITE(IFL,'(A)') 'Z Displacement'

        CALL WT_VEC(0,0,4,7,INDG,UG,3,6,NNOD,IFL)

        WRITE(IFL,'(3(I0,A))') IOUT,C,5,C,1,C

        WRITE(IFL,'(A)') 'Total Velocity'

        CALL WT_VEC(6,7,8,7,INDG,TVEL,1,1,NNOD,IFL)

        WRITE(IFL,'(3(I0,A))') IOUT,C,6,C,1,C

        WRITE(IFL,'(A)') 'X Velocity'

        CALL WT_VEC(6,0,0,7,INDG,VEL,1,3,NNOD,IFL)

        WRITE(IFL,'(3(I0,A))') IOUT,C,7,C,1,C

        WRITE(IFL,'(A)') 'Y Velocity'

        CALL WT_VEC(0,7,0,7,INDG,VEL,2,3,NNOD,IFL)

        WRITE(IFL,'(3(I0,A))') IOUT,C,8,C,1,C

        WRITE(IFL,'(A)') 'Z Velocity'

        CALL WT_VEC(0,0,8,7,INDG,VEL,3,3,NNOD,IFL)

        WRITE(IFL,'(3(I0,A))') IOUT,C,9,C,1,C

        WRITE(IFL,'(A)') 'Total Acceleration'

        CALL WT_VEC(10,11,12,7,INDG,TACC,1,1,NNOD,IFL)

        WRITE(IFL,'(3(I0,A))') IOUT,C,10,C,1,C

        WRITE(IFL,'(A)') 'X Acceleration'

        CALL WT_VEC(10,0,0,7,INDG,ACC,1,3,NNOD,IFL)

        WRITE(IFL,'(3(I0,A))') IOUT,C,11,C,1,C

        WRITE(IFL,'(A)') 'Y Acceleration'

        CALL WT_VEC(0,11,0,7,INDG,ACC,2,3,NNOD,IFL)

        WRITE(IFL,'(3(I0,A))') IOUT,C,12,C,1,C

        WRITE(IFL,'(A)') 'Z Acceleration'

        CALL WT_VEC(0,0,12,7,INDG,ACC,3,3,NNOD,IFL)

        WRITE(IFL,'(3(I0,A))') IOUT,C,13,C,1,C

        WRITE(IFL,'(A)') 'Total Reactive Force'

        CALL WT_VEC(14,15,16,7,INDG,TRFCO,1,1,NNOD,IFL)

        WRITE(IFL,'(3(I0,A))') IOUT,C,14,C,1,C

        WRITE(IFL,'(A)') 'X Reactive Force'

        CALL WT_VEC(14,0,0,7,INDG,RFCO,1,6,NNOD,IFL)

        WRITE(IFL,'(3(I0,A))') IOUT,C,15,C,1,C

        WRITE(IFL,'(A)') 'Y Reactive Force'

        CALL WT_VEC(0,15,0,7,INDG,RFCO,2,6,NNOD,IFL)

        WRITE(IFL,'(3(I0,A))') IOUT,C,16,C,1,C

        WRITE(IFL,'(A)') 'Z Reactive Force'

        CALL WT_VEC(0,0,16,7,INDG,RFCO,3,6,NNOD,IFL)

        IF( NPFC > 0 ) THEN

          WRITE(IFL,'(3(I0,A))') IOUT,C,17,C,1,C

          WRITE(IFL,'(A)') 'Surface Pressure'

          CALL WT_VEC(0,0,0,7,INDG,PPND,1,1,NNOD,IFL)

        ENDIF

        IF( IGEO > 0 ) THEN

          WRITE(IFL,'(3(I0,A))') IOUT,C,70,C,1,C

          WRITE(IFL,'(A)') 'Pore Water Pressure'

          CALL WT_VEC(0,0,0,7,INDG,PG3,1,1,NNOD,IFL)

        ENDIF

      ELSE

        CALL M_MPI_SEND_D(TUG,NNODI,0)
        CALL M_MPI_SEND_D(UG,6*NNODI,0)

        CALL M_MPI_SEND_D(TVEL,NNODI,0)
        CALL M_MPI_SEND_D(VEL,3*NNODI,0)

        CALL M_MPI_SEND_D(TACC,NNODI,0)
        CALL M_MPI_SEND_D(ACC,3*NNODI,0)

        CALL M_MPI_SEND_D(TRFCO,NNODI,0)
        CALL M_MPI_SEND_D(RFCO,6*NNODI,0)

        IF( ICPL > 0 ) CALL M_MPI_SEND_D(PPND,NNODI,0)

        IF( IGEO > 0 ) CALL M_MPI_SEND_D(PG3,NNODI,0)

      ENDIF

      DEALLOCATE( TUG )
      DEALLOCATE( TVEL )
      DEALLOCATE( TACC )
      DEALLOCATE( TRFCO )

      END
