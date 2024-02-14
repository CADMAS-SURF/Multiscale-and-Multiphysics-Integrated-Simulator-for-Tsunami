      SUBROUTINE FMP_BAR0(IOUT,NEB,NBAR,IFL)

      USE MPI_PARAM
      USE M_PART

      CHARACTER*1 C /','/
      INTEGER, POINTER :: IENO(:),IENOW(:)
      REAL(8), POINTER :: SIG(:),SIGW(:)

      ALLOCATE( IENO(NBAR) )
      ALLOCATE( SIG(NBAR) )

      ALLOCATE( IENOW(NBAR) )
      ALLOCATE( SIGW(NBAR) )

      IENO(:) = 0

      DO IP = 1, NPROCS - 1

        CALL M_MPI_RECV_I(NEBW,1,IP)
        CALL M_MPI_RECV_I(NBARW,1,IP)
        CALL M_MPI_RECV_I(IENOW,NBARW,IP)
        CALL M_MPI_RECV_D(SIGW,NBARW,IP)

        DO I = 1, NBARW
          IE = IEL(NEBW+I,IP)
          IB = IE - NEB
          IF( IENO(IB) == 0 ) THEN
            IENO(IB) = IENOW(I)
            SIG(IB) = SIGW(I)
          ENDIF
        ENDDO
        
      ENDDO

      IF( NBAR > 0 ) THEN

        WRITE(IFL,'(3(I0,A))') IOUT,C,81,C,1,C

        WRITE(IFL,'(A)') 'Bar Axial Stress'

        CALL WT_VEC(0,0,0,8,IENO,SIG,1,1,NBAR,IFL)

      ENDIF

      DEALLOCATE( IENO )
      DEALLOCATE( SIG )

      DEALLOCATE( IENOW )
      DEALLOCATE( SIGW )

      END
