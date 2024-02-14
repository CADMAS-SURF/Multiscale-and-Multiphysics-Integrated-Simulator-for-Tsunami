      SUBROUTINE FMP_ROD0(IOUT,NER,NROD,IFL)

      USE MPI_PARAM
      USE M_PART

      CHARACTER*1 C /','/
      INTEGER, POINTER :: IENO(:),IENOW(:)
      REAL(8), POINTER :: SIG(:),SIGW(:)

      ALLOCATE( IENO(NROD) )
      ALLOCATE( SIG(NROD) )

      ALLOCATE( IENOW(NROD) )
      ALLOCATE( SIGW(NROD) )

      IENO(:) = 0

      DO IP = 1, NPROCS - 1

        CALL M_MPI_RECV_I(NERW,1,IP)
        CALL M_MPI_RECV_I(NRODW,1,IP)
        CALL M_MPI_RECV_I(IENOW,NRODW,IP)
        CALL M_MPI_RECV_D(SIGW,NRODW,IP)

        DO I = 1, NRODW
          IE = IEL(NERW+I,IP)
          IR = IE - NER
          IF( IENO(IR) == 0 ) THEN
            IENO(IR) = IENOW(I)
            SIG(IR) = SIGW(I)
          ENDIF
        ENDDO
        
      ENDDO

      IF( NROD > 0 ) THEN

        WRITE(IFL,'(3(I0,A))') IOUT,C,80,C,1,C

        WRITE(IFL,'(A)') 'Rod Axial Stress'

        CALL WT_VEC(0,0,0,8,IENO,SIG,1,1,NROD,IFL)

      ENDIF

      DEALLOCATE( IENO )
      DEALLOCATE( SIG )

      DEALLOCATE( IENOW )
      DEALLOCATE( SIGW )

      END
