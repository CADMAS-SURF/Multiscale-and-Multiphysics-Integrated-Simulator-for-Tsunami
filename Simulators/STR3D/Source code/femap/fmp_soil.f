      SUBROUTINE FMP_SOIL(IOUT,KK,INDG,IELM,NM,VELG,MGP,VELE,IFL)

      USE MPI_PARAM
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*1 C /','/
      DIMENSION KK(*),IELM(NM,*),VEL(3,20),VELG(3,MGP,*),VELE(3,*)
     &         ,INDG(*)

      INTEGER, POINTER :: NAVE(:)
      REAL(8), POINTER :: VELP(:,:),TVELP(:)

      NNOD = KK(8)
      NELM = KK(12)

      ALLOCATE( NAVE(NNOD) )
      ALLOCATE( VELP(3,NNOD) )
      ALLOCATE( TVELP(NNOD) )

      NAVE(:) = 0
      VELP(:,:) = 0.

      DO I = 1, NELM

        ITYP = IELM(2,I)
        ND   = IELM(3,I)

        IF( ITYP /= 6 ) CYCLE

        SELECT CASE( ND )
        CASE( 4 )
          CALL VELTE1(VEL,VELG(1,1,I))
        CASE( 10 )
          CALL VELTE2(VEL,VELG(1,1,I))
        CASE( 6, 15 )
          CALL VELPN2(VEL,VELG(1,1,I),ND)
        CASE( 8 )
          CALL VELHX2(VEL,VELG(1,1,I),VELE(1,I),ND,2)
        CASE( 20 )
          CALL VELHX2(VEL,VELG(1,1,I),VELE(1,I),ND,3)
        END SELECT

        CALL VAVRG(VELP,VEL,IELM(8,I),ND,NAVE)

      ENDDO

      DO I = 1, NNOD
        CALL VECML2(TVELP(I),VELP(1,I),3)
      ENDDO

      IF( MYRANK == 0 ) THEN

        WRITE(IFL,'(3(I0,A))') IOUT,C,71,C,1,C

        WRITE(IFL,'(A)') 'Total Pore Water Flux'

        CALL WT_VEC(72,73,74,7,INDG,TVELP,1,1,NNOD,IFL)

        WRITE(IFL,'(3(I0,A))') IOUT,C,72,C,1,C

        WRITE(IFL,'(A)') 'X Pore Water Flux'

        CALL WT_VEC(72,0,0,7,INDG,VELP,1,3,NNOD,IFL)

        WRITE(IFL,'(3(I0,A))') IOUT,C,73,C,1,C

        WRITE(IFL,'(A)') 'Y Pore Water Flux'

        CALL WT_VEC(0,73,0,7,INDG,VELP,2,3,NNOD,IFL)

        WRITE(IFL,'(3(I0,A))') IOUT,C,74,C,1,C

        WRITE(IFL,'(A)') 'Z Pore Water Flux'

        CALL WT_VEC(0,0,74,7,INDG,VELP,3,3,NNOD,IFL)

      ELSE

        NNODI = KK(26)

        CALL M_MPI_SEND_D(TVELP,NNODI,0)
        CALL M_MPI_SEND_D(VELP,3*NNODI,0)

      ENDIF

      DEALLOCATE( NAVE )
      DEALLOCATE( VELP )
      DEALLOCATE( TVELP )

      END
