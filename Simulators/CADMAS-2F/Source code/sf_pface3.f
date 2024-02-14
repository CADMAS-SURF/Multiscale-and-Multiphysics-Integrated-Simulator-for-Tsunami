      SUBROUTINE SF_PFACE3()

      USE SF_ARRAY

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION XS(3,3),TR(3,3),X0(3),XS2(2,3),XS2G(2),XM(3,3),XM2(2,3)
     &         ,XM2G(2),X(3,3)

      REAL(8), POINTER :: SFC0(:),SFC(:)

      DATA AFC_MIN /1.D-4/

      IF( MYRANK > 0 ) RETURN

      AFC(:) = 0.D0

      DO I = 1, NPFC0
        IF( IPFACE0(1,I) > 0 .OR. IPFACE0(2,I) > 0 ) AFC(I) = 1.D0
      ENDDO

      IF( ICON == 0 ) RETURN

      EPS = E_MEAN * 1.D-9

      EPSZ = E_MEAN * 1.D-2

      ! SFC0 : 表面要素の面積
      ! SFC  : 接触面同士の干渉により覆われている面積

      ALLOCATE( SFC0(NPFC0), STAT=IERR )
      ALLOCATE( SFC(NPFC0), STAT=IERR )
      IF( IERR /= 0 ) CALL VF_A2ERR('SF_PFACE3','CAN NOT ALLOC.')

      SFC0(:) = 0.

      DO I = 1, NCTR
        X(:,:) = POS0(:,ICTR(2:4,I))
        CALL SF_STRIA(S,X,3)
        SFC0( ICTR(1,I) ) = SFC0( ICTR(1,I) ) + S
      ENDDO

      SFC(:) = 0.

      DO IBDY = 1, NICRG

        CALL SF_ADDSET(IS,IE,ICRG,IBDY)

        DO I = IS, IE

          IF( IPFACE0( 2, ICTR(1,I) ) > 0 ) CYCLE  ! 地盤面は無視

          XS(:,:) = POS0(:,ICTR(2:4,I))

          CALL SF_TRM1(TR,X0,XS2,XS)

          CALL SF_CSCRIB(XS2G,RS,XS2)

          DO JBDY = 1, NICRG

            IF( ICTB(IBDY,JBDY) == 0 ) CYCLE

            CALL SF_ADDSET(JS,JE,ICRG,JBDY)

            DO J = JS, JE

              IF( IPFACE0( 2, ICTR(1,J) ) > 0 ) CYCLE

              XM(:,:) = POS0(:,ICTR(2:4,J))

              CALL SF_TRNS1(XM,EZ,X0,TR)

              IF( .NOT. ( EZ > .9D0 .AND.
     &            DMAX1( DABS(XM(3,1)), DABS(XM(3,2)), DABS(XM(3,3)) )
     &            < EPSZ ) ) CYCLE

              XM2(:,:) = XM(1:2,:)

              CALL SF_CSCRIB(XM2G,RM,XM2)

              CALL SF_LENGTH(DXSM,XS2G,XM2G,2)

              IF( DXSM > RS + RM ) CYCLE

              CALL SF_CUT2TR(S,XM2,XS2,EPS)

              SFC( ICTR(1,I) ) = SFC( ICTR(1,I) ) + S
              SFC( ICTR(1,J) ) = SFC( ICTR(1,J) ) + S

            ENDDO

          ENDDO

        ENDDO

      ENDDO

      DO I = 1, NPFC0
        IF( SFC(I) == 0. ) CYCLE
        AFC(I) = AFC(I) - SFC(I) / SFC0(I)
        IF( AFC(I) < AFC_MIN ) AFC(I) = 0.
      ENDDO

      DEALLOCATE( SFC0, STAT = IERR )
      DEALLOCATE( SFC, STAT = IERR )
      IF( IERR /= 0 ) CALL VF_A2ERR('SF_PFACE3','CAN NOT DEALLOC.')

      END