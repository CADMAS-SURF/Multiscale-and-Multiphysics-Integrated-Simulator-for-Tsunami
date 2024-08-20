      SUBROUTINE SF_CUT2X(NT,P1,P2,P0,XG,EPS,EPSA,IX,IY,IMODE)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION P1(2,3),P2(2,3),P0(2,3),IP(3),IC(3),IN(3),V1(2),V2(2)
!-----------------------------------------------------------------------
      DO I = 1, 3
        IF(XG - EPS < P0(IX,I) .AND. P0(IX,I) < XG + EPS) P0(IX,I) = XG
      ENDDO

      CALL SF_S34(S,P0,3)

      IF( DABS(S) <= EPSA ) THEN
        NT = 0
        RETURN
      ELSEIF( S < -EPSA ) THEN
        CALL VF_A2ERR('SF_CUT2X','P.G ERROR.')
      ENDIF

      IC(:) = 0
      IN(:) = 0

      IF( IMODE == 1 ) THEN

        DO I = 1, 3
          IF( P0(IX,I) < XG ) THEN
            IC(1) = IC(1) + 1
          ELSEIF( P0(IX,I) == XG ) THEN
            IC(2) = IC(2) + 1
          ELSE
            IC(3) = IC(3) + 1
            IN(I) = 1
          ENDIF
        ENDDO

      ELSEIF( IMODE == 2 ) THEN

        DO I = 1, 3
          IF( P0(IX,I) > XG ) THEN
            IC(1) = IC(1) + 1
          ELSEIF( P0(IX,I) == XG ) THEN
            IC(2) = IC(2) + 1
          ELSE
            IC(3) = IC(3) + 1
            IN(I) = 1
          ENDIF
        ENDDO

      ENDIF

      IST = 100 * IC(1) + 10 * IC(2) + IC(3)

      SELECT CASE( IST )
      CASE( 300, 210, 120 )

        NT = 0

      CASE( 201, 111, 21 )

        NT = 1

        IF( IN(1) == 1 ) THEN
          IP(1) = 1
          IP(2) = 2
          IP(3) = 3
        ELSEIF( IN(2) == 1 ) THEN
          IP(1) = 2
          IP(2) = 3
          IP(3) = 1
        ELSEIF( IN(3) == 1 ) THEN
          IP(1) = 3
          IP(2) = 1
          IP(3) = 2
        ENDIF

        Y1 = SF_RINT( P0(1,IP(2)), P0(1,IP(1)), IX, IY, XG )
        Y2 = SF_RINT( P0(1,IP(3)), P0(1,IP(1)), IX, IY, XG )

        P1(:,1) = P0(:,IP(1))

        P1(IX,2) = XG
        P1(IY,2) = Y1

        P1(IX,3) = XG
        P1(IY,3) = Y2

      CASE( 102 )

        NT = 2

        IF( IN(1) == 0 ) THEN
          IP(1) = 1
          IP(2) = 2
          IP(3) = 3
        ELSEIF( IN(2) == 0 ) THEN
          IP(1) = 2
          IP(2) = 3
          IP(3) = 1
        ELSEIF( IN(3) == 0 ) THEN
          IP(1) = 3
          IP(2) = 1
          IP(3) = 2
        ENDIF

        Y1 = SF_RINT( P0(1,IP(1)), P0(1,IP(3)), IX, IY, XG )
        Y2 = SF_RINT( P0(1,IP(1)), P0(1,IP(2)), IX, IY, XG )

        V1(IX) = P0(IX,IP(2)) - XG
        V1(IY) = P0(IY,IP(2)) - Y1

        V2(IX) = P0(IX,IP(3)) - XG
        V2(IY) = P0(IY,IP(3)) - Y2
        
        CALL SF_DIRCOS(V1,V1,2)
        CALL SF_DIRCOS(V2,V2,2)

        IF( DABS(V1(IX)) >= DABS(V2(IX)) ) THEN
          P1(:,1) = P0(:,IP(2))
        ELSE
          P1(:,1) = P0(:,IP(3))
        ENDIF

        P1(IX,2) = XG
        P1(IY,2) = Y1

        P1(IX,3) = XG
        P1(IY,3) = Y2

        P2(:,1) = P0(:,IP(2))

        P2(:,2) = P0(:,IP(3))

        P2(IX,3) = XG
        IF( DABS(V1(IX)) >= DABS(V2(IX)) ) THEN
          P2(IY,3) = Y1
        ELSE
          P2(IY,3) = Y2
        ENDIF

      CASE( 12, 3 )

        NT = 1

        P1(:,:) = P0(:,:)

      CASE( 30 )

        NT = 0

      CASE DEFAULT

        CALL VF_A2ERR('SF_CUT2X','P.G ERROR.')

      END SELECT
        
      END