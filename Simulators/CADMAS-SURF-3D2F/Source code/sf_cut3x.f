      SUBROUTINE SF_CUT3X(NT,P1,P2,P3,SS,P0,XG,YG,ZG,EPS,EPSV,IX,IY,IZ
     &                   ,IMODE)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION P1(3,4),P2(3,4),P3(3,4),P0(3,4),IC(3),IN(4),IO(4),IP(4)
     &         ,Y(4),Z(4),R(2,3),YG(2),ZG(2),PX(4),RX(3)
!-----------------------------------------------------------------------
      PX(:) = P0(IX,:)

      DO I = 1, 4
        IF(XG - EPS < P0(IX,I) .AND. P0(IX,I) < XG + EPS) P0(IX,I) = XG
      ENDDO

      CALL SF_TETVOL(P0,V)

C     IF( DABS(V) <= EPSV ) THEN
C       NT = 0
C       RETURN
C     ELSEIF( V < -EPSV ) THEN
C       CALL VF_A2ERR('SF_CUT3X','P.G ERROR.')
C     ENDIF

      IF( -EPSV*1.D3 <= V .AND. V <= EPSV ) THEN
        NT = 0
        RETURN
      ELSEIF( V < -EPSV*1.D3 ) THEN
        CALL VF_A2ERR('SF_CUT3X','P.G ERROR.')
      ENDIF

      IC(:) = 0
      IN(:) = 0
      IO(:) = 0

      IF( IMODE == 1 ) THEN

        DO I = 1, 4
          IF( P0(IX,I) < XG ) THEN
            IC(1) = IC(1) + 1
            IO(I) = 1
          ELSEIF( P0(IX,I) == XG ) THEN
            IC(2) = IC(2) + 1
          ELSE
            IC(3) = IC(3) + 1
            IN(I) = 1
          ENDIF
        ENDDO

      ELSEIF( IMODE == 2 ) THEN

        DO I = 1, 4
          IF( P0(IX,I) > XG ) THEN
            IC(1) = IC(1) + 1
            IO(I) = 1
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
      CASE( 400, 310, 220 )  ! OUT

        NT = 0

      CASE( 130 )  ! OUT

        NT = 0

        IF( IO(1) == 1 ) THEN
          IP(1) = 2
          IP(2) = 4
          IP(3) = 3
          IP(4) = 1
        ELSEIF( IO(2) == 1 ) THEN
          IP(1) = 3
          IP(2) = 4
          IP(3) = 1
          IP(4) = 2
        ELSEIF( IO(3) == 1 ) THEN
          IP(1) = 4
          IP(2) = 2
          IP(3) = 1
          IP(4) = 3
        ELSEIF( IO(4) == 1 ) THEN
          IP(1) = 1
          IP(2) = 2
          IP(3) = 3
          IP(4) = 4
        ENDIF

        IF( IMODE == 1 ) THEN

          RX(:) = PX(IP(1:3))

          CALL SF_MEAN1(RXM,RX,3)

          IF( RXM > XG ) THEN

            R(1,1) = P0(IY,IP(1))
            R(2,1) = P0(IZ,IP(1))
            R(1,2) = P0(IY,IP(3))
            R(2,2) = P0(IZ,IP(3))
            R(1,3) = P0(IY,IP(2))
            R(2,3) = P0(IZ,IP(2))

            CALL SF_CUT2(S,R,YG,ZG,EPS)

            SS = SS + S

          ENDIF

        ENDIF

      CASE( 301, 211, 121 )  ! TETRA

        NT = 1

        IF( IN(1) == 1 ) THEN
          IP(1) = 2
          IP(2) = 4
          IP(3) = 3
          IP(4) = 1
        ELSEIF( IN(2) == 1 ) THEN
          IP(1) = 3
          IP(2) = 4
          IP(3) = 1
          IP(4) = 2
        ELSEIF( IN(3) == 1 ) THEN
          IP(1) = 4
          IP(2) = 2
          IP(3) = 1
          IP(4) = 3
        ELSEIF( IN(4) == 1 ) THEN
          IP(1) = 1
          IP(2) = 2
          IP(3) = 3
          IP(4) = 4
        ENDIF

        DO I = 1, 3
          Y(I) = SF_RINT( P0(1,IP(I)), P0(1,IP(4)), IX, IY, XG )
          Z(I) = SF_RINT( P0(1,IP(I)), P0(1,IP(4)), IX, IZ, XG )
        ENDDO

        P1(IX,1) = XG
        P1(IY,1) = Y(1)
        P1(IZ,1) = Z(1)

        P1(IX,2) = XG
        P1(IY,2) = Y(2)
        P1(IZ,2) = Z(2)

        P1(IX,3) = XG
        P1(IY,3) = Y(3)
        P1(IZ,3) = Z(3)

        P1(:,4) = P0(:,IP(4))

        IF( IMODE == 1 ) THEN

          R(1,1) = Y(1)
          R(2,1) = Z(1)
          R(1,2) = Y(2)
          R(2,2) = Z(2)
          R(1,3) = Y(3)
          R(2,3) = Z(3)

          CALL SF_CUT2(S,R,YG,ZG,EPS)

          SS = SS + S

        ENDIF

      CASE( 202 )  ! PENTA A

        NT = 3

        IF( IN(1) == 1 .AND. IN(2) == 1 ) THEN
          IP(1) = 1
          IP(2) = 3
          IP(3) = 4
          IP(4) = 2
        ELSEIF( IN(1) == 1 .AND. IN(3) == 1 ) THEN
          IP(1) = 1
          IP(2) = 4
          IP(3) = 2
          IP(4) = 3
        ELSEIF( IN(1) == 1 .AND. IN(4) == 1 ) THEN
          IP(1) = 1
          IP(2) = 2
          IP(3) = 3
          IP(4) = 4
        ELSEIF( IN(2) == 1 .AND. IN(3) == 1 ) THEN
          IP(1) = 2
          IP(2) = 1
          IP(3) = 4
          IP(4) = 3
        ELSEIF( IN(2) == 1 .AND. IN(4) == 1 ) THEN
          IP(1) = 2
          IP(2) = 3
          IP(3) = 1
          IP(4) = 4
        ELSEIF( IN(3) == 1 .AND. IN(4) == 1 ) THEN
          IP(1) = 3
          IP(2) = 1
          IP(3) = 2
          IP(4) = 4
        ENDIF

        Y(1) = SF_RINT( P0(1,IP(2)), P0(1,IP(1)), IX, IY, XG )
        Z(1) = SF_RINT( P0(1,IP(2)), P0(1,IP(1)), IX, IZ, XG )

        Y(2) = SF_RINT( P0(1,IP(2)), P0(1,IP(4)), IX, IY, XG )
        Z(2) = SF_RINT( P0(1,IP(2)), P0(1,IP(4)), IX, IZ, XG )

        Y(3) = SF_RINT( P0(1,IP(3)), P0(1,IP(4)), IX, IY, XG )
        Z(3) = SF_RINT( P0(1,IP(3)), P0(1,IP(4)), IX, IZ, XG )

        Y(4) = SF_RINT( P0(1,IP(3)), P0(1,IP(1)), IX, IY, XG )
        Z(4) = SF_RINT( P0(1,IP(3)), P0(1,IP(1)), IX, IZ, XG )

        P1(:,1) = P0(:,IP(1))

        P1(:,2) = P0(:,IP(4))

        P1(IX,3) = XG
        P1(IY,3) = Y(2)
        P1(IZ,3) = Z(2)

        P1(IX,4) = XG
        P1(IY,4) = Y(4)
        P1(IZ,4) = Z(4)

        P2(:,1) = P0(:,IP(1))

        P2(IX,2) = XG
        P2(IY,2) = Y(1)
        P2(IZ,2) = Z(1)

        P2(IX,3) = XG
        P2(IY,3) = Y(4)
        P2(IZ,3) = Z(4)

        P2(IX,4) = XG
        P2(IY,4) = Y(2)
        P2(IZ,4) = Z(2)

        P3(:,1) = P0(:,IP(4))

        P3(IX,2) = XG
        P3(IY,2) = Y(2)
        P3(IZ,2) = Z(2)

        P3(IX,3) = XG
        P3(IY,3) = Y(4)
        P3(IZ,3) = Z(4)

        P3(IX,4) = XG
        P3(IY,4) = Y(3)
        P3(IZ,4) = Z(3)

        IF( IMODE == 1 ) THEN

          R(1,1) = Y(1)
          R(2,1) = Z(1)
          R(1,2) = Y(2)
          R(2,2) = Z(2)
          R(1,3) = Y(4)
          R(2,3) = Z(4)

          CALL SF_CUT2(S1,R,YG,ZG,EPS)

          R(1,1) = Y(2)
          R(2,1) = Z(2)
          R(1,2) = Y(3)
          R(2,2) = Z(3)
          R(1,3) = Y(4)
          R(2,3) = Z(4)

          CALL SF_CUT2(S2,R,YG,ZG,EPS)

          SS = SS + S1 + S2

        ENDIF

      CASE( 112 )  ! PYRAMID

        NT = 2

        IF( IN(1) == 1 .AND. IN(2) == 1 ) THEN
          IF( IO(3) == 1 ) THEN
            IP(1) = 1
            IP(2) = 4
            IP(3) = 2
            IP(4) = 3
          ELSE
            IP(1) = 2
            IP(2) = 3
            IP(3) = 1
            IP(4) = 4
          ENDIF
        ELSEIF( IN(1) == 1 .AND. IN(3) == 1 ) THEN
          IF( IO(2) == 1 ) THEN
            IP(1) = 3
            IP(2) = 4
            IP(3) = 1
            IP(4) = 2
          ELSE
            IP(1) = 1
            IP(2) = 2
            IP(3) = 3
            IP(4) = 4
          ENDIF
        ELSEIF( IN(1) == 1 .AND. IN(4) == 1 ) THEN
          IF( IO(2) == 1 ) THEN
            IP(1) = 1
            IP(2) = 3
            IP(3) = 4
            IP(4) = 2
          ELSE
            IP(1) = 4
            IP(2) = 2
            IP(3) = 1
            IP(4) = 3
          ENDIF
        ELSEIF( IN(2) == 1 .AND. IN(3) == 1 ) THEN
          IF( IO(1) == 1 ) THEN
            IP(1) = 2
            IP(2) = 4
            IP(3) = 3
            IP(4) = 1
          ELSE
            IP(1) = 3
            IP(2) = 1
            IP(3) = 2
            IP(4) = 4
          ENDIF
        ELSEIF( IN(2) == 1 .AND. IN(4) == 1 ) THEN
          IF( IO(1) == 1 ) THEN
            IP(1) = 4
            IP(2) = 3
            IP(3) = 2
            IP(4) = 1
          ELSE
            IP(1) = 2
            IP(2) = 1
            IP(3) = 4
            IP(4) = 3
          ENDIF
        ELSEIF( IN(3) == 1 .AND. IN(4) == 1 ) THEN
          IF( IO(1) == 1 ) THEN
            IP(1) = 3
            IP(2) = 2
            IP(3) = 4
            IP(4) = 1
          ELSE
            IP(1) = 4
            IP(2) = 1
            IP(3) = 3
            IP(4) = 2
          ENDIF
        ENDIF

        Y(1) = SF_RINT( P0(1,IP(4)), P0(1,IP(1)), IX, IY, XG )
        Z(1) = SF_RINT( P0(1,IP(4)), P0(1,IP(1)), IX, IZ, XG )

        Y(2) = SF_RINT( P0(1,IP(4)), P0(1,IP(3)), IX, IY, XG )
        Z(2) = SF_RINT( P0(1,IP(4)), P0(1,IP(3)), IX, IZ, XG )

        P1(:,1) = P0(:,IP(1))

        P1(IX,2) = XG
        P1(IY,2) = Y(2)
        P1(IZ,2) = Z(2)

        P1(IX,3) = XG
        P1(IY,3) = Y(1)
        P1(IZ,3) = Z(1)

        P1(:,4) = P0(:,IP(2))

        P2(:,1) = P0(:,IP(1))

        P2(:,2) = P0(:,IP(3))

        P2(IX,3) = XG
        P2(IY,3) = Y(2)
        P2(IZ,3) = Z(2)

        P2(:,4) = P0(:,IP(2))

        IF( IMODE == 1 ) THEN

          R(1,1) = Y(1)
          R(2,1) = Z(1)
          R(1,2) = Y(2)
          R(2,2) = Z(2)
          R(1,3) = P0(IY,IP(2))
          R(2,3) = P0(IZ,IP(2))

          CALL SF_CUT2(S,R,YG,ZG,EPS)

          SS = SS + S

        ENDIF

      CASE( 103 )  ! PENTA B

        NT = 3

        IF( IO(1) == 1 ) THEN
          IP(1) = 2
          IP(2) = 4
          IP(3) = 3
          IP(4) = 1
        ELSEIF( IO(2) == 1 ) THEN
          IP(1) = 3
          IP(2) = 4
          IP(3) = 1
          IP(4) = 2
        ELSEIF( IO(3) == 1 ) THEN
          IP(1) = 4
          IP(2) = 2
          IP(3) = 1
          IP(4) = 3
        ELSEIF( IO(4) == 1 ) THEN
          IP(1) = 1
          IP(2) = 2
          IP(3) = 3
          IP(4) = 4
        ENDIF

        DO I = 1, 3
          Y(I) = SF_RINT( P0(1,IP(4)), P0(1,IP(I)), IX, IY, XG )
          Z(I) = SF_RINT( P0(1,IP(4)), P0(1,IP(I)), IX, IZ, XG )
        ENDDO

        P1(:,1) = P0(:,IP(1))

        P1(:,2) = P0(:,IP(2))

        P1(:,3) = P0(:,IP(3))

        P1(IX,4) = XG
        P1(IY,4) = Y(2)
        P1(IZ,4) = Z(2)

        P2(:,1) = P0(:,IP(1))

        P2(:,2) = P0(:,IP(3))

        P2(IX,3) = XG
        P2(IY,3) = Y(1)
        P2(IZ,3) = Z(1)

        P2(IX,4) = XG
        P2(IY,4) = Y(2)
        P2(IZ,4) = Z(2)

        P3(:,1) = P0(:,IP(3))

        P3(IX,2) = XG
        P3(IY,2) = Y(1)
        P3(IZ,2) = Z(1)

        P3(IX,3) = XG
        P3(IY,3) = Y(2)
        P3(IZ,3) = Z(2)

        P3(IX,4) = XG
        P3(IY,4) = Y(3)
        P3(IZ,4) = Z(3)

        IF( IMODE == 1 ) THEN

          R(1,1) = Y(1)
          R(2,1) = Z(1)
          R(1,2) = Y(3)
          R(2,2) = Z(3)
          R(1,3) = Y(2)
          R(2,3) = Z(2)

          CALL SF_CUT2(S,R,YG,ZG,EPS)

          SS = SS + S

        ENDIF

      CASE( 31 )  ! IN

        NT = 1

        IF( IN(1) == 1 ) THEN
          IP(1) = 2
          IP(2) = 4
          IP(3) = 3
          IP(4) = 1
        ELSEIF( IN(2) == 1 ) THEN
          IP(1) = 3
          IP(2) = 4
          IP(3) = 1
          IP(4) = 2
        ELSEIF( IN(3) == 1 ) THEN
          IP(1) = 4
          IP(2) = 2
          IP(3) = 1
          IP(4) = 3
        ELSEIF( IN(4) == 1 ) THEN
          IP(1) = 1
          IP(2) = 2
          IP(3) = 3
          IP(4) = 4
        ENDIF

        P1(:,:) = P0(:,:)

        IF( IMODE == 1 ) THEN

          RX(:) = PX(IP(1:3))

          CALL SF_MEAN1(RXM,RX,3)

          IF( RXM <= XG ) THEN

            R(1,1) = P0(IY,IP(1))
            R(2,1) = P0(IZ,IP(1))
            R(1,2) = P0(IY,IP(2))
            R(2,2) = P0(IZ,IP(2))
            R(1,3) = P0(IY,IP(3))
            R(2,3) = P0(IZ,IP(3))

            CALL SF_CUT2(S,R,YG,ZG,EPS)

            SS = SS + S

          ENDIF

        ENDIF

      CASE( 22, 13, 4 )  ! IN

        NT = 1

        P1(:,:) = P0(:,:)

      CASE( 40 )  ! PLANE

        NT = 0

      CASE DEFAULT

        CALL VF_A2ERR('SF_CUT3X','P.G ERROR.')

      END SELECT
        
      END