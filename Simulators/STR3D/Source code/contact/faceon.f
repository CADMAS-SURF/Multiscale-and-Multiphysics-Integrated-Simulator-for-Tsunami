      SUBROUTINE FACEON(ISLV,RSLV,POSS,MA,IELC,IEDG,ICELA,ICEL,IBTE
     &                 ,ICFCA,ICFC,POS,THETA)

      IMPLICIT REAL*8(A-H,O-Z)
      LOGICAL AREAIN,VOLIN3
      DIMENSION IG(2),X0(3,3),X(3,4),RN0(3),RN(3),RL(4),POSS(3),POS(3,*)
     &         ,IEDG(6,*),IELC(3,*),ICFCA(2,*),ICFC(*),ISLV(2),RSLV(3)
     &         ,ICELA(2,*),ICEL(*),IBTE(4,*)

      IST = ISLV(1)

      X0(:,:) = POS(:,IELC(:,MA))
      CALL NRMVEC(RN0,X0)

      X(:,4) = POSS(:)

      SELECT CASE( IST )
      CASE( 11 )
        N = 1
        IG(1) = ISLV(2)
      CASE( 12 )
        N = 2
        IG(:) = IEDG(1:2,ISLV(2))
      END SELECT

      DO I = 1, N

        JS = ICFCA(1,IG(I))
        JE = ICFCA(2,IG(I))

        DO J = JS, JE

          IFC = ICFC(J)

          IF( IFC == MA ) CYCLE

          X(:,1:3) = POS(:,IELC(:,IFC))

          CALL AREACD(RL,RN,H,X)

          IF( DOT_PRODUCT(RN0,RN) > DCOS(THETA) .AND.
     &        AREAIN(RL,1.D-10) ) THEN

            ISLV(1) = 13
            ISLV(2) = IFC

            SELECT CASE( IST )
            CASE( 11 )
              X(:,4) = POS(:,IG(1))
            CASE( 12 )
              T = RSLV(1)
              X(:,4) = ( 1.D0 - T ) * POS(:,IG(1)) + T * POS(:,IG(2))
            END SELECT

            CALL AREACD(RL,RN,H,X)

            RSLV(:) = RL(1:3)

            GOTO 10

          ENDIF

        ENDDO

      ENDDO

   10 IF( ISLV(1) == 13 ) RETURN

      DO I = 1, N

        JS = ICELA(1,IG(I))
        JE = ICELA(2,IG(I))

        DO J = JS, JE

          K = ICEL(J)

          X(:,:) = POS(:,IBTE(:,K))

          CALL VOLCD(RL,X,POSS)

          IF( VOLIN3(RL,1.D-10) ) RETURN

        ENDDO

      ENDDO

      ISLV(:) = 0
      RSLV(:) = 0.

      END
