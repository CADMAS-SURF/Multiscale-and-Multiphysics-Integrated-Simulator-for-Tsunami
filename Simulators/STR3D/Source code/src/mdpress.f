      SUBROUTINE MDPRESS(IPND,PRESS,NNOD,NIELG,GRID,POS,PG,IELC,GELC
     &                  ,IELG,TIM,WLEVEL,ALEVEL,ISEQ)

      IMPLICIT REAL*8 (A-H,O-Z)
      LOGICAL AREAIN
      DIMENSION IPND(NNOD),PRESS(NNOD),POS(3,*),PG(*),IELC(3,*),GELC(*)
     &         ,IELG(NIELG),X(3,4),XG(3),RL(3),RN(3),GRID(3,NNOD)
      DATA G /9.8D0/

      DO I = 1, NNOD

        IF( IPND(I) >= 0 ) THEN
          IF( IPND(I) == 2 ) THEN
            IF( ISEQ == 0 .AND. TIM == 0.D0 ) THEN
              PRESS(I) = ( WLEVEL - GRID(3,I) ) * G * 1.D3
     &                 + ( ALEVEL - WLEVEL ) * G
            ELSE
              PRESS(I) = PG(I)
            ENDIF
          ENDIF
          CYCLE
        ENDIF

        RL_MIN = -1.D0

        X(:,4) = POS(:,I)

        DO J = 1, NIELG

          JP = IELG(J)

          X(:,1:3) = POS(:,IELC(:,JP))

          CALL MEAN3(XG,X,3,3)

          CALL LENGTH(DIST,X(1,4),XG,3)

          IF( DIST > 2.D0*GELC(JP) ) CYCLE

          CALL AREACD(RL,RN,H,X)

          IF( DABS(H) >= GELC(JP) ) CYCLE

          IF( AREAIN(RL,1.D-2) ) THEN
            IPND(I) = 2
            IF( ISEQ == 0 .AND. TIM == 0.D0 ) THEN
              PRESS(I) = ( WLEVEL - GRID(3,I) ) * G * 1.D3
     &                 + ( ALEVEL - WLEVEL ) * G
            ELSE
              PRESS(I) = RL(1) * PG( IELC(1,JP) )
     &                 + RL(2) * PG( IELC(2,JP) )
     &                 + RL(3) * PG( IELC(3,JP) )
            ENDIF
            EXIT
          ELSEIF( AREAIN(RL,1.D-1) ) THEN
            IF( MINVAL(RL) > RL_MIN ) THEN
              RL_MIN = MINVAL(RL)
              IPND(I) = 2
              IF( ISEQ == 0 .AND. TIM == 0.D0 ) THEN
                PRESS(I) = ( WLEVEL - GRID(3,I) ) * G * 1.D3
     &                   + ( ALEVEL - WLEVEL ) * G
              ELSE
                PRESS(I) = RL(1) * PG( IELC(1,JP) )
     &                   + RL(2) * PG( IELC(2,JP) )
     &                   + RL(3) * PG( IELC(3,JP) )
              ENDIF
            ENDIF
          ENDIF

        ENDDO

      ENDDO

      END
