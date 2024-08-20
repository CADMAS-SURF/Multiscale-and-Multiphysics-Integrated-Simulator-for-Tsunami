      SUBROUTINE SF_RFACE2()

      USE VF_A2ARRAY
      USE SF_ARRAY

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      DATA EPS / 1.D-1 /

      IF( IPART == 0 ) THEN
        IF( MYRANK == 0 ) IRGRID(:) = IRGRID0(:)
        CALL VF_P1BCSI(IRGRID,NNOD,0)
      ELSEIF( IPART == 1 ) THEN
        CALL SF_BCAST_I(IRGRID,IGNO,NNOD,IRGRID0,NNOD0,1)
      ENDIF

      DO 10 IG = 1, NNOD

        IF( IRGRID(IG) <= 0 ) CYCLE

        DO I = MYIS, MYIE

          IF( I == MYIS .AND. MYMIS == 1 ) THEN
            XS = XX(1,I) - XX(2,I) * EPS
          ELSE
            XS = XX(1,I)
          ENDIF

          IF( I == MYIE .AND. MYMIE == 1 ) THEN
            XE = XX(1,I+1) + XX(2,I) * EPS
          ELSE
            XE = XX(1,I+1)
          ENDIF

          IF( GRID(1,IG) >= XS .AND. GRID(1,IG) < XE ) THEN

            DO J = MYJS, MYJE

              IF( J == MYJS .AND. MYMJS == 1 ) THEN
                YS = YY(1,J) - YY(2,J) * EPS
              ELSE
                YS = YY(1,J)
              ENDIF

              IF( J == MYJE .AND. MYMJE == 1 ) THEN
                YE = YY(1,J+1) + YY(2,J) * EPS
              ELSE
                YE = YY(1,J+1)
              ENDIF

              IF( GRID(2,IG) >= YS .AND. GRID(2,IG) < YE ) THEN

                IRGRID(IG) = I + NUMI*(J-1)  ! 節点が属するセル

                GOTO 10

              ENDIF

            ENDDO

          ENDIF

        ENDDO

        IRGRID(IG) = 0  ! 領域外

   10 CONTINUE

      END