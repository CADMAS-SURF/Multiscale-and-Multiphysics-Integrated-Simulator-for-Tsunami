      SUBROUTINE SF_MDLNDH(LNDH,XX,YY,ZZ,IELM,GRID)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
     &         ,IELM(24,NELM),GRID(3,NNOD),LNDH(NUMI,NUMJ)

      DATA EPS / 1.D-1 /

      DO IEL = 1, NELM

        IF( IELM(3,IEL) /= 2 ) CYCLE

        DO 50 L = 1, IELM(4,IEL)

          IG = IELM(4+L,IEL)

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

                  DO K = 2, NUMK - 1

                    IF( K == 2 ) THEN
                      ZS = ZZ(1,K) - ZZ(2,K) * EPS
                    ELSE
                      ZS = ZZ(1,K)
                    ENDIF

                    IF( K == NUMK - 1 ) THEN
                      ZE = ZZ(1,K+1) + ZZ(2,K) * EPS
                    ELSE
                      ZE = ZZ(1,K+1)
                    ENDIF

                    IF( GRID(3,IG) >= ZS .AND. GRID(3,IG) < ZE ) THEN
                      LNDH(I,J) = MAX0( K, LNDH(I,J) )
                      GOTO 50
                    ENDIF

                  ENDDO

                ENDIF

              ENDDO

            ENDIF

          ENDDO

   50   CONTINUE

      ENDDO

      END