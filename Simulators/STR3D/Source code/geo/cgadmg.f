      SUBROUTINE CGADMG(STF,ELHM12,ELHM21,ELHM22,KN,ND,IDCR,NCR,INDOF
     &                 ,INDOP,IDSK,IDCG,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION INDOF(6,*),IDCR(2,NCR),INDOP(*),KN(ND),IDSK(*),IDCG(*)
     &         ,STF(*),ELHM12(NCR,ND),ELHM21(ND,NCR),ELHM22(ND,ND)

      DO I = 1, NCR
        II = INDOF(IDCR(2,I),IDCR(1,I))
        IF( II <= 0 ) CYCLE
        DO J = 1, ND
          JJ = INDOP(KN(J))
          IF( JJ <= 0 ) CYCLE
          DO K = IDSK(II), IDSK(II+1) - 1
            IF( IDCG(K) == JJ ) THEN
              STF(K) = STF(K) + ELHM12(I,J)
              GOTO 10
            ENDIF
          ENDDO
          GOTO 90
   10   ENDDO
      ENDDO

      DO I = 1, ND
        II = INDOP(KN(I))
        IF( II <= 0 ) CYCLE
        DO J = 1, NCR
          JJ = INDOF(IDCR(2,J),IDCR(1,J))
          IF( JJ <= 0 ) CYCLE
          DO K = IDSK(II), IDSK(II+1) - 1
            IF( IDCG(K) == JJ ) THEN
              STF(K) = STF(K) + ELHM21(I,J)
              GOTO 20
            ENDIF
          ENDDO
          GOTO 90
   20   ENDDO
      ENDDO

      DO I = 1, ND
        II = INDOP(KN(I))
        IF( II <= 0 ) CYCLE
        DO J = 1, ND
          JJ = INDOP(KN(J))
          IF( JJ <= 0 ) CYCLE
          DO K = IDSK(II), IDSK(II+1) - 1
            IF( IDCG(K) == JJ ) THEN
              STF(K) = STF(K) + ELHM22(I,J)
              GOTO 30
            ENDIF
          ENDDO
          GOTO 90
   30   ENDDO
      ENDDO

      RETURN

   90 WRITE(ITO,*) 'NO ADRESS FOR [K], PROGRAM STOPPED.'
      CALL ERRSTP(90,ITO)

      END
