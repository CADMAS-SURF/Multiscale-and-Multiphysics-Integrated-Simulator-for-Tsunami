      SUBROUTINE SPCDRHVG(FT,ELHM12,ELHM21,ELHM22,UG,PG,KN,ND,IDCR,NCR
     &                   ,INDOF,INDOP)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION FT(*),ELHM12(NCR,ND),ELHM21(ND,NCR),ELHM22(ND,ND)
     &         ,UG(6,*),PG(*),KN(ND),IDCR(2,NCR),INDOF(6,*),INDOP(*)

      DO J = 1, NCR
        JJ = INDOF(IDCR(2,J),IDCR(1,J))
        IF( JJ == -1 ) THEN
          DO I = 1, ND
            II = INDOP(KN(I))
            IF( II > 0 ) 
     &        FT(II) = FT(II) - ELHM21(I,J) * UG(IDCR(2,J),IDCR(1,J))
          ENDDO
        ENDIF
      ENDDO

      DO J = 1, ND
        JJ = INDOP(KN(J))
        IF( JJ == -1 ) THEN
          DO I = 1, NCR
            II = INDOF(IDCR(2,I),IDCR(1,I))
            IF( II > 0 ) FT(II) = FT(II) - ELHM12(I,J) * PG(KN(J))
          ENDDO
        ENDIF
      ENDDO

      DO J = 1, ND
        JJ = INDOP(KN(J))
        IF( JJ == -1 ) THEN
          DO I = 1, ND
            II = INDOP(KN(I))
            IF( II > 0 ) FT(II) = FT(II) - ELHM22(I,J) * PG(KN(J))
          ENDDO
        ENDIF
      ENDDO

      END
