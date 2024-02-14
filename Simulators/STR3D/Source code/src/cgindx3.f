      SUBROUTINE CGINDX3(IDSK,IDCG,IDCGWK,NCGMAX,N,NEXT,NEQ,ISLV)
C
      DIMENSION IDSK(NEQ+1),IDCG(*),IDCGWK(NCGMAX,*),NEXT(*),N(*)
C
      NCG = 0
C
      DO I = 1, NEQ
C
        IF( ISLV <= 11 ) THEN
          NCG = NCG + 1
          IDSK(I) = NCG
          IDCG(NCG) = I
        ELSE
          IDSK(I) = NCG + 1
        ENDIF
C
        IP = I
C
        DO
C
          NN = N(IP)
C
          IDCG(NCG+1:NCG+NN) = IDCGWK(1:NN,IP)
C
          NCG = NCG + NN
C
          IP = NEXT(IP)
          IF( IP == 0 ) EXIT
C
        ENDDO
C
      ENDDO
C
      IDSK(NEQ+1) = NCG + 1
C
      END