      SUBROUTINE CGINDX2(NCGSPC,IDCGWK,N,IPREV,NEXT,LAST,IELM,NM,NELM
     &                  ,NELMC,NELMX,NEQ,NCRMAX,NCGMAX,MIDX,INDOF,INDMPC
     &                  ,MPCF,IMPC,INDOP,ISLV,ITO)
C
      DIMENSION IDCGWK(NCGMAX,MIDX),N(MIDX),IPREV(MIDX),NEXT(MIDX)
     &         ,LAST(NEQ),IELM(NM,NELM+NELMC+NELMX),INDOF(6,*)
     &         ,INDMPC(2,6,*),MPCF(2,*),IDCR(2,NCRMAX),LOCEL(NCRMAX)
     &         ,LOCEP(20),INDOP(*)
C
      IDCGWK(:,:) = 0
C
      NIDX = NEQ
C
      N(:) = 0
      IPREV(:) = 0
      NEXT(:) = 0
C
      DO I = 1, NEQ
        LAST(I) = I
      ENDDO
C
      DO I = 1, NELM + NELMC + NELMX
C
        IF( I > NELM .AND. I <= NELM + NELMC ) CYCLE
C
        ITYP = IELM(2,I)
        ND = IELM(3,I)
C
        IF( ITYP == 1 .AND. ND == 9 ) ND = 8
C
        IF( ITYP == 1 .OR. ITYP == 4 ) THEN
          NDF = 6
        ELSE
          NDF = 3
        ENDIF
C
        CALL IDCRSET(IMPC,IELM(8,I),ND,NDF,INDOF,INDMPC,MPCF,IDCR,NCR)
C
        DO J = 1, NCR
          LOCEL(J) = INDOF( IDCR(2,J), IDCR(1,J) )
        ENDDO
C
        CALL CGINDX1(IDCGWK,NCGMAX,MIDX,NIDX,N,IPREV,NEXT,LAST,LOCEL,NCR
     &              ,ISLV,ITO)
C
        IF( ITYP /= 6 ) CYCLE
C
        LOCEP(1:ND) = INDOP( IELM(8:7+ND,I) )
C
        CALL CGINDX1G(IDCGWK,NCGMAX,MIDX,NIDX,N,IPREV,NEXT,LAST,LOCEL
     &               ,NCR,LOCEP,ND,ISLV,ITO)
C
      ENDDO
C
      IF( ISLV <= 11 ) THEN
        NCGSPC = NEQ
      ELSE
        NCGSPC = 0
      ENDIF
C
      DO I = 1, NIDX
        NCGSPC = NCGSPC + N(I)
      ENDDO
C
      END