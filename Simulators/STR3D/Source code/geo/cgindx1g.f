      SUBROUTINE CGINDX1G(IDX,NSIZ,MIDX,NIDX,N,IPREV,NEXT,LAST,IDOF,NDOF
     &                   ,IDOP,NDOP,ISLV,ITO)
C
      DIMENSION IDX(NSIZ,MIDX),N(MIDX),IPREV(MIDX),NEXT(MIDX),LAST(*)
     &         ,IDOF(NDOF),IDOP(NDOP)
C
      DO I = 1, NDOF
C
        II = IDOF(I)
C
        IF( II <= 0 ) CYCLE
C
        DO J = 1, NDOP
C
          JJ = IDOP(J)
C
          IF( JJ <= 0 ) CYCLE
C
          IROW = II
C
          DO
C
            NN = N(IROW)
C
            DO K = 1, NN
              IF( JJ < IDX(K,IROW) ) THEN
                CALL REARRANGE(IDX,NSIZ,NIDX,N,IPREV,NEXT,LAST,II,IROW,K
     &                        ,JJ)
                GOTO 10
              ELSEIF( JJ == IDX(K,IROW) ) THEN
                GOTO 10
              ENDIF
            ENDDO
C
            IF( NEXT(IROW) == 0 ) EXIT
C
            IROW = NEXT(IROW)
C
          ENDDO
C
          CALL REARRANGE(IDX,NSIZ,NIDX,N,IPREV,NEXT,LAST,II,IROW,NN+1
     &                  ,JJ)
C
   10     IF( NIDX == MIDX ) THEN
            WRITE(ITO,'(2A)') 'MORE MEMORY IS NECESSARY FOR NEXT '
     &                       ,'PROCESS OF MATRIX SOLVER.'
            CALL ERRSTP(20,ITO)
          ENDIF
C
        ENDDO
C
      ENDDO
C
      DO I = 1, NDOP
C
        II = IDOP(I)
C
        IF( II <= 0 ) CYCLE
C
        DO J = 1, NDOF
C
          JJ = IDOF(J)
C
          IF( JJ <= 0 ) CYCLE
C
          IROW = II
C
          DO
C
            NN = N(IROW)
C
            DO K = 1, NN
              IF( JJ < IDX(K,IROW) ) THEN
                CALL REARRANGE(IDX,NSIZ,NIDX,N,IPREV,NEXT,LAST,II,IROW,K
     &                        ,JJ)
                GOTO 20
              ELSEIF( JJ == IDX(K,IROW) ) THEN
                GOTO 20
              ENDIF
            ENDDO
C
            IF( NEXT(IROW) == 0 ) EXIT
C
            IROW = NEXT(IROW)
C
          ENDDO
C
          CALL REARRANGE(IDX,NSIZ,NIDX,N,IPREV,NEXT,LAST,II,IROW,NN+1
     &                  ,JJ)
C
   20     IF( NIDX == MIDX ) THEN
            WRITE(ITO,'(2A)') 'MORE MEMORY IS NECESSARY FOR NEXT '
     &                       ,'PROCESS OF MATRIX SOLVER.'
            CALL ERRSTP(20,ITO)
          ENDIF
C
        ENDDO
C
      ENDDO
C
      DO I = 1, NDOP
C
        II = IDOP(I)
C
        IF( II <= 0 ) CYCLE
C
        DO J = 1, NDOP
C
          JJ = IDOP(J)
C
          IF( JJ <= 0 ) CYCLE
C
          IF( ISLV == 11 .AND. JJ == II ) CYCLE
C
          IROW = II
C
          DO
C
            NN = N(IROW)
C
            DO K = 1, NN
              IF( JJ < IDX(K,IROW) ) THEN
                CALL REARRANGE(IDX,NSIZ,NIDX,N,IPREV,NEXT,LAST,II,IROW,K
     &                        ,JJ)
                GOTO 30
              ELSEIF( JJ == IDX(K,IROW) ) THEN
                GOTO 30
              ENDIF
            ENDDO
C
            IF( NEXT(IROW) == 0 ) EXIT
C
            IROW = NEXT(IROW)
C
          ENDDO
C
          CALL REARRANGE(IDX,NSIZ,NIDX,N,IPREV,NEXT,LAST,II,IROW,NN+1
     &                  ,JJ)
C
   30     IF( NIDX == MIDX ) THEN
            WRITE(ITO,'(2A)') 'MORE MEMORY IS NECESSARY FOR NEXT '
     &                       ,'PROCESS OF MATRIX SOLVER.'
            CALL ERRSTP(20,ITO)
          ENDIF
C
        ENDDO
C
      ENDDO
C
      END