      SUBROUTINE ST_ELEM( IELM, NM, NELM, THK, RODA, BARD, BVEC, JELS,
     &                    PELS, IELR, GRID, NNOD, AMAT, J_PSHEL,
     &                    R_PSHEL, N_PSHEL, J_PSOL, N_PSOL, J_PROD,
     &                    R_PROD, N_PROD, J_PBAR, R_PBAR, N_PBAR,
     &                    J_PELS, R_PELS, N_PELS, J_SHEL, N_SHEL, J_SOL,
     &                    N_SOL, J_ROD, N_ROD, J_BAR, R_BAR, N_BAR,
     &                    J_ELS, R_ELS, N_ELS, ICRDR, INDGR, IMATR,
     &                    ISOL, ITO )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION IELM(NM,NELM), IELR(*), THK(N_PSHEL), RODA(N_PROD),
     &          BARD(6,N_PBAR), BVEC(3,N_BAR), JELS(2,N_ELS), 
     &          PELS(3,N_ELS), J_PSHEL(2,N_PSHEL), R_PSHEL(N_PSHEL),
     &          J_PSOL(3,N_PSOL), J_PROD(2,N_PROD), R_PROD(N_PROD),
     &          J_PBAR(3,N_PBAR), R_PBAR(6,N_PBAR), J_PELS(N_PELS),
     &          R_PELS(3,N_PELS), J_SHEL(10,N_SHEL), J_SOL(22,N_SOL),
     &          J_ROD(4,N_ROD), J_BAR(4,N_BAR), R_BAR(3,N_BAR),
     &          J_ELS(6,N_ELS), R_ELS(3,N_ELS), ICRDR(*), INDGR(*),
     &          IMATR(*), GRID(3,NNOD+N_SHEL),AMAT(33,*)
C
C     --- IELM ---
C
      IE = 0
C
      DO 10 I = 1, N_SHEL
C
        IE = IE + 1
C
        IELM(1,IE) = J_SHEL(1,I)
C
        IELM(2,IE) = 1
C
        DO J = 1, 8
          IG = J_SHEL(2+J,I)
          IF( IG == 0 ) EXIT
          IELM(7+J,IE) = IG
        ENDDO
C
        IELM(3,IE) = J - 1
C
        DO J = 1, N_PSHEL
          IF( J_SHEL(2,I) == J_PSHEL(1,J) )  THEN
            IELM(4,IE) = J_PSHEL(2,J)
            IELM(5,IE) = J
            GOTO 10
          ENDIF
        ENDDO
C
        WRITE(ITO,*) 'SHELL PID ERROR, STOP IN SUB. ST_ELEM.'
        CALL ERRSTP(90,ITO)
C
   10 CONTINUE
C
      DO 20 I = 1, N_SOL
C
        IE = IE + 1
C
        IELM(1,IE) = J_SOL(1,I)
C
        IELM(2,IE) = 2
C
        DO J = 1, 20
          IG = J_SOL(2+J,I)
          IF( IG == 0 ) EXIT
          IELM(7+J,IE) = IG
        ENDDO
C
        IELM(3,IE) = J - 1
C
        DO J = 1, N_PSOL
          IF( J_SOL(2,I) == J_PSOL(1,J) )  THEN
            IELM(4,IE) = J_PSOL(2,J)
            IELM(6,IE) = J_PSOL(3,J)
            GOTO 20
          ENDIF
        ENDDO
C
        WRITE(ITO,*) 'SOLID PID ERROR, STOP IN SUB. ST_ELEM.'
        CALL ERRSTP(90,ITO)
C
   20 CONTINUE
C
      DO 30 I = 1, N_ROD
C
        IE = IE + 1
C
        IELM(1,IE) = J_ROD(1,I)
C
        IELM(2,IE) = 3
C
        IELM(3,IE) = 2
C
        IELM(8:9,IE) = J_ROD(3:4,I)
C
        DO J = 1, N_PROD
          IF( J_ROD(2,I) == J_PROD(1,J) )  THEN
            IELM(4,IE) = J_PROD(2,J)
            IELM(5,IE) = J
            GOTO 30
          ENDIF
        ENDDO
C
        WRITE(ITO,*) 'ROD PID ERROR, STOP IN SUB. ST_ELEM.'
        CALL ERRSTP(90,ITO)
C
   30 CONTINUE
C
      DO 40 I = 1, N_BAR
C
        IE = IE + 1
C
        IELM(1,IE) = J_BAR(1,I)
C
        IELM(2,IE) = 4
C
        IELM(3,IE) = 2
C
        IELM(8:9,IE) = J_BAR(3:4,I)
C
        DO J = 1, N_PBAR
          IF( J_BAR(2,I) == J_PBAR(1,J) )  THEN
            IELM(4,IE) = J_PBAR(2,J)
            IELM(5,IE) = J
            IELM(6,IE) = J_PBAR(3,J)
            GOTO 41
          ENDIF
        ENDDO
C
        WRITE(ITO,*) 'BAR PID ERROR, STOP IN SUB. ST_ELEM.'
        CALL ERRSTP(90,ITO)
C
   41   IELM(7,IE) = I
C
   40 CONTINUE
C
      DO 50 I = 1, N_ELS
C
        IE = IE + 1
C
        IELM(1,IE) = J_ELS(1,I)
C
        IELM(2,IE) = 5
C
        IF( J_ELS(3,I) > 0 .AND. J_ELS(5,I) > 0 ) THEN
          IELM(3,IE) = 2
          IELM(8,IE) = J_ELS(3,I)
          IELM(9,IE) = J_ELS(5,I)
        ELSEIF( J_ELS(3,I) > 0 .AND. J_ELS(5,I) == 0 ) THEN
          IELM(3,IE) = 1
          IELM(8,IE) = J_ELS(3,I)
        ELSEIF( J_ELS(3,I) == 0 .AND. J_ELS(5,I) > 0 ) THEN
          IELM(3,IE) = 1
          IELM(8,IE) = J_ELS(5,I)
        ELSE
          WRITE(ITO,*) 'ELAS GRID NO. ERROR, STOP IN SUB. ST_ELEM.'
          CALL ERRSTP(90,ITO)
        ENDIF
C
        IELM(5,IE) = I
C
   50 CONTINUE
C
C     --- IELR ---
C
      DO I = 1, NELM
        IELR( IELM(1,I) ) = I
      ENDDO
C
C     --- EXTERNAL NO. -> INTERNAL NO. ---
C
      DO I = 1, NELM
C
        ITYP = IELM(2,I)
        ND = IELM(3,I)
C
        IF( ITYP == 2 .AND. IELM(4,I) >= 100 ) IELM(2,I) = 6
C
        IF( ITYP <= 4 ) IELM(4,I) = IMATR( IELM(4,I) )
C
        IMAT = IELM(4,I)
C
        IF( IELM(2,I) == 2 .AND. AMAT(6,IMAT) > 0. ) IELM(5,I) = 1
C
        IF( ITYP == 2 .AND. IELM(6,I) > 0 ) 
     &    IELM(6,I) = ICRDR( IELM(6,I) )
C
        IELM(8:7+ND,I) = INDGR( IELM(8:7+ND,I) )
C
      ENDDO
C
C     --- FOR MITC9 ELEMENT ---
C
      DO I = 1, N_SHEL
C
        IF( IELM(3,I) == 8 .AND. ISOL /= 1 ) THEN
C
          IELM(3,I) = 9

          IG = NNOD + I

          IELM(7+9,I) = IG
C
          GRID(:,IG) = 0.
          DO J = 1, 8
            JG = IELM(7+J,I)
            IF( J <= 4 ) THEN
              H = -.25D0
            ELSE
              H = .5D0
            ENDIF
            GRID(:,IG) = GRID(:,IG) + H*GRID(:,JG)
          ENDDO
C
        ENDIF
C
      ENDDO
C
C     --- THK ---
C
      THK(:) = R_PSHEL(:)
C
C     --- RODA ---
C
      RODA(:) = R_PROD(:)
C
C     --- BARD ---
C
      BARD(:,:) = R_PBAR(:,:)
C
C     --- BVEC ---
C
      BVEC(:,:) = R_BAR(:,:)
C
C     --- JELS, PELS ---
C
      DO 70 I = 1, N_ELS
C
        IF( J_ELS(4,I) > 0 .AND. J_ELS(6,I) > 0 ) THEN
          JELS(1,I) = J_ELS(4,I)
          JELS(2,I) = J_ELS(6,I)
        ELSEIF( J_ELS(4,I) > 0 .AND. J_ELS(6,I) == 0 ) THEN
          JELS(1,I) = J_ELS(4,I)
        ELSEIF( J_ELS(4,I) == 0 .AND. J_ELS(6,I) > 0 ) THEN
          JELS(1,I) = J_ELS(6,I)
        ELSE
          WRITE(ITO,*) 'ELAS COMPONENT NO. ERROR, STOP IN SUB. ST_ELEM.'
          CALL ERRSTP(90,ITO)
        ENDIF
C
        IF( J_ELS(2,I) == 0 ) THEN
          PELS(:,I) = R_ELS(:,I)
        ELSE
          DO J = 1, N_PELS
            IF( J_ELS(2,I) == J_PELS(J) ) THEN
              PELS(:,I) = R_PELS(:,J)
              GOTO 70
            ENDIF
          ENDDO
          WRITE(ITO,*) 'ELAS PID ERROR, STOP IN SUB. ST_ELEM.'
          CALL ERRSTP(90,ITO)
        ENDIF
C
   70 CONTINUE
C
      END