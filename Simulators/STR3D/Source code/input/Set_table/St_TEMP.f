      SUBROUTINE ST_TEMP( ITMP, TEMP, NITMP, J_TMPD, R_TMPD, N_TMPD,
     &                    J_TMP, R_TMP, N_TMP, INDGR, NNOD )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION ITMP(NITMP), TEMP(NNOD,NITMP), J_TMPD(N_TMPD),
     &          R_TMPD(N_TMPD), J_TMP(2,N_TMP), R_TMP(N_TMP), INDGR(*)
C
      ITMP(1:N_TMPD) = J_TMPD(:)
C
      N = N_TMPD
C
      DO 10 I = 1, N_TMP
        DO J = 1, N
          IF( J_TMP(1,I) == ITMP(J) ) GOTO 10
        ENDDO
        N = N + 1
        ITMP(N) = J_TMP(1,I)
   10 CONTINUE
C
      DO I = 1, N_TMPD
        TEMP(:,I) = R_TMPD(I)
      ENDDO
C
      DO I = 1, NITMP
        DO J = 1, N_TMP
          IF( J_TMP(1,J) == ITMP(I) ) THEN
            IG = INDGR( J_TMP(2,J) )
            TEMP(IG,I) = R_TMP(J)
          ENDIF
        ENDDO
      ENDDO
C
      END