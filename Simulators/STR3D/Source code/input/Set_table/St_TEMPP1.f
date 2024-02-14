      SUBROUTINE ST_TEMPP1( ITP1, ITPF, TMP1, NITP1, J_TMP1, R_TMP1,
     &                      N_TMP1, IELR, NELM )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION ITP1(NITP1), ITPF(NELM,NITP1), TMP1(2,NELM,NITP1),
     &          J_TMP1(4,N_TMP1), R_TMP1(2,N_TMP1), IELR(*)
C
      N = 0
C
      DO 10 I = 1, N_TMP1
        DO J = 1, N
          IF( J_TMP1(1,I) == ITP1(J) ) GOTO 10
        ENDDO
        N = N + 1
        ITP1(N) = J_TMP1(1,I)
   10 CONTINUE
C
      DO I = 1, NITP1
        DO J = 1, N_TMP1
          IF( J_TMP1(1,J) == ITP1(I) ) THEN
            IS = J_TMP1(2,J)
            IE = J_TMP1(3,J)
            DO K = IS, IE
              IEL = IELR(K)
              IF( IEL == 0 ) CYCLE
              ITPF(IEL,I) = J_TMP1(4,J)
              TMP1(:,IEL,I) = R_TMP1(:,J)
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C
      END