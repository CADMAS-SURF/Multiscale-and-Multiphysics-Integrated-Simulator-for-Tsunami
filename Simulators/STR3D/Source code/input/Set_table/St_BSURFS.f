      SUBROUTINE ST_BSURFS( ICRG, NCRG, ICRGR, NICRG, NNCRG, J_BSRF,
     &                      N_BSRF, IELR, INDGR )
C
      DIMENSION ICRG(2,NICRG), NCRG(4,NNCRG), J_BSRF(5,N_BSRF),
     &          IELR(*), INDGR(*), ICRGR(*)
C
      IP = 0
C
      DO I = 1, NICRG
C
        DO J = 1, N_BSRF
C
          IF( J_BSRF(1,J) == ICRG(1,I) ) THEN
C
            IP = IP + 1
            NCRG(:,IP) = J_BSRF(2:5,J)
C
          ENDIF
C
        ENDDO
C
        ICRG(2,I) = IP
C
      ENDDO
C
      NCRG(1,:) = IELR( NCRG(1,:) )
C
      DO I = 2, 4
        NCRG(I,:) = INDGR( NCRG(I,:) )
      ENDDO
C
      DO I = 1, NICRG
        ICRGR( ICRG(1,I) ) = I
      ENDDO
C
      END