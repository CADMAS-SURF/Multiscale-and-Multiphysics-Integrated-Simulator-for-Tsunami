      SUBROUTINE OUTNODW( UG, PPND, NNOD, NMAT, NPFC, NICRG, NIGSF,
     &                    N_PART, NG_ENSW, IG_ENSW, IFL )
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IFL(*), UG(6,NNOD+NIGSF), NG_ENSW(NMAT+NICRG),
     &          IG_ENSW(NNOD+NIGSF,NMAT+NICRG), PPND(NNOD)
C
      IF( NPFC > 0 .OR. NICRG > 0 ) WRITE(IFL(20),'(A)') 'displacement'
C
      IP = 0
C
      IF( NPFC > 0 ) THEN
C
        DO K = 1, NMAT
C
          IP = IP + 1
C
          WRITE(IFL(20),'(A)') 'part'
          WRITE(IFL(20),'(I10)') N_PART + IP
          WRITE(IFL(20),'(A)') 'coordinates'
C
          DO J = 1, 3
            DO I = 1, NG_ENSW(IP)
              IG = IG_ENSW(I,IP)
              WRITE(IFL(20),'(1PE12.4)') UG(J,IG)
            ENDDO
          ENDDO
C
        ENDDO
C
      ENDIF
C
      IF( NICRG > 0 ) THEN
C
        DO K = 1, NICRG
C
          IP = IP + 1
C
          WRITE(IFL(20),'(A)') 'part'
          WRITE(IFL(20),'(I10)') N_PART + IP
          WRITE(IFL(20),'(A)') 'coordinates'
C
          DO J = 1, 3
            DO I = 1, NG_ENSW(IP)
              IG = IG_ENSW(I,IP)
              WRITE(IFL(20),'(1PE12.4)') UG(J,IG)
            ENDDO
          ENDDO
C
        ENDDO
C
      ENDIF
C
      IF( NPFC > 0 ) THEN
C
        WRITE(IFL(21),'(A)') 'surface pressure'
C
        IP = 0
C
        DO K = 1, NMAT
C
          IP = IP + 1
C
          WRITE(IFL(21),'(A)') 'part'
          WRITE(IFL(21),'(I10)') N_PART + IP
          WRITE(IFL(21),'(A)') 'coordinates'
C
          DO I = 1, NG_ENSW(IP)
            IG = IG_ENSW(I,IP)
            WRITE(IFL(21),'(1PE12.4)') PPND(IG)
          ENDDO
C
        ENDDO
C
      ENDIF
C
      END
