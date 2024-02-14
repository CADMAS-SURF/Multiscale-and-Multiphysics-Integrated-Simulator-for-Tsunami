      SUBROUTINE EX_SPC(NISPCW,NNSPCW,ISPCW,NSPCW,SPCW,NISPC,ISPC,NSPC
     &                 ,SPC,LNOD,NNODW,N)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ISPC(2,NISPC),NSPC(N,*),SPC(6,*),ISPCW(2,*),NSPCW(N,*)
     &         ,SPCW(6,*),LNOD(*)

      IP = 0
      JP = 0

      DO I = 1, NISPC
        JP0 = JP
        CALL ADDSET3(JS,JE,ISPC,I)
        DO J = JS, JE
          LN = LNOD(NSPC(1,J))
          IF( LN > 0 .AND. LN <= NNODW ) THEN
            JP = JP + 1
            NSPCW(1,JP) = LN
            NSPCW(2:,JP) = NSPC(2:,J)
            SPCW(:,JP) = SPC(:,J)
          ENDIF
        ENDDO
        IF( JP > JP0 ) THEN
          IP = IP + 1
          ISPCW(1,IP) = ISPC(1,I)
          ISPCW(2,IP) = JP
        ENDIF
      ENDDO

      NISPCW = IP
      NNSPCW = JP

      END
