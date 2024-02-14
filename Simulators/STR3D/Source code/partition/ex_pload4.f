      SUBROUTINE EX_PLOAD4(NIPL4W,NNPL4W,IPL4W,NPL4W,PLD4W,NIPL4,IPL4
     &                    ,NPL4,PLD4,LELM,LNOD)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IPL4(2,NIPL4),NPL4(4,*),PLD4(4,*),IPL4W(2,*),NPL4W(4,*)
     &         ,PLD4W(4,*),LELM(*),LNOD(*)

      IP = 0
      JP = 0

      DO I = 1, NIPL4
        JP0 = JP
        CALL ADDSET3(JS,JE,IPL4,I)
        DO J = JS, JE
          LE = LELM(NPL4(1,J))
          IF( LE > 0 ) THEN
            JP = JP + 1
            NPL4W(1,JP) = LE
            NPL4W(2:3,JP) = 0
            IF( NPL4(2,J) > 0 ) NPL4W(2,JP) = LNOD(NPL4(2,J))
            IF( NPL4(3,J) > 0 ) NPL4W(3,JP) = LNOD(NPL4(3,J))
            NPL4W(4,JP) = NPL4(4,J)
            PLD4W(:,JP) = PLD4(:,J)
          ENDIF
        ENDDO
        IF( JP > JP0 ) THEN
          IP = IP + 1
          IPL4W(1,IP) = IPL4(1,I)
          IPL4W(2,IP) = JP
        ENDIF
      ENDDO

      NIPL4W = IP
      NNPL4W = JP

      END
