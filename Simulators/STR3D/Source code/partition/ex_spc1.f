      SUBROUTINE EX_SPC1(NISP1W,NNSP1W,ISP1W,NSP1W,NISP1,ISP1,NSP1,LNOD
     &                  ,NNODW)

      DIMENSION ISP1(2,NISP1),NSP1(7,*),ISP1W(2,*),NSP1W(7,*),LNOD(*)

      IP = 0
      JP = 0

      DO I = 1, NISP1
        JP0 = JP
        CALL ADDSET3(JS,JE,ISP1,I)
        DO J = JS, JE
          LN = LNOD(NSP1(1,J))
          IF( LN > 0 .AND. LN <= NNODW ) THEN
            JP = JP + 1
            NSP1W(1,JP) = LN
            NSP1W(2:7,JP) = NSP1(2:7,J)
          ENDIF
        ENDDO
        IF( JP > JP0 ) THEN
          IP = IP + 1
          ISP1W(1,IP) = ISP1(1,I)
          ISP1W(2,IP) = JP
        ENDIF
      ENDDO

      NISP1W = IP
      NNSP1W = JP

      END
