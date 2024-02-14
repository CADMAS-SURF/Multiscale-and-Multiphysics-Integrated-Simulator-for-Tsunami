      SUBROUTINE PLD_CADMAS(FT,FT2,GRID,IELM,NM,NPFC,IPFC,AFC,IPND,PPND
     &                     ,ITO)

      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION FT(6,*),GRID(3,*),IPFC(10,NPFC),FC(3,8),DIR(3),NOD(8)
     &         ,P(8),AFC(*),IPND(*),PPND(*),FT2(6,*),IELM(NM,*)

      DIR(:) = 0.

      DO I = 1, NPFC

        IF( AFC(I) == 0. ) CYCLE

        ND = IPFC(2,I)
        NOD(1:ND) = IPFC(3:2+ND,I)

        NDP = 0
        PA = 0.
        DO J = 1, ND
          IF( IPND( NOD(J) ) > 0 ) THEN
            NDP = NDP + 1
            PA = PA + PPND( NOD(J) )
          ENDIF
        ENDDO

        IF( NDP == 0 ) THEN
          CYCLE
        ELSEIF( NDP > 0 .AND. NDP < ND ) THEN
          P(1:ND) = PA / DBLE(NDP)
        ELSEIF( NDP == ND ) THEN
          P(1:ND) = PPND( NOD(1:ND) )
        END IF

        SELECT CASE( ND )
        CASE( 3 )
          CALL LOADTR1(GRID,NOD,P,DIR,0,IDUM,DUM,FC,ITO)
        CASE( 6 )
          CALL LOADTR2(GRID,NOD,P,DIR,0,IDUM,DUM,FC,ITO)
        CASE( 4, 8 )
          CALL LOADQU2(GRID,ND,NOD,P,DIR,0,IDUM,DUM,FC,ITO)
        END SELECT

        IE = IPFC(1,I)
        ITYP = IELM(2,IE)

        IF( ITYP == 6 ) THEN
          FT(1:3,NOD(1:ND)) = FT(1:3,NOD(1:ND)) - FC(:,1:ND) * AFC(I)
        ELSE
          FT2(1:3,NOD(1:ND)) = FT2(1:3,NOD(1:ND)) - FC(:,1:ND) * AFC(I)
        ENDIF

      ENDDO

      END
