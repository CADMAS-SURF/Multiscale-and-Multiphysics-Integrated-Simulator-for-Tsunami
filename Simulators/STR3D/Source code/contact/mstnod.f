      SUBROUTINE MSTNOD(N,IG,IST,MA,IELC,IEDG,IELQ,IFCQ,IEDQ,IVRQ)

      DIMENSION IG(*),IEDG(6,*),IELC(3,*),IELQ(4,*),IVRQ(*),IEDQ(*)
     &         ,IFCQ(*)

      SELECT CASE( IST )
      CASE( 1, 11 )
        IF( IVRQ(MA) > 0 ) THEN
          N = 4
          IG(1:4) = IELQ(:,IVRQ(MA))
        ELSE
          N = 1
          IG(1) = MA
        ENDIF
      CASE( 2, 12, 14 )
        IF( IEDQ(MA) > 0 ) THEN
          N = 4
          IG(1:4) = IELQ(:,IEDQ(MA))
        ELSE
          N = 2
          IG(1:2) = IEDG(1:2,MA)
        ENDIF
      CASE( 3, 13, 15 )
        IF( IFCQ(MA) > 0 ) THEN
          N = 4
          IG(1:4) = IELQ(:,IFCQ(MA))
        ELSE
          N = 3
          IG(1:3) = IELC(:,MA)
        ENDIF
      END SELECT

      END
