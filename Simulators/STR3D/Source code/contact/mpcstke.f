      SUBROUTINE MPCSTKE(IDEP,IDX,IRH,IS,NS,IG,IEDQ,IELQ,INDOF,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IDEP(2,3),IDX(2,3),IRH(2,*),IG(2),IELQ(4,*),NDQ(4)
     &         ,INDOF(3)

      IF( INDOF(1) /= 0 .OR. INDOF(2) /= 0 .OR. INDOF(3) /= 0 ) THEN
        WRITE(ITO,'(/X,A)') 'SLAVE POINT IS CONSTRAINED.'
        CALL ERRSTP(50,ITO)
      ENDIF

      IF( IEDQ == 0 ) THEN

        IP = 0

        DO I = 1, 3
          IDEP(1,I) = NS
          IDEP(2,I) = I
          IDX(1,I) = IS + 2 * ( I - 1 )
          IDX(2,I) = IS + 2 * I - 1
          DO J = 1, 2
            IP = IP + 1
            IRH(1,IP) = IG(J)
            IRH(2,IP) = I
          ENDDO
        ENDDO

      ELSE

        NDQ(1)=IG(1)

        IF( IG(1) .EQ. IELQ(1,IEDQ) ) THEN
          NDQ(2)=IELQ(2,IEDQ)
          NDQ(3)=IELQ(3,IEDQ)
          NDQ(4)=IELQ(4,IEDQ)
        ELSEIF( IG(1) .EQ. IELQ(2,IEDQ) ) THEN
          NDQ(2)=IELQ(1,IEDQ)
          NDQ(3)=IELQ(3,IEDQ)
          NDQ(4)=IELQ(4,IEDQ)
        ELSEIF( IG(1) .EQ. IELQ(3,IEDQ) ) THEN
          NDQ(2)=IELQ(1,IEDQ)
          NDQ(3)=IELQ(2,IEDQ)
          NDQ(4)=IELQ(4,IEDQ)
        ELSEIF( IG(1) .EQ. IELQ(4,IEDQ) ) THEN
          NDQ(2)=IELQ(1,IEDQ)
          NDQ(3)=IELQ(2,IEDQ)
          NDQ(4)=IELQ(3,IEDQ)
        ENDIF

        IP = 0

        DO I = 1, 3
          IDEP(1,I) = NS
          IDEP(2,I) = I
          IDX(1,I) = IS + 4 * ( I - 1 )
          IDX(2,I) = IS + 4 * I - 1
          DO J = 1, 4
            IP = IP + 1
            IRH(1,IP) = NDQ(J)
            IRH(2,IP) = I
          ENDDO
        ENDDO

      ENDIF

      END
