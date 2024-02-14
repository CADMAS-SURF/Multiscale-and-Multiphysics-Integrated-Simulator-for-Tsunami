      SUBROUTINE MPCSTKF(IDEP,IDX,IRH,IS,NS,IELC,IFCQ,IELQ,INDOF,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IDEP(2,3),IDX(2,3),IRH(2,*),IELC(3),IELQ(4,*),NDQ(4)
     &         ,INDOF(3)

      IF( INDOF(1) /= 0 .OR. INDOF(2) /= 0 .OR. INDOF(3) /= 0 ) THEN
        WRITE(ITO,'(/X,A)') 'SLAVE POINT IS CONSTRAINED.'
        CALL ERRSTP(50,ITO)
      ENDIF

      IF( IFCQ == 0 ) THEN

        IP = 0

        DO I = 1, 3
          IDEP(1,I) = NS
          IDEP(2,I) = I
          IDX(1,I) = IS + 3 * ( I - 1 )
          IDX(2,I) = IS + 3 * I - 1
          DO J = 1, 3
            IP = IP + 1
            IRH(1,IP) = IELC(J)
            IRH(2,IP) = I
          ENDDO
        ENDDO

      ELSE

        NDQ(1) = IELC(1)
        NDQ(2) = IELC(2)

        IF( IELC(1) == IELQ(1,IFCQ) ) THEN
          NDQ(3) = IELQ(3,IFCQ)
          NDQ(4) = IELQ(4,IFCQ)
        ELSEIF( IELC(1) == IELQ(2,IFCQ) ) THEN
          NDQ(3) = IELQ(1,IFCQ)
          NDQ(4) = IELQ(4,IFCQ)
        ELSEIF( IELC(1) == IELQ(3,IFCQ) ) THEN
          NDQ(3) = IELQ(1,IFCQ)
          NDQ(4) = IELQ(2,IFCQ)
        ELSEIF( IELC(1) == IELQ(4,IFCQ) ) THEN
          NDQ(3) = IELQ(2,IFCQ)
          NDQ(4) = IELQ(3,IFCQ)
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
