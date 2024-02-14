      SUBROUTINE ELCQUAD(IELC,ICSF,IG5)

      DIMENSION IG(4),ICSF(6),IELC(3,4)

      IG(1:4) = ICSF(3:6)

      IELC(1,1) = IG(1)
      IELC(2,1) = IG(2)
      IELC(3,1) = IG5

      IELC(1,2) = IG(2)
      IELC(2,2) = IG(3)
      IELC(3,2) = IG5

      IELC(1,3) = IG(3)
      IELC(2,3) = IG(4)
      IELC(3,3) = IG5

      IELC(1,4) = IG(4)
      IELC(2,4) = IG(1)
      IELC(3,4) = IG5

      END
