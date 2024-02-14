      SUBROUTINE EDGQUAD(IEDG,IELC,IP)

      DIMENSION IEDG(6,4),IELC(3,4)

      IEDG(1:2,:) = IELC(2:3,:)

      IEDG(4,:) = 1
      IEDG(6,:) = 2

      IEDG(3,1) = IP
      IEDG(5,1) = IP + 1

      IEDG(3,2) = IP + 1
      IEDG(5,2) = IP + 2

      IEDG(3,3) = IP + 2
      IEDG(5,3) = IP + 3

      IEDG(3,4) = IP + 3
      IEDG(5,4) = IP

      END
