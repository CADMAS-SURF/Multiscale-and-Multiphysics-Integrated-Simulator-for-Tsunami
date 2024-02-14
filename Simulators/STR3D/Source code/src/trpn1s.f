      SUBROUTINE TRPN1S(GQD,X,ITO)
C
C     GQD : OUT : [E] (γyz,γzxの変換行列)
C     X   : IN  : 節点座標
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(3,6),V23(2),V31(2),G(6,6),A(3,3),AINV(3,3),B(3,3)
     &         ,BAINV(3,3),Q(6,3),D(3,6),GQ(6,3),GQD(6,6)
C
      X1 = 0.
      Y1 = 0.
      X2 = ( X(1,2) + X(1,5) ) * .5D0
      Y2 = 0.
      X3 = ( X(1,3) + X(1,6) ) * .5D0
      Y3 = ( X(2,3) + X(2,6) ) * .5D0
C
      V23(1)=X3-X2
      V23(2)=Y3-Y2
C
      V31(1)=X1-X3
      V31(2)=Y1-Y3
C
      CALL DIRCOS(V23,V23,2)
      CALL DIRCOS(V31,V31,2)
C
      COS2=V23(1)
      SIN2=V23(2)
      COS3=V31(1)
      SIN3=V31(2)
C
      G(:,:)=0.
      G(1,4)=1.D0
      G(2,2)=SIN2
      G(2,5)=COS2
      G(3,3)=SIN3
      G(3,6)=COS3
      G(4,1)=1.D0
      G(5,2)=COS2
      G(5,5)=-SIN2
      G(6,3)=COS3
      G(6,6)=-SIN3
C
      A(:,:)=0.
      A(1,2)=1.D0
      A(1,3)=1.D0
      A(2,1)=SIN2
      A(2,2)=COS2
      A(2,3)=1.D0
      A(3,1)=SIN3
      A(3,2)=COS3
      A(3,3)=1.D0
      CALL INV3(A,AINV,DET,ITO)
C
      B(:,:)=0.
      B(1,1)=1.D0
      B(2,1)=COS2
      B(2,2)=-SIN2
      B(3,1)=COS3
      B(3,2)=-SIN3
C
      CALL AXB(BAINV,B,AINV,3,3,3)
C
      Q(:,:)=0.
      Q(1,1)=1.D0
      Q(2,2)=1.D0
      Q(3,3)=1.D0
      DO I=1,3
        DO J=1,3
          Q(I+3,J)=BAINV(I,J)
        ENDDO
      ENDDO
C
      D(:,:) = 0.
C
      D(1,4) = 1.D0
      D(2,2) = SIN2
      D(2,5) = COS2
      D(3,3) = SIN3
      D(3,6) = COS3
C
      CALL AXB(GQ,G,Q,6,6,3)
C
      CALL AXB(GQD,GQ,D,6,3,6)
C
      END
