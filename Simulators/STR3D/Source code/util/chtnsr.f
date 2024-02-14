      SUBROUTINE CHTNSR(TE,A,B)
C-------------------------------------
C     {X'}=[TE]*{X}
C
C     {SIG'}=[A]*{SIG}   [A_INV]=[B_TRANS]
C
C     {EPS'}=[B]*{EPS}   [B_INV]=[A_TRANS]
C-------------------------------------
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION TE(3,3),A(6,6),B(6,6)
C
      RL1=TE(1,1)
      RL2=TE(2,1)
      RL3=TE(3,1)
      RM1=TE(1,2)
      RM2=TE(2,2)
      RM3=TE(3,2)
      RN1=TE(1,3)
      RN2=TE(2,3)
      RN3=TE(3,3)
C                                                                       
      A(1,1)=RL1*RL1
      A(1,2)=RM1*RM1
      A(1,3)=RN1*RN1
      A(1,4)=2.D0*RL1*RM1
      A(1,5)=2.D0*RM1*RN1
      A(1,6)=2.D0*RN1*RL1
C
      A(2,1)=RL2*RL2
      A(2,2)=RM2*RM2
      A(2,3)=RN2*RN2
      A(2,4)=2.D0*RL2*RM2
      A(2,5)=2.D0*RM2*RN2
      A(2,6)=2.D0*RN2*RL2
C
      A(3,1)=RL3*RL3
      A(3,2)=RM3*RM3
      A(3,3)=RN3*RN3
      A(3,4)=2.D0*RL3*RM3
      A(3,5)=2.D0*RM3*RN3
      A(3,6)=2.D0*RN3*RL3
C
      A(4,1)=RL1*RL2
      A(4,2)=RM1*RM2
      A(4,3)=RN1*RN2
      A(4,4)=RL1*RM2+RM1*RL2
      A(4,5)=RM1*RN2+RN1*RM2
      A(4,6)=RN1*RL2+RL1*RN2
C
      A(5,1)=RL2*RL3
      A(5,2)=RM2*RM3
      A(5,3)=RN2*RN3
      A(5,4)=RL2*RM3+RM2*RL3
      A(5,5)=RM2*RN3+RN2*RM3
      A(5,6)=RN2*RL3+RL2*RN3
C
      A(6,1)=RL3*RL1
      A(6,2)=RM3*RM1
      A(6,3)=RN3*RN1
      A(6,4)=RL3*RM1+RM3*RL1
      A(6,5)=RM3*RN1+RN3*RM1
      A(6,6)=RN3*RL1+RL3*RN1
C
      DO 100 I=1,6
        DO 200 J=1,6
          IF(I.GE.4.AND.J.LE.3) THEN
            FACT=2.D0
          ELSEIF(I.LE.3.AND.J.GE.4) THEN
            FACT=0.5D0
          ELSE
            FACT=1.D0
          ENDIF
          B(I,J)=FACT*A(I,J)
  200   CONTINUE
  100 CONTINUE
C
      RETURN
      END
