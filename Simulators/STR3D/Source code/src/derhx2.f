      SUBROUTINE DERHX2(R,S,T,ND,P)
C
C     R  : IN  : ξ (ξ1)
C     S  : IN  : η (ξ2)
C     T  : IN  : ζ (ξ3)
C     ND : IN  : 節点数
C     P  : OUT : P(i,j) = ∂Nj/∂ξi
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(3,20),RG(20),SG(20),TG(20)
      DATA RG / -1. ,  1. ,  1. , -1. , -1. ,  1. ,  1. , -1.
     &        ,  0. ,  1. ,  0. , -1. , -1. ,  1. ,  1. , -1.
     &        ,  0. ,  1. ,  0. , -1. /
      DATA SG / -1. , -1. ,  1. ,  1. , -1. , -1. ,  1. ,  1.
     &        , -1. ,  0. ,  1. ,  0. , -1. , -1. ,  1. ,  1.
     &        , -1. ,  0. ,  1. ,  0. /
      DATA TG / -1. , -1. , -1. , -1. ,  1. ,  1. ,  1. ,  1.
     &        , -1. , -1. , -1. , -1. ,  0. ,  0. ,  0. ,  0.
     &        ,  1. ,  1. ,  1. ,  1. /
C
      IF(ND .EQ. 8) THEN
C
        DO 100 I=1,8
          R1=1.+R*RG(I)
          S1=1.+S*SG(I)
          T1=1.+T*TG(I)
          P(1,I)=0.125*RG(I)*S1*T1
          P(2,I)=0.125*R1*SG(I)*T1
          P(3,I)=0.125*R1*S1*TG(I)
  100   CONTINUE
C
      ELSE
C
        DO 200 I=1,8
          R1=1.+R*RG(I)
          S1=1.+S*SG(I)
          T1=1.+T*TG(I)
          RST1=R1*S1*T1
          RST2=R*RG(I)+S*SG(I)+T*TG(I)-2.
          P(1,I)=0.125*( RG(I)*S1*T1*RST2+RST1*RG(I) )
          P(2,I)=0.125*( R1*SG(I)*T1*RST2+RST1*SG(I) )
          P(3,I)=0.125*( R1*S1*TG(I)*RST2+RST1*TG(I) )
  200   CONTINUE
C  
        DO 300 J=9,17,8
        DO 300 K=0,2,2
          I=J+K
          S1=1.+S*SG(I)
          T1=1.+T*TG(I)
          RR=1.-R*R
          P(1,I)=-0.5*R*S1*T1
          P(2,I)=0.25*RR*SG(I)*T1
          P(3,I)=0.25*RR*S1*TG(I)
  300   CONTINUE
C
        DO 400 J=10,18,8
        DO 400 K=0,2,2
          I=J+K
          R1=1.+R*RG(I)
          T1=1.+T*TG(I)
          SS=1.-S*S
          P(1,I)=0.25*RG(I)*SS*T1
          P(2,I)=-0.5*R1*S*T1
          P(3,I)=0.25*R1*SS*TG(I)
  400   CONTINUE
C
        DO 500 I=13,16
          R1=1.+R*RG(I)
          S1=1.+S*SG(I)
          TT=1.-T*T
          P(1,I)=0.25*RG(I)*S1*TT
          P(2,I)=0.25*R1*SG(I)*TT
          P(3,I)=-0.5*R1*S1*T
  500   CONTINUE
C
      ENDIF
C
      RETURN
      END
