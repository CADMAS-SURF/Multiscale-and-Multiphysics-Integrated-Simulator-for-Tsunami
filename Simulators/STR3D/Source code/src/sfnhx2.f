      SUBROUTINE SFNHX2(R,S,T,ND,H)
C
C     R  : IN  : ξ
C     S  : IN  : η
C     T  : IN  : ζ
C     ND : IN  : 節点数
C     H  : OUT : H(i) = Ni(ξ,η,ζ)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION H(20),RG(20),SG(20),TG(20)
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
          H(I)=0.125*(1.+R*RG(I))*(1.+S*SG(I))*(1.+T*TG(I))
  100   CONTINUE
C
      ELSE
C
        DO 200 I=1,8
          H(I)=0.125*(1.+R*RG(I))*(1.+S*SG(I))*(1.+T*TG(I))
     &              *(R*RG(I)+S*SG(I)+T*TG(I)-2.)
  200   CONTINUE
C  
        DO 300 J=9,17,8
        DO 300 K=0,2,2
          I=J+K
          H(I)=0.25*(1.-R*R)*(1.+S*SG(I))*(1.+T*TG(I))
  300   CONTINUE
C
        DO 400 J=10,18,8
        DO 400 K=0,2,2
          I=J+K
          H(I)=0.25*(1.+R*RG(I))*(1.-S*S)*(1.+T*TG(I))
  400   CONTINUE
C
        DO 500 I=13,16
          H(I)=0.25*(1.+R*RG(I))*(1.+S*SG(I))*(1.-T*T)
  500   CONTINUE
C
      ENDIF
C
      RETURN
      END
