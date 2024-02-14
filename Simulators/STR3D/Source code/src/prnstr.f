      SUBROUTINE PRNSTR(ND,SIG,EPS,PRNSIG,PRNEPS,VMSIG,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION SIG(6,ND),EPS(6,ND),PRNSIG(3,ND),PRNEPS(3,ND),VMSIG(ND)
     &         ,CMX(3,3)
C---------------------------------------------------------------------==
C        1         2         3         4         5         6         7**
C23456789+123456789+123456789+123456789+123456789+123456789+123456789+12
C---------------------------------------------------------------------==
C
      DO 100 I=1,ND
        CALL KOJI3D(SIG(1,I),PRNSIG(1,I),CMX,ITO)
        CALL KOJI3E(EPS(1,I),PRNEPS(1,I),CMX,ITO)
        VMSIG(I)=DSQRT(0.5D0*( (SIG(2,I)-SIG(3,I))*(SIG(2,I)-SIG(3,I))
     &                        +(SIG(3,I)-SIG(1,I))*(SIG(3,I)-SIG(1,I))
     &                        +(SIG(1,I)-SIG(2,I))*(SIG(1,I)-SIG(2,I)) )
     &                +3.0D0*( SIG(4,I)*SIG(4,I)+SIG(5,I)*SIG(5,I)
     &                        +SIG(6,I)*SIG(6,I) )  )
  100 CONTINUE
C
      RETURN
      END
