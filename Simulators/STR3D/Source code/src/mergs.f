      SUBROUTINE MERGS(IAVE,SIGG,EPSG,SIG,EPS,PRNSIGG,PRNEPSG,PRNSIG
     &                ,PRNEPS,VMSIGG,VMSIG,KN,ND)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KN(ND),IAVE(*),SIGG(6,*),EPSG(6,*),SIG(6,ND),EPS(6,ND)
     &         ,PRNSIGG(3,*),PRNEPSG(3,*),PRNSIG(3,ND),PRNEPS(3,ND)
     &         ,VMSIGG(*),VMSIG(ND)
C-----------------------------------------------------------------------
      DO 100 I=1,ND
        NODE=KN(I)
        IAVE(NODE)=IAVE(NODE)+1
        ALP=DBLE(IAVE(NODE))
        DO 200 J=1,6
          SIGG(J,NODE)=( SIGG(J,NODE)*(ALP-1.D0) + SIG(J,I) ) / ALP
          EPSG(J,NODE)=( EPSG(J,NODE)*(ALP-1.D0) + EPS(J,I) ) / ALP
  200   CONTINUE
        DO 300 J=1,3
          PRNSIGG(J,NODE)=(PRNSIGG(J,NODE)*(ALP-1.D0)+PRNSIG(J,I))/ALP
          PRNEPSG(J,NODE)=(PRNEPSG(J,NODE)*(ALP-1.D0)+PRNEPS(J,I))/ALP
  300   CONTINUE
        VMSIGG(NODE)=( VMSIGG(NODE)*(ALP-1.D0) + VMSIG(I) ) / ALP
  100 CONTINUE
C
      RETURN
      END
