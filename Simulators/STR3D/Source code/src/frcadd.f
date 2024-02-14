      SUBROUTINE FRCADD(NN,FC,NFC,SI,FT)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION FT(6,*),NFC(NN),FC(3,NN)
C-----------------------------------------------------------------------
      DO 100 I=1,NN
        II=NFC(I)
        CALL RMULT2(FT(1,II),FC(1,I),SI,3)
  100 CONTINUE
C
      RETURN
      END
