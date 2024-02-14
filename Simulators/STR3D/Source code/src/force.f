      SUBROUTINE FORCE(NN,FC,NFC,SI,ICRD,TR,GRID,FT,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION FT(6,*),NFC(2,NN),FC(6,NN),FC0(6),TRG(9),ICRD(*)
     &         ,GRID(3,*),TR(12,*)
C-----------------------------------------------------------------------
      DO 100 I=1,NN
        II=NFC(1,I)
        ID=NFC(2,I)
        IF( ID .EQ. 0 ) THEN
          CALL SHIFT1(FC0,FC(1,I),6)
        ELSE
          CALL TRNSMTX6(TRG,ICRD(ID),GRID(1,II),TR(1,ID),ITO)
          CALL AXB(FC0(1),TRG,FC(1,I),3,3,1)
          CALL AXB(FC0(4),TRG,FC(4,I),3,3,1)
        ENDIF
        CALL RMULT2(FT(1,II),FC0,SI,6)
  100 CONTINUE
C
      RETURN
      END
