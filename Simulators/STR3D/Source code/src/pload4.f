      SUBROUTINE PLOAD4(NN,NPL4,PLD4,IELM,NM,ICRD,TR,GRID,SI,FT,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION NPL4(4,NN),IELM(NM,*),GRID(3,*),PLD4(4,NN),FC(3,9)
     &         ,NOD(9),FT(6,*),ICRD(*),TR(*),PRESS(9)
C-----------------------------------------------------------------------
      DO 100 I=1,NN
        IENO=NPL4(1,I)
        ITYPE=IELM(2,IENO)
        ID=NPL4(4,I)
        PRESS=PLD4(1,I)
        IF(ITYPE.EQ.1)THEN
          ND=IELM(3,IENO)
          CALL SHIFT0(NOD,IELM(8,IENO),ND)
          IF(ND.EQ.3) THEN
            CALL LOADTR1(GRID,NOD,PRESS,PLD4(2,I),ID,ICRD,TR,FC,ITO)
          ELSEIF(ND.EQ.6) THEN
            CALL LOADTR2(GRID,NOD,PRESS,PLD4(2,I),ID,ICRD,TR,FC,ITO)
          ELSE
            CALL LOADQU2(GRID,ND,NOD,PRESS,PLD4(2,I),ID,ICRD,TR,FC,ITO)
          ENDIF
        ELSE
          NODCOR=NPL4(2,I)
          NODOP=NPL4(3,I)
          IF(IELM(3,IENO).EQ.4) THEN
            ND=3
            CALL NODSET1(IELM(8,IENO),NODOP,NOD,ITO)
            CALL LOADTR1(GRID,NOD,PRESS,PLD4(2,I),ID,ICRD,TR,FC,ITO)
          ELSEIF(IELM(3,IENO).EQ.10) THEN
            ND=6
            CALL NODSET2(IELM(8,IENO),NODOP,NOD,ITO)
            CALL LOADTR2(GRID,NOD,PRESS,PLD4(2,I),ID,ICRD,TR,FC,ITO)
          ELSEIF(IELM(3,IENO).EQ.6) THEN
            IF(NODOP.EQ.0) THEN
              ND=3
              CALL NODSET5(ND,IELM(8,IENO),NODCOR,NODOP,NOD,ITO)
              CALL LOADTR1(GRID,NOD,PRESS,PLD4(2,I),ID,ICRD,TR,FC,ITO)
            ELSE
              ND=4
              CALL NODSET5(ND,IELM(8,IENO),NODCOR,NODOP,NOD,ITO)
              CALL LOADQU2(GRID,ND,NOD,PRESS,PLD4(2,I),ID,ICRD,TR,FC
     &                    ,ITO)
            ENDIF
          ELSEIF(IELM(3,IENO).EQ.15) THEN
            IF(NODOP.EQ.0) THEN
              ND=6
              CALL NODSET5(ND,IELM(8,IENO),NODCOR,NODOP,NOD,ITO)
              CALL LOADTR2(GRID,NOD,PRESS,PLD4(2,I),ID,ICRD,TR,FC,ITO)
            ELSE
              ND=8
              CALL NODSET5(ND,IELM(8,IENO),NODCOR,NODOP,NOD,ITO)
              CALL LOADQU2(GRID,ND,NOD,PRESS,PLD4(2,I),ID,ICRD,TR,FC
     &                    ,ITO)
            ENDIF
          ELSEIF(IELM(3,IENO).EQ.8) THEN
            ND=4
            CALL NODSET3(ND,IELM(8,IENO),NODCOR,NODOP,NOD,ITO)
            CALL LOADQU2(GRID,ND,NOD,PRESS,PLD4(2,I),ID,ICRD,TR,FC,ITO)
          ELSEIF(IELM(3,IENO).EQ.20) THEN
            ND=8
            CALL NODSET3(ND,IELM(8,IENO),NODCOR,NODOP,NOD,ITO)
            CALL LOADQU2(GRID,ND,NOD,PRESS,PLD4(2,I),ID,ICRD,TR,FC,ITO)
          ENDIF
        ENDIF
        CALL FRCADD(ND,FC,NOD,SI,FT)
  100 CONTINUE
C
      RETURN
      END
