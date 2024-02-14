      SUBROUTINE NODSET3(ND,KN,NODCOR,NODOP,NOD,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KN(*),NOD(ND)
C---------------------------------------------------------------------==
C        1         2         3         4         5         6         7**
C23456789+123456789+123456789+123456789+123456789+123456789+123456789+12
C---------------------------------------------------------------------==
C
      DO 100 I=1,8
        IF(NODCOR.EQ.KN(I)) GOTO(201,202,203,204,205,206,207,208) I
  100 CONTINUE
C
      GOTO 900
C
  201 IF(NODOP.EQ.KN(3)) THEN
        NOD( 1) = KN( 1)
        NOD( 2) = KN( 2)
        NOD( 3) = KN( 3)
        NOD( 4) = KN( 4)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN( 9)
          NOD( 6) = KN(10)
          NOD( 7) = KN(11)
          NOD( 8) = KN(12)
        ENDIF
      ELSEIF(NODOP.EQ.KN(6)) THEN
        NOD( 1) = KN( 1)
        NOD( 2) = KN( 5)
        NOD( 3) = KN( 6)
        NOD( 4) = KN( 2)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(13)
          NOD( 6) = KN(17)
          NOD( 7) = KN(14)
          NOD( 8) = KN( 9)
        ENDIF
      ELSEIF(NODOP.EQ.KN(8)) THEN
        NOD( 1) = KN( 1)
        NOD( 2) = KN( 4)
        NOD( 3) = KN( 8)
        NOD( 4) = KN( 5)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(12)
          NOD( 6) = KN(16)
          NOD( 7) = KN(20)
          NOD( 8) = KN(13)
        ENDIF
      ELSE
        GOTO 900
      ENDIF
      RETURN
C
  202 IF(NODOP.EQ.KN(4)) THEN
        NOD( 1) = KN( 2)
        NOD( 2) = KN( 3)
        NOD( 3) = KN( 4)
        NOD( 4) = KN( 1)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(10)
          NOD( 6) = KN(11)
          NOD( 7) = KN(12)
          NOD( 8) = KN( 9)
        ENDIF
      ELSEIF(NODOP.EQ.KN(5)) THEN
        NOD( 1) = KN( 2)
        NOD( 2) = KN( 1)
        NOD( 3) = KN( 5)
        NOD( 4) = KN( 6)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN( 9)
          NOD( 6) = KN(13)
          NOD( 7) = KN(17)
          NOD( 8) = KN(14)
        ENDIF
      ELSEIF(NODOP.EQ.KN(7)) THEN
        NOD( 1) = KN( 2)
        NOD( 2) = KN( 6)
        NOD( 3) = KN( 7)
        NOD( 4) = KN( 3)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(14)
          NOD( 6) = KN(18)
          NOD( 7) = KN(15)
          NOD( 8) = KN(10)
        ENDIF
      ELSE
        GOTO 900
      ENDIF
      RETURN
C
  203 IF(NODOP.EQ.KN(1)) THEN
        NOD( 1) = KN( 3)
        NOD( 2) = KN( 4)
        NOD( 3) = KN( 1)
        NOD( 4) = KN( 2)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(11)
          NOD( 6) = KN(12)
          NOD( 7) = KN( 9)
          NOD( 8) = KN(10)
        ENDIF
      ELSEIF(NODOP.EQ.KN(6)) THEN
        NOD( 1) = KN( 3)
        NOD( 2) = KN( 2)
        NOD( 3) = KN( 6)
        NOD( 4) = KN( 7)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(10)
          NOD( 6) = KN(14)
          NOD( 7) = KN(18)
          NOD( 8) = KN(15)
        ENDIF
      ELSEIF(NODOP.EQ.KN(8)) THEN
        NOD( 1) = KN( 3)
        NOD( 2) = KN( 7)
        NOD( 3) = KN( 8)
        NOD( 4) = KN( 4)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(15)
          NOD( 6) = KN(19)
          NOD( 7) = KN(16)
          NOD( 8) = KN(11)
        ENDIF
      ELSE
        GOTO 900
      ENDIF
      RETURN
C
  204 IF(NODOP.EQ.KN(2)) THEN
        NOD( 1) = KN( 4)
        NOD( 2) = KN( 1)
        NOD( 3) = KN( 2)
        NOD( 4) = KN( 3)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(12)
          NOD( 6) = KN( 9)
          NOD( 7) = KN(10)
          NOD( 8) = KN(11)
        ENDIF
      ELSEIF(NODOP.EQ.KN(5)) THEN
        NOD( 1) = KN( 4)
        NOD( 2) = KN( 8)
        NOD( 3) = KN( 5)
        NOD( 4) = KN( 1)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(16)
          NOD( 6) = KN(20)
          NOD( 7) = KN(13)
          NOD( 8) = KN(12)
        ENDIF
      ELSEIF(NODOP.EQ.KN(7)) THEN
        NOD( 1) = KN( 4)
        NOD( 2) = KN( 3)
        NOD( 3) = KN( 7)
        NOD( 4) = KN( 8)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(11)
          NOD( 6) = KN(15)
          NOD( 7) = KN(19)
          NOD( 8) = KN(16)
        ENDIF
      ELSE
        GOTO 900
      ENDIF
      RETURN
C
  205 IF(NODOP.EQ.KN(2)) THEN
        NOD( 1) = KN( 5)
        NOD( 2) = KN( 6)
        NOD( 3) = KN( 2)
        NOD( 4) = KN( 1)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(17)
          NOD( 6) = KN(14)
          NOD( 7) = KN( 9)
          NOD( 8) = KN(13)
        ENDIF
      ELSEIF(NODOP.EQ.KN(4)) THEN
        NOD( 1) = KN( 5)
        NOD( 2) = KN( 1)
        NOD( 3) = KN( 4)
        NOD( 4) = KN( 8)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(13)
          NOD( 6) = KN(12)
          NOD( 7) = KN(16)
          NOD( 8) = KN(20)
        ENDIF
      ELSEIF(NODOP.EQ.KN(7)) THEN
        NOD( 1) = KN( 5)
        NOD( 2) = KN( 8)
        NOD( 3) = KN( 7)
        NOD( 4) = KN( 6)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(20)
          NOD( 6) = KN(19)
          NOD( 7) = KN(18)
          NOD( 8) = KN(17)
        ENDIF
      ELSE
        GOTO 900
      ENDIF
      RETURN
C
  206 IF(NODOP.EQ.KN(1)) THEN
        NOD( 1) = KN( 6)
        NOD( 2) = KN( 2)
        NOD( 3) = KN( 1)
        NOD( 4) = KN( 5)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(14)
          NOD( 6) = KN( 9)
          NOD( 7) = KN(13)
          NOD( 8) = KN(17)
        ENDIF
      ELSEIF(NODOP.EQ.KN(3)) THEN
        NOD( 1) = KN( 6)
        NOD( 2) = KN( 7)
        NOD( 3) = KN( 3)
        NOD( 4) = KN( 2)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(18)
          NOD( 6) = KN(15)
          NOD( 7) = KN(10)
          NOD( 8) = KN(14)
        ENDIF
      ELSEIF(NODOP.EQ.KN(8)) THEN
        NOD( 1) = KN( 6)
        NOD( 2) = KN( 5)
        NOD( 3) = KN( 8)
        NOD( 4) = KN( 7)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(17)
          NOD( 6) = KN(20)
          NOD( 7) = KN(19)
          NOD( 8) = KN(18)
        ENDIF
      ELSE
        GOTO 900
      ENDIF
      RETURN
C
  207 IF(NODOP.EQ.KN(2)) THEN
        NOD( 1) = KN( 7)
        NOD( 2) = KN( 3)
        NOD( 3) = KN( 2)
        NOD( 4) = KN( 6)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(15)
          NOD( 6) = KN(10)
          NOD( 7) = KN(14)
          NOD( 8) = KN(18)
        ENDIF
      ELSEIF(NODOP.EQ.KN(4)) THEN
        NOD( 1) = KN( 7)
        NOD( 2) = KN( 8)
        NOD( 3) = KN( 4)
        NOD( 4) = KN( 3)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(19)
          NOD( 6) = KN(16)
          NOD( 7) = KN(11)
          NOD( 8) = KN(15)
        ENDIF
      ELSEIF(NODOP.EQ.KN(5)) THEN
        NOD( 1) = KN( 7)
        NOD( 2) = KN( 6)
        NOD( 3) = KN( 5)
        NOD( 4) = KN( 8)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(18)
          NOD( 6) = KN(17)
          NOD( 7) = KN(20)
          NOD( 8) = KN(19)
        ENDIF
      ELSE
        GOTO 900
      ENDIF
      RETURN
C
  208 IF(NODOP.EQ.KN(1)) THEN
        NOD( 1) = KN( 8)
        NOD( 2) = KN( 5)
        NOD( 3) = KN( 1)
        NOD( 4) = KN( 4)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(20)
          NOD( 6) = KN(13)
          NOD( 7) = KN(12)
          NOD( 8) = KN(16)
        ENDIF
      ELSEIF(NODOP.EQ.KN(3)) THEN
        NOD( 1) = KN( 8)
        NOD( 2) = KN( 4)
        NOD( 3) = KN( 3)
        NOD( 4) = KN( 7)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(16)
          NOD( 6) = KN(11)
          NOD( 7) = KN(15)
          NOD( 8) = KN(19)
        ENDIF
      ELSEIF(NODOP.EQ.KN(6)) THEN
        NOD( 1) = KN( 8)
        NOD( 2) = KN( 7)
        NOD( 3) = KN( 6)
        NOD( 4) = KN( 5)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(19)
          NOD( 6) = KN(18)
          NOD( 7) = KN(17)
          NOD( 8) = KN(20)
        ENDIF
      ELSE
        GOTO 900
      ENDIF
      RETURN
C
  900 WRITE(ITO,*) 'STOP IN SUB. NODSET3!'
      CALL ERRSTP(90,ITO)
C
      END
