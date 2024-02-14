      SUBROUTINE MODIJ(IS,IE,JS,JE,MODE,INSIDE)
C--------------------------------------------------
C     MODIFY INDEX
C
C     <INPUT/OUTPUT>
C     IS,IE,JS,JE : INDEX OF REGION
C
C     <INPUT>
C     MODE = 0 : POINT
C     MODE = 1 : CROP CELL/PLATE-Z AREA
C     MODE = 2 : CROP PLATE-X AREA
C     MODE = 3 : CROP PLATE-Y AREA
C
C     <OUTPUT>
C     INSIDE = 0 : OUTSIDE
C     INSIDE = 1 :  INSIDE
C--------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      INTEGER:: IS,IE,JS,JE,MODE,INSIDE
      INTEGER:: I1,J1
C
C
      INSIDE = 1
      IF( IAUTOD.EQ.0 ) RETURN
C
      IF( MODE.GE.1.AND.MODE.LE.3 ) THEN
         I1=0
         J1=0
         IF(MODE.EQ.2) I1=1
         IF(MODE.EQ.3) J1=1
C
         IS = MAX(IS,MYIS-I1)
         IE = MIN(IE,MYIE)
         JS = MAX(JS,MYJS-J1)
         JE = MIN(JE,MYJE)
C
         IF( IS.GT.IE.OR.JS.GT.JE ) THEN
            INSIDE=0
            RETURN
         ENDIF
C
         IS = IS-MYIS+2
         IE = IE-MYIS+2
         JS = JS-MYJS+2
         JE = JE-MYJS+2
C
      ELSE IF( MODE.EQ.0 ) THEN
         IF( IS.LT.MYIS .OR. IE.GT.MYIE .OR. 
     $       JS.LT.MYJS .OR. JE.GT.MYJE ) THEN
            INSIDE=0
            RETURN
         ENDIF
         IS = IS-MYIS+2
         JS = JS-MYJS+2
      ENDIF
C
      RETURN
      END
C
C
      SUBROUTINE MODI(I,INSIDE)
C--------------------------------------------------
C     MODIFY INDEX
C
C     <INPUT/OUTPUT>
C     I : INDEX OF REGION
C
C     <OUTPUT>
C     INSIDE = 0 : OUTSIDE
C     INSIDE = 1 :  INSIDE
C--------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      INTEGER:: I,INSIDE
C
C
      INSIDE = 1
      IF( IAUTOD.EQ.0 ) RETURN
C
      IF( I.LT.MYIS .OR. I.GT.MYIE ) THEN
         INSIDE=0
         RETURN
      ENDIF
      I = I-MYIS+2
C
      RETURN
      END
C
C
      SUBROUTINE MODJ(J,INSIDE)
C--------------------------------------------------
C     MODIFY INDEX
C
C     <INPUT/OUTPUT>
C     I : INDEX OF REGION
C
C     <OUTPUT>
C     INSIDE = 0 : OUTSIDE
C     INSIDE = 1 :  INSIDE
C--------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      INTEGER:: J,INSIDE
C
C
      INSIDE = 1
      IF( IAUTOD.EQ.0 ) RETURN
C
      IF( J.LT.MYJS .OR. J.GT.MYJE ) THEN
         INSIDE=0
         RETURN
      ENDIF
      J = J-MYJS+2
C
      RETURN
      END
