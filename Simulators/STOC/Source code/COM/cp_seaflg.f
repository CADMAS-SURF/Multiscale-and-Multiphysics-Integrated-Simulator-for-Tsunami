      SUBROUTINE CP_SEAFLG(I0,J0,ICHK,IP,JP)
C
      IMPLICIT NONE
C
      INCLUDE 'OBSTI.h'
      INCLUDE 'FILE.h'
C
      INTEGER,INTENT(INOUT)::I0,J0,ICHK,IP,JP
C
      INTEGER::II,JJ,N
C
      IF(NSEA.LT.0) RETURN
C
      II   = I0
      JJ   = J0
      DO 100 N = 1,NSEA
        IF(ISEA(1,N).EQ.II.AND.ISEA(2,N).EQ.JJ) THEN
          GO TO 110
        END IF
  100 CONTINUE
C
      IF(ICHK.EQ.2) THEN
        WRITE(LP,60) II,JJ,IP,JP
   60   FORMAT(' ## CHILD-PARENT (LAND-SEA)POINT = (',2I5,' )',
     $         '    # PARENT(I,J) =',2I5 )
        ICHK = - ICHK
      ELSE IF(ICHK.EQ.3) THEN
        WRITE(LP,65) II,JJ,IP,JP
   65   FORMAT(' ## CHILD-PARENT (SEA-LAND)POINT = (',2I5,' )',
     $         '    # PARENT(I,J) =',2I5 )
        ICHK = - ICHK
      END IF
C
  110 CONTINUE
C
      RETURN
      END
