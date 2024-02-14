      SUBROUTINE AUTONEST(IXS,IXE,JYS,JYE,KZS,KZE)
C======================================================================
C     親領域において子領域のネスティング設定を自動で行う
C                                 coded by Hiroshi Higashino @PARI TRC
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'AREA.h'
      INCLUDE 'FILE.h'
      INCLUDE 'OBSTI.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'BOUNDI.h'

      INTEGER::IXS,IXE,JYS,JYE,KZS,KZE
      INTEGER::I,J


C
C SOLIDを指定
C
      NOBSS = NOBSS + 1
      IF( NOBSS.GT.NOBSSZ ) THEN
         CALL ERRMSG('AUTONEST',6810)
         WRITE(LP,*) 'THE NUMBER OF SOLID OBSTACLE MAY ',
     $               'NOT BE OVER NOBSSZ'
         WRITE(LP,*) 'AUTONEST SUBROUTINE'
         CALL ABORT1('')
      END IF
C
      IOBSS(1,NOBSS) = IXS+NESML(2)+1
      IOBSS(2,NOBSS) = IXE-NESML(3)-1
      IOBSS(3,NOBSS) = JYS+NESML(1)+1
      IOBSS(4,NOBSS) = JYE-NESML(4)-1
      IOBSS(5,NOBSS) = KZS
      IOBSS(6,NOBSS) = KZE
C
C
      IF( NOUTLT+4.GT.NOTFSZ ) THEN
         CALL ERRMSG('AUTONEST',6811)
         WRITE(LP,*) 'THE NUMBER OF FREE BOUNDARY MAY ',
     $               'NOT BE OVER NOTFSZ'
         WRITE(LP,*) 'AUTONEST SUBROUTINE'
         CALL ABORT1('')
      END IF
      IF( NAREA+4.GT.NARASZ ) THEN
         CALL ERRMSG('AUTONEST',6812)
         WRITE(LP,*) 'THE NUMBER OF AREA MAY NOT BE ',
     $               'OVER NARASZ IN WHOLE INPUT DATA'
         WRITE(LP,*) 'AUTONEST SUBROUTINE'
         CALL ABORT1('')
      END IF
C
C FREE-Iを指定
C
      DO I=1,2
        NOUTLT = NOUTLT + 1
        NAREA = NAREA + 1

        IF(I.EQ.1)THEN
          IAREA(1,NAREA) = IOBSS(1,NOBSS)-1
        ELSE
          IAREA(1,NAREA) = IOBSS(2,NOBSS)
        ENDIF
        IAREA(2,NAREA) = IAREA(1,NAREA)
        IAREA(3,NAREA) = IOBSS(3,NOBSS)
        IAREA(4,NAREA) = IOBSS(4,NOBSS)
        IAREA(5,NAREA) = IOBSS(5,NOBSS)
        IAREA(6,NAREA) = IOBSS(6,NOBSS)
        IAREA(7,NAREA) = 1
        MOUTLT(NOUTLT) = NAREA
        IOUTLT(1,NOUTLT) = -1
        IOUTLT(2,NOUTLT) = -1
        IOUTLT(3,NOUTLT) = 0
      ENDDO

C
C FREE-Jを指定
C
      DO J=1,2
        NOUTLT = NOUTLT + 1
        NAREA = NAREA + 1

        IAREA(1,NAREA) = IOBSS(1,NOBSS)
        IAREA(2,NAREA) = IOBSS(2,NOBSS)
        IF(J.EQ.1)THEN
          IAREA(3,NAREA) = IOBSS(3,NOBSS)-1
        ELSE
          IAREA(3,NAREA) = IOBSS(4,NOBSS)
        ENDIF
        IAREA(4,NAREA) = IAREA(3,NAREA)
        IAREA(5,NAREA) = IOBSS(5,NOBSS)
        IAREA(6,NAREA) = IOBSS(6,NOBSS)
        IAREA(7,NAREA) = 2
        MOUTLT(NOUTLT) = NAREA
        IOUTLT(1,NOUTLT) = -1
        IOUTLT(2,NOUTLT) = -1
        IOUTLT(3,NOUTLT) = 0
      ENDDO

      WRITE(*,*) 'AUTONEST_SOLID',IOBSS(1,NOBSS),IOBSS(2,NOBSS)
     &          ,IOBSS(3,NOBSS),IOBSS(4,NOBSS)
      WRITE(LP,*) 'AUTONEST_SOLID',IOBSS(1,NOBSS),IOBSS(2,NOBSS)
     &          ,IOBSS(3,NOBSS),IOBSS(4,NOBSS)
      RETURN
      END
