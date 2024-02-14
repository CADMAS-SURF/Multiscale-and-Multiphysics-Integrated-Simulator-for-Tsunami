      SUBROUTINE INAIR(IRTRN)
C======================================================================
C     風場の計算条件を読み込む
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
C
      INTEGER::IRTRN
      CHARACTER(3)::CTMP3
      CHARACTER(8)::CTMP8
C
      INTEGER::N,IS,IE,IERR
C
      DO 100 N=1,100000
         CALL GET1(IS,IE,IERR)
         IF( IERR.GT.0 ) GO TO 900
C
         IF( CLINE(IS:IE) .EQ. '%END' ) THEN
            GO TO 200
C
         ELSE IF( CLINE(IS:IE) .EQ. 'CALC' ) THEN
            CALL GETC(CTMP3,3)
            IF( CTMP3.EQ.'OFF' ) THEN
               LAIR = 0
            ELSE IF( CTMP3.EQ.'ON ' ) THEN
               LAIR = 1
            ELSE IF( CTMP3.EQ.'NO ' ) THEN
               LAIR = 0
            ELSE IF( CTMP3.EQ.'YES' ) THEN
               LAIR = 1
            ELSE
               CALL ERRMSG('INAIR',6410)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE= INAIR'
               WRITE(LP,*) 'VALUE=',CTMP3
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'Z-AIR' ) THEN
            CALL MGETR(ZGRIDA,MZMA,MAXMZA)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'TURBULENT-AIR' ) THEN
            CALL GETC(CTMP3,3)
            IF( CTMP3.EQ.'OFF' ) THEN
               LTURBA = 0
            ELSE IF( CTMP3.EQ.'ON ' .OR. CTMP3.EQ.'LES' ) THEN
               LTURBA = 1
            ELSE IF( CTMP3.EQ.'K-E' ) THEN
               LTURBA = 2
            ELSE
               CALL ERRMSG('INAIR',6411)
               WRITE(LP,*) 'VALUE MUST BE ON OR LES OR OFF OR K-E'
               WRITE(LP,*) 'VARIABLE=TURBULENT-AIR'
               WRITE(LP,*) 'VALUE=',CTMP3
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'BOUNDARY-TYPE-AIR-XMIN' ) THEN
            CALL GETC(CTMP8,8)
            IF( CTMP8.EQ.'CONSTANT' ) THEN
               IBCAIRWES = 0
            ELSE IF( CTMP8.EQ.'FILE    ' ) THEN
               IBCAIRWES = 1
            ELSE IF( CTMP8.EQ.'SLIP    ' ) THEN
               IBCAIRWES = -1
            ELSE
               CALL ERRMSG('INAIR',6412)
               WRITE(LP,*) 'VALUE MUST BE CONSTANT OR FILE OR SLIP'
               WRITE(LP,*) 'VARIABLE=BOUNDARY-TYPE-AIR-XMIN'
               WRITE(LP,*) 'VALUE=',CTMP8
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'BOUNDARY-TYPE-AIR-XMAX' ) THEN
            CALL GETC(CTMP8,8)
            IF( CTMP8.EQ.'CONSTANT' ) THEN
               IBCAIREAS = 0
            ELSE IF( CTMP8.EQ.'FILE    ' ) THEN
               IBCAIREAS = 1
            ELSE IF( CTMP8.EQ.'SLIP    ' ) THEN
               IBCAIREAS = -1
            ELSE
               CALL ERRMSG('INAIR',6413)
               WRITE(LP,*) 'VALUE MUST BE CONSTANT OR FILE OR SLIP'
               WRITE(LP,*) 'VARIABLE=BOUNDARY-TYPE-AIR-XMAX'
               WRITE(LP,*) 'VALUE=',CTMP8
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'BOUNDARY-TYPE-AIR-YMIN' ) THEN
            CALL GETC(CTMP8,8)
            IF( CTMP8.EQ.'CONSTANT' ) THEN
               IBCAIRSOU = 0
            ELSE IF( CTMP8.EQ.'FILE    ' ) THEN
               IBCAIRSOU = 1
            ELSE IF( CTMP8.EQ.'SLIP    ' ) THEN
               IBCAIRSOU = -1
            ELSE
               CALL ERRMSG('INAIR',6414)
               WRITE(LP,*) 'VALUE MUST BE CONSTANT OR FILE OR SLIP'
               WRITE(LP,*) 'VARIABLE=BOUNDARY-TYPE-AIR-YMIN'
               WRITE(LP,*) 'VALUE=',CTMP8
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'BOUNDARY-TYPE-AIR-YMAX' ) THEN
            CALL GETC(CTMP8,8)
            IF( CTMP8.EQ.'CONSTANT' ) THEN
               IBCAIRNOR = 0
            ELSE IF( CTMP8.EQ.'FILE    ' ) THEN
               IBCAIRNOR = 1
            ELSE IF( CTMP8.EQ.'SLIP    ' ) THEN
               IBCAIRNOR = -1
            ELSE
               CALL ERRMSG('INAIR',6415)
               WRITE(LP,*) 'VALUE MUST BE CONSTANT OR FILE OR SLIP'
               WRITE(LP,*) 'VARIABLE=BOUNDARY-TYPE-AIR-YMAX'
               WRITE(LP,*) 'VALUE=',CTMP8
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'BOUNDARY-TYPE-AIR-TOP' ) THEN
            CALL GETC(CTMP8,8)
            IF( CTMP8.EQ.'FREE    ' ) THEN
               IBCAIRTOP = -2
            ELSE IF( CTMP8.EQ.'SLIP    ' ) THEN
               IBCAIRTOP = -1
            ELSE
               CALL ERRMSG('INAIR',6416)
               WRITE(LP,*) 'VALUE MUST BE FREE OR SLIP'
               WRITE(LP,*) 'VARIABLE=BOUNDARY-TYPE-AIR-TOP'
               WRITE(LP,*) 'VALUE=',CTMP8
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'U-AIR-XMIN' ) THEN
            CALL GETR(UBCAIRWES)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'V-AIR-XMIN' ) THEN
            CALL GETR(VBCAIRWES)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'U-AIR-XMAX' ) THEN
            CALL GETR(UBCAIREAS)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'V-AIR-XMAX' ) THEN
            CALL GETR(VBCAIREAS)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'U-AIR-YMIN' ) THEN
            CALL GETR(UBCAIRSOU)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'V-AIR-YMIN' ) THEN
            CALL GETR(VBCAIRSOU)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'U-AIR-YMAX' ) THEN
            CALL GETR(UBCAIRNOR)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'V-AIR-YMAX' ) THEN
            CALL GETR(VBCAIRNOR)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'K-AIR-XMIN' ) THEN
            CALL GETR(AKBCAIRWES)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'E-AIR-XMIN' ) THEN
            CALL GETR(EPBCAIRWES)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'K-AIR-XMAX' ) THEN
            CALL GETR(AKBCAIREAS)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'E-AIR-XMAX' ) THEN
            CALL GETR(EPBCAIREAS)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'K-AIR-YMIN' ) THEN
            CALL GETR(AKBCAIRSOU)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'E-AIR-YMIN' ) THEN
            CALL GETR(EPBCAIRSOU)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'K-AIR-YMAX' ) THEN
            CALL GETR(AKBCAIRNOR)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'E-AIR-YMAX' ) THEN
            CALL GETR(EPBCAIRNOR)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'P-AIR' ) THEN
            CALL GETR(PBCAIR)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'U-INITIAL-AIR' ) THEN
            CALL GETR(UINITAIR)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'V-INITIAL-AIR' ) THEN
            CALL GETR(VINITAIR)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'K-INITIAL-AIR' ) THEN
            CALL GETR(AKINITAIR)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'E-INITIAL-AIR' ) THEN
            CALL GETR(EPINITAIR)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DENSITY-AIR' ) THEN
            CALL GETR(RHOAIR)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'VISCOSITY-AIR' ) THEN
            CALL GETR(AMUAIR)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'PARAM-SCHEME-AIR' ) THEN
            CALL GETR(PARAMAIR)
            PARAMAIR2=1.0D0-PARAMAIR
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MAX-VELOCITY-AIR' ) THEN
            CALL GETR(VVMAXAIR)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'LIMIT-GZH-AIR' ) THEN
            CALL GETR(GZHAIR)
C
         ELSE
            CALL ERRMSG('INAIR',6417)
            WRITE(LP,*) 'UNKNOWN VARIABLE NAME: ',CLINE(IS:IE)
            IRTRN = 9
         END IF
  100 CONTINUE
  200 CONTINUE
C
C ... MZAの値を設定
      MZA = MZMA+1
C
      RETURN
C
C ... 読み込みエラー
  900 CONTINUE
      CALL ERRMSG('INAIR',6418)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
      RETURN
      END
