      SUBROUTINE INSEDI
C======================================================================
C     地形変化のモデル・パラメータ等を読み込む
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'SEDIMENT.h'
C
      CHARACTER(3)::CTMP3
      CHARACTER(6)::CTMP6
      CHARACTER(7)::CTMP7
      CHARACTER(8)::CTMP8
      CHARACTER(9)::CTMP9
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
               LSEDI = 0
            ELSE IF( CTMP3.EQ.'ON ' ) THEN
               LSEDI = 1
            ELSE
               CALL ERRMSG('INSEDI',6700)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE= INSEDI'
               WRITE(LP,*) 'VALUE=',CTMP3
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MODEL-EXCHANGE' ) THEN
            CALL GETC(CTMP9,9)
            IF( CTMP9.EQ.'TAKAHASHI' ) THEN
               MWEXSD = 0
            ELSE IF( CTMP9.EQ.'IKENO    ' ) THEN
               MWEXSD = 1
            ELSE
               CALL ERRMSG('INSEDI',6701)
               WRITE(LP,*) 'VALUE MUST BE TAKAHASHI OR IKENO'
               WRITE(LP,*) 'VARIABLE= MODEL-EXCHANGE'
               WRITE(LP,*) 'VALUE=',CTMP9
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MODEL-BED-SLOPE' ) THEN
            CALL GETC(CTMP3,3)
            IF( CTMP3.EQ.'OFF' ) THEN
               MBDSLP = 0
            ELSE IF( CTMP3.EQ.'ON ' ) THEN
               MBDSLP = 1
            ELSE
               CALL ERRMSG('INSEDI',6702)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE= OFFLINE'
               WRITE(LP,*) 'VALUE=',CTMP3
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'ANGLE-OF-REPOSE' ) THEN
            CALL GETR(PHIS)
            PHIS=PHIS/180.0D0*3.14159265358979D0
C
         ELSE IF( CLINE(IS:IE) .EQ. 'LOWER-LIMIT-KC' ) THEN
            CALL GETR(KCMIN)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MODEL-SEDI-CONC' ) THEN
            CALL GETC(CTMP7,7)
            IF( CTMP7.EQ.'AVERAGE' ) THEN
               MCONCSD = 0
            ELSE IF( CTMP7.EQ.'BOTTOM ' ) THEN
               MCONCSD = 1
            ELSE IF( CTMP7.EQ.'FUJII  ' ) THEN
               MCONCSD = 2
            ELSE
               CALL ERRMSG('INSEDI',6703)
               WRITE(LP,*) 'VALUE MUST BE AVERAGE, BOTTOM OR IKENO'
               WRITE(LP,*) 'VARIABLE= MODEL-SEDI-CONC'
               WRITE(LP,*) 'VALUE=',CTMP7
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MODEL-SUSPENDED-LOAD' ) THEN
            CALL GETC(CTMP8,8)
            IF( CTMP8.EQ.'CONSTANT' ) THEN
               MDIFSD = 0
            ELSE IF( CTMP8.EQ.'SC      ' ) THEN
               MDIFSD = 1
            ELSE
               CALL ERRMSG('INSEDI',6704)
               WRITE(LP,*) 'VALUE MUST BE CONSTANT OR SC'
               WRITE(LP,*) 'VARIABLE= MODEL-SUSPENDED-LOAD'
               WRITE(LP,*) 'VALUE=',CTMP8
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MODEL-SEDIMENTATION' ) THEN
            CALL GETC(CTMP7,7)
            IF( CTMP7.EQ.'JIMENEZ' ) THEN
               MSETSD = 0
            ELSE IF( CTMP7.EQ.'RUBEY  ' ) THEN
               MSETSD = 1
            ELSE IF( CTMP7.EQ.'AHRENS ' ) THEN
               MSETSD = 2
            ELSE IF( CTMP7.EQ.'SOULSBY' ) THEN
               MSETSD = 3
            ELSE
               CALL ERRMSG('INSEDI',6705)
               WRITE(LP,*)
     $          'VALUE MUST BE JIMENEZ, RUBEY, AHRENS OR SOULSBY'
               WRITE(LP,*) 'VARIABLE= MODEL-SEDIMENTATION'
               WRITE(LP,*) 'VALUE=',CTMP7
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MODEL-BED-FRICTION' ) THEN
            CALL GETC(CTMP6,6)
            IF( CTMP6.EQ.'LOG   ' ) THEN
               MUSTSD = 0
            ELSE IF( CTMP6.EQ.'SEKINE' ) THEN
               MUSTSD = 1
            ELSE
               CALL ERRMSG('INSEDI',6706)
               WRITE(LP,*) 'VALUE MUST BE LOG OR SEKINE'
               WRITE(LP,*) 'VARIABLE= MODEL-BED-FRICTION'
               WRITE(LP,*) 'VALUE=',CTMP6
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MODEL-ROUGHNESS' ) THEN
            CALL GETC(CTMP9,9)
            IF( CTMP9.EQ.'PARTICLE ' ) THEN
               MRGHSD = 0
            ELSE IF( CTMP9.EQ.'KOBAYASHI' ) THEN
               MRGHSD = 1
            ELSE IF( CTMP9.EQ.'HERRMANN ' ) THEN
               MRGHSD = 2
            ELSE IF( CTMP9.EQ.'MANNING  ' ) THEN
               MRGHSD = 3
            ELSE
               CALL ERRMSG('INSEDI',6707)
               WRITE(LP,*)
     $          'VALUE MUST BE PARTICLE, KOBAYASHI, HERRMANN OR MANNING'
               WRITE(LP,*) 'VARIABLE= MODEL-ROUGHNESS'
               WRITE(LP,*) 'VALUE=',CTMP9
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MODEL-SHIELDS' ) THEN
            CALL GETC(CTMP7,7)
            IF( CTMP7.EQ.'IWAGAKI' ) THEN
               MSHLSD = 0
            ELSE IF( CTMP7.EQ.'SOULSBY' ) THEN
               MSHLSD = 1
            ELSE
               CALL ERRMSG('INSEDI',6708)
               WRITE(LP,*) 'VALUE MUST BE IWAGAKI OR SOULSBY'
               WRITE(LP,*) 'VARIABLE= MODEL-SHIELDS'
               WRITE(LP,*) 'VALUE=',CTMP7
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'CALC-LIMIT-DEPTH' ) THEN
            CALL GETR(ZLIMSD)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'SPECIFIC-GRAVITY' ) THEN
            CALL GETR(SSAND)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'PARTICLE-SIZE' ) THEN
            CALL GETR(DSAND)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'POROSITY' ) THEN
            CALL GETR(GVSAND)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DIFFUSION-COEFF-H' ) THEN
            CALL GETR(DIFHSD)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DIFFUSION-COEFF-V' ) THEN
            CALL GETR(DIFVSD)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'SCHMIDT-H' ) THEN
            CALL GETR(SCTHSD)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'SCHMIDT-V' ) THEN
            CALL GETR(SCTVSD)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'A-IKENO' ) THEN
            CALL GETR(AEXSD)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'UPPER-LIMIT' ) THEN
            CALL GETR(CMAXSD)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'PARAM-SCHEME-SUSP' ) THEN
            CALL GETR(PARAMSD)
            PARAMSD2 = 1.0D0-PARAMSD
C
         ELSE IF( CLINE(IS:IE) .EQ. 'BED-INITIAL-TYPE' ) THEN
            CALL GETC(CTMP8,8)
            IF( CTMP8.EQ.'CONSTANT' ) THEN
               IBEDINI = 0
            ELSE IF( CTMP8.EQ.'FILE    ' ) THEN
               IBEDINI = 1
            ELSE
               CALL ERRMSG('INSEDI',6709)
               WRITE(LP,*) 'VALUE MUST BE CONSTANT OR FILE'
               WRITE(LP,*) 'VARIABLE= BED-INITIAL-TYPE'
               WRITE(LP,*) 'VALUE=',CTMP8
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'STRUCTURE-FILE' ) THEN
            CALL GETC(CTMP3,3)
            IF( CTMP3.EQ.'YES' ) THEN
               IBEDSTR = 1
            ELSE IF( CTMP3.EQ.'NO' ) THEN
               IBEDSTR = 0
            ELSE
               CALL ERRMSG('INSEDI',6710)
               WRITE(LP,*) 'VALUE MUST BE YES OR NO'
               WRITE(LP,*) 'VARIABLE= STRUCTURE-FILE'
               WRITE(LP,*) 'VALUE=',CTMP3
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'BED-INITIAL-VALUE' ) THEN
            CALL GETR(BEDINI)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'FEEDBACK' ) THEN
            CALL GETC(CTMP3,3)
            IF( CTMP3.EQ.'OFF' ) THEN
               MFDBCKSD = 0
            ELSE IF( CTMP3.EQ.'ON ' ) THEN
               MFDBCKSD = 1
            ELSE
               CALL ERRMSG('INSEDI',6711)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE= FEEDBACK'
               WRITE(LP,*) 'VALUE=',CTMP3
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'OFFLINE' ) THEN
            CALL GETC(CTMP3,3)
            IF( CTMP3.EQ.'OFF' ) THEN
               MOFFLNSD = 0
            ELSE IF( CTMP3.EQ.'ON ' ) THEN
               MOFFLNSD = 1
            ELSE
               CALL ERRMSG('INSEDI',6712)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE= OFFLINE'
               WRITE(LP,*) 'VALUE=',CTMP3
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'OFFLINE-START' ) THEN
            CALL GETR(TSOFFLN)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'OFFLINE-END' ) THEN
            CALL GETR(TEOFFLN)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'OFFLINE-INTERVAL' ) THEN
            CALL GETR(DTOFFLN)
C
         ELSE
            CALL ERRMSG('INSEDI',6713)
            WRITE(LP,*) 'UNKNOWN VARIABLE NAME: ',CLINE(IS:IE)
            CALL ABORT1('')
         END IF
  100 CONTINUE
  200 CONTINUE
C
      RETURN
C
C ... 読み込みエラー
  900 CONTINUE
      CALL ERRMSG('INSEDI',6714)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
      RETURN
      END
