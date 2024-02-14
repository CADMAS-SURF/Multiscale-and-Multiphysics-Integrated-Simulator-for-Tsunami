      SUBROUTINE INMETHD
C======================================================================
C     METHODブロックを読み込む
C======================================================================
      IMPLICIT NONE
C
      INCLUDe 'CONTROL.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
C
      INTEGER::IE,IERR,IS,M,N,NDAT1
      REAL(8):: RTMP(2*MAXTBL)
C
C
      DO 100 N=1,100000
         CALL GET1(IS,IE,IERR)
         IF( IERR.GT.0 ) GO TO 900
C
         IF( CLINE(IS:IE) .EQ. '%END' ) THEN
            GO TO 200
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MODEL' ) THEN
            CALL GETC(CMODEL,8)
            IF( CMODEL.NE.'RANDOM'.AND.CMODEL.NE.'AVERAGE' ) THEN
               CALL ERRMSG('INMETHD',23)
               WRITE(LP,*)' VALUE MUST BE RANDOM OR AVERAGE'
               WRITE(LP,*) 'VARIABLE=MODEL'
               WRITE(LP,*) 'VALUE=',trim(CMODEL)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            ENDIF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'RAND_SEED' ) THEN
            CALL GETI(SEED)
            IF( SEED.LT.0 ) THEN
               CALL ERRMSG('INMETHD',25)
               WRITE(LP,*)' VALUE MUST BE POSITIVE'
               WRITE(LP,*) 'VARIABLE=RAND_SEED'
               WRITE(LP,*) 'VALUE=',SEED
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            ENDIF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'FRAGILITY' ) THEN
            CALL MGETR(RTMP,NDAT1,2*MAXTBL)
            IF( MOD(NDAT1,2).EQ.1 ) THEN
               CALL ERRMSG('INMETHD',26)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 2*N'
               WRITE(LP,*) 'VARIABLE=FRAGILITY'
               WRITE(LP,*) 'VALUE=',(RTMP(M),' ',M=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            ENDIF
            N_TBL=NDAT1/2
            DO M=1,N_TBL
               TBL_INUND(M)=RTMP(M*2-1)
               TBL_PDST(M) =RTMP(M*2)
C
C ............ ERROR CHECK
               IF( M.EQ.1.AND.TBL_PDST(M).NE.0.D0 ) THEN
                  CALL ERRMSG('INMETHD',30)
                  WRITE(LP,*) 'PROBABILITY DATA MUST START',
     $               ' WITH VALUE 0.0'
                  WRITE(LP,*) 'VARIABLE=FRAGILITY'
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
               ENDIF
               IF( M.GT.1 ) THEN
                  IF( TBL_INUND(M).LE.TBL_INUND(M-1).OR.
     $                TBL_PDST(M) .LT.TBL_PDST(M-1) )THEN
                     CALL ERRMSG('INMETHD',27)
                     WRITE(LP,*) 'FRAGILITY TABLE MUST',
     $                           ' MONOTONICLY INCREASE'
                     WRITE(LP,*) 'VARIABLE=FRAGILITY'
                     WRITE(LP,*) 'LINE=',CLINE
                     CALL ABORT1('')
                  ENDIF
               ENDIF
               IF( TBL_INUND(M).LT.0.D0 ) THEN
                  CALL ERRMSG('INMETHD',28)
                  WRITE(LP,*) 'INUNDATION VALUE OF FRAGILITY TABLE',
     $                        ' MUST >= 0.0'
                  WRITE(LP,*) 'VARIABLE=FRAGILITY'
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
               ENDIF
               IF( TBL_PDST(M).LT.0.D0.OR.TBL_PDST(M).GT.1.D0 ) THEN
                  CALL ERRMSG('INMETHD',29)
                  WRITE(LP,*) 'PROBABILITY VALUE OF FRAGILITY TABLE',
     $                        ' MUST >= 0.0 AND <=1.0'
                  WRITE(LP,*) 'VARIABLE=FRAGILITY'
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
               ENDIF
            ENDDO
C
         ELSE IF( CLINE(IS:IE) .EQ. 'GENERATE-INUND' ) THEN
            CALL GETR(G_INUND)
            IF( G_INUND.LT.1.D-2.AND.G_INUND.NE.0.0D0 ) THEN
               CALL ERRMSG('INMETHD',29)
               WRITE(LP,*)' VALUE MUST BE >= 1cm or 0'
               WRITE(LP,*) 'VARIABLE=GENERATE-INUND'
               WRITE(LP,*) 'VALUE=',G_INUND
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            ENDIF
C
         ELSE
            CALL ERRMSG('INMETHD',24)
            WRITE(LP,*) 'UNKNOWN VARIABLE NAME: ',CLINE(IS:IE)
            CALL ABORT1('')
         END IF
  100 CONTINUE
  200 CONTINUE
C
      IF( CMODEL.EQ.'AVERAGE'.AND.G_INUND.EQ.0.D0 ) THEN
         CALL ERRMSG('INMETHD',28)
         WRITE(LP,*)' VALUE MUST BE > 0 IF MODEL=AVERAGE'
         WRITE(LP,*) 'VARIABLE=GENERATE-INUND'
         WRITE(LP,*) 'VALUE=',G_INUND
         CALL ABORT1('')
      ENDIF
C
      RETURN
C
C ... 読み込みエラー
  900 CONTINUE
      CALL ERRMSG('INMETHD',22)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
      END
