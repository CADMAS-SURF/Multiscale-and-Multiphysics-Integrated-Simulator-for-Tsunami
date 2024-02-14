      SUBROUTINE INOUTP
C======================================================================
C     計算結果のファイル出力方法の読み込み
C     入力データの追加方法は README_INPUT を参照
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'OUTPUT.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
C=================================================FOR OIL_PARTICLE START
      INCLUDE 'OIL.h'
      INTEGER :: NOILF
C=================================================FOR OIL_PARTICLE END
      INCLUDE 'AGENT.h'
C
      INTEGER::ITMP(NPNTSZ)
      REAL(8)::RTMP(3)
      CHARACTER(3)::CTMP
      CHARACTER(6)::CTMP6
C
      REAL(8)::OUTDT,OUTET
      INTEGER::I,IE,IERR,IS,N,NTMP,NTMP2,INSIDE,IDUM,JDUM
      INTEGER:: NH1,NH2,NDAT
      DATA NH1,NH2 /0.0D0,0.0D0/
C
C
      DO 100 N=1,100000
         CALL GET1(IS,IE,IERR)
         IF( IERR.GT.0 ) GO TO 900
c         write(*,*) 'debug:inoutp:variable=',cline(is:ie)
C
         IF( CLINE(IS:IE) .EQ. '%END' ) THEN
            GO TO 200
C
         ELSE IF( CLINE(IS:IE) .EQ. 'RESTART-TIME' ) THEN
            CALL MGETR(RREST,NREST,NRSTSZ)
            LREST  = 1
            IREST0 = 1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'RESTART-STEP' ) THEN
            CALL MGETI(IREST,NREST,NRSTSZ)
            LREST  = 0
            IREST0 = 1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'RESTART-ELAPSE' ) THEN
            CALL GETR(ETIME)
            IREST0 = 1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MAM-TIME-START' ) THEN
            CALL GETR(RMAMS)
            IMMTYP = 2
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MAM-TIME-END' ) THEN
            CALL GETR(RMAME)
            IMMTYP = 2
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MAM-TIME-INTERVAL' ) THEN
            CALL GETR(RMAMI)
            IMMTYP = 2
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MAM-STEP-START' ) THEN
            CALL GETI(IMAMS)
            IMMTYP = 1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MAM-STEP-END' ) THEN
            CALL GETI(IMAME)
            IMMTYP = 1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MAM-STEP-INTERVAL' ) THEN
            CALL GETI(IMAMI)
            IMMTYP = 1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'LIST-TYPE' ) THEN
            CALL GETC(CTMP6,6)
            IF( CTMP6.EQ.'ASCII ' ) THEN
               LISTT = 0
            ELSE IF( CTMP6.EQ.'BINARY' ) THEN
               LISTT = 1
            ELSE
               CALL ERRMSG('INOUTP',6670)
               WRITE(LP,*) 'VALUE MUST BE ASCII OR BINARY'
               WRITE(LP,*) 'VARIABLE=LIST-TYPE'
               WRITE(LP,*) 'VALUE=',CTMP6
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'LIST-TIME' ) THEN
            CALL MGETR(RLIST,NLIST,NOUTSZ)
            LLIST  = 1
            ILIST0 = 1
C
            IF((NLIST.EQ.3).AND.
     1         (RLIST(1).LT.RLIST(2)).AND.RLIST(2).GT.RLIST(3)) THEN
                OUTDT=RLIST(3)
                OUTET=RLIST(2)
                DO 350 I=2,NOUTSZ
                  RLIST(I)=RLIST(I-1)+OUTDT
                  NLIST = I
                  IF(RLIST(I).GT.OUTET) GO TO 360
  350                        CONTINUE
                IF(RLIST(NOUTSZ)+OUTDT.LT.OUTET) THEN
                  CALL ERRMSG('INOUTP',6671)
                  WRITE(LP,*) 'NUMBER OG LIST-OUT TIME IS OVER'
                  WRITE(LP,*) 'VARIABLE=LIST-TIME'
                  CALL ABORT1('')
                END IF
  360                      CONTINUE
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'LIST-STEP' ) THEN
            CALL MGETI(ILIST,NLIST,NOUTSZ)
            LLIST  = 0
            ILIST0 = 1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'END-TIME' ) THEN
            CALL MGETR(RENDF,NENDF,NOUTSZ)
            LENDF  = 1
            IENDF0 = 1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'END-INUNDATION' ) THEN
            CALL MGETR(DFRAGL,NFRAGL,MAXFRAGL)
            DO I=1,NFRAGL
               IF( DFRAGL(I).LE.EPST ) THEN
                  CALL ERRMSG('INOUTP',6684)
                  WRITE(LP,*) 'VALUE MUST BE > ',EPST
                  WRITE(LP,*) 'VALUE=',DFRAGL(I)
                  WRITE(LP,*) 'VARIABLE=END-INUNDATION'
                  WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')                  
               ENDIF
            ENDDO
C
         ELSE IF( CLINE(IS:IE) .EQ. 'END-STEP' ) THEN
            CALL MGETI(IENDF,NENDF,NOUTSZ)
            LENDF  = 0
            IENDF0 = 1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'LIST-PHYS' ) THEN
            CALL MGETC(CLIST,8,MLIST,60)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'LIST-SECT-I' ) THEN
            CALL MGETI(ITMP,NTMP,NPNTSZ)
C
            DO I=1,NTMP
               CALL MODI(ITMP(I),INSIDE)
               IF( INSIDE.EQ.0 ) ITMP(I)=-1
            ENDDO
C
            IF( NLSECT+NTMP.GT.NPNTSZ ) THEN
               CALL ERRMSG('INOUTP',6672)
               WRITE(LP,*) 'THE NUMBER OF 2D-SECTIONS MAY ',
     $                     'NOT BE OVER NPNTSZ'
               WRITE(LP,*) 'VARIABLE=LIST-SECT-I'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            NTMP2=0
            DO 300 I=1,NTMP
               IF(ITMP(I).LT.0) GOTO 300
               NTMP2=NTMP2+1
               ILSECT(1,NLSECT+I) = 1
               ILSECT(2,NLSECT+I) = ITMP(I)
  300       CONTINUE
            NLSECT = NLSECT + NTMP2
C
         ELSE IF( CLINE(IS:IE) .EQ. 'LIST-SECT-J' ) THEN
            CALL MGETI(ITMP,NTMP,NPNTSZ)
C
            DO I=1,NTMP
               CALL MODJ(ITMP(I),INSIDE)
               IF( INSIDE.EQ.0 ) ITMP(I)=-1
            ENDDO
C
            IF( NLSECT+NTMP.GT.NPNTSZ ) THEN
               CALL ERRMSG('INOUTP',6673)
               WRITE(LP,*) 'THE NUMBER OF 2D-SECTIONS MAY ',
     $                     'NOT BE OVER NPNTSZ'
               WRITE(LP,*) 'VARIABLE=LIST-SECT-J'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            NTMP2=0
            DO 310 I=1,NTMP
               IF(ITMP(I).LT.0) GOTO 310
               NTMP2=NTMP2+1
               ILSECT(1,NLSECT+I) = 2
               ILSECT(2,NLSECT+I) = ITMP(I)
  310       CONTINUE
            NLSECT = NLSECT + NTMP2
C
         ELSE IF( CLINE(IS:IE) .EQ. 'LIST-SECT-K' ) THEN
            CALL MGETI(ITMP,NTMP,NPNTSZ)
            IF( NLSECT+NTMP.GT.NPNTSZ ) THEN
               CALL ERRMSG('INOUTP',6674)
               WRITE(LP,*) 'THE NUMBER OF 2D-SECTIONS MAY ',
     $                     'NOT BE OVER NPNTSZ'
               WRITE(LP,*) 'VARIABLE=LIST-SECT-K'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            DO 320 I=1,NTMP
               ILSECT(1,NLSECT+I) = 3
               ILSECT(2,NLSECT+I) = ITMP(I)
  320       CONTINUE
            NLSECT = NLSECT + NTMP
C
C
         ELSE IF( CLINE(IS:IE) .EQ. 'GRAPHIC-TIME' ) THEN
            CALL MGETR(RGRPH,NGRPH,NOUTSZ)
            LGRPH  = 1
            IGRPH0 = 1
            IF((NGRPH.EQ.3).AND.
     1         (RGRPH(1).LT.RGRPH(2)).AND.RGRPH(2).GT.RGRPH(3)) THEN
                OUTDT=RGRPH(3)
                OUTET=RGRPH(2)
                DO 330 I=2,NOUTSZ
                  RGRPH(I)=RGRPH(I-1)+OUTDT
                  NGRPH = I
                  IF(RGRPH(I).GT.OUTET) GO TO 340
  330           CONTINUE
                IF(RGRPH(NOUTSZ)+OUTDT.LT.OUTET) THEN
                  CALL ERRMSG('INOUTP',6675)
                  WRITE(LP,*) 'NUMBER OG GRAPHIC-OUT TIME IS OVER'
                  WRITE(LP,*) 'VARIABLE=GRAPHIC-TIME'
                  CALL ABORT1('')
                END IF
  340           CONTINUE
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'GRAPHIC-STEP' ) THEN
            CALL MGETI(IGRPH,NGRPH,NOUTSZ)
            LGRPH  = 0
            IGRPH0 = 1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'GRAPHIC-PHYS' ) THEN
            CALL MGETC(CGRPH,8,MGRPH,60)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'GRAPHIC-ALFA-FLOW' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'NO ' ) THEN
               IALFAFLOW = 0
            ELSE IF( CTMP.EQ.'YES' ) THEN
               IALFAFLOW = 1
            ELSE
               CALL ERRMSG('INOUTP',6676)
               WRITE(LP,*) 'VALUE MUST BE YES OR NO'
               WRITE(LP,*) 'VARIABLE=GRAPHIC-ALOFA-FLOW'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'HISTORY-TIME' ) THEN
            CALL GETR(RHIST)
            LHIST  = 1
            RHIST0 = 0.0D0
            IHIST0 = 1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'HISTORY-STEP' ) THEN
            CALL GETI(IHIST)
            LHIST  = 0
            IHIST0 = 1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'HISTORY-PHYS' ) THEN
            CALL MGETC(CHIST,8,MHIST,60)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'HISTORY-CELL' ) THEN
            CALL MGETI(ITMP,NTMP,3)
            IF( NTMP.NE.3 ) THEN
               CALL ERRMSG('INOUTP',6677)
               WRITE(LP,*) 'CELL INDEX FORMAT MUST BE (I J K)'
               WRITE(LP,*) 'VARIABLE=HISTORY-CELL'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
            NH1=NH1+1
            IDUM=ITMP(1)
            JDUM=ITMP(2)
            CALL MODIJ(ITMP(1),IDUM,ITMP(2),JDUM,0,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
C
            NHCELL = NHCELL + 1
            IF( NHCELL.GT.NPNTSZ ) THEN
               CALL ERRMSG('INOUTP',6678)
               WRITE(LP,*) 'THE NUMBER OF CELL MAY ',
     $                     'NOT BE OVER NPNTSZ'
               WRITE(LP,*) 'VARIABLE=HISTORY-CELL'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            IHCELL(1,NHCELL) = ITMP(1)
            IHCELL(2,NHCELL) = ITMP(2)
            IHCELL(3,NHCELL) = ITMP(3)
            LHCELL(NHCELL)   = NH1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'BC-FILE' ) THEN
            CALL MGETR(RTMP,NTMP,3)
            IF( NTMP.NE.3 ) THEN
               CALL ERRMSG('INOUTP',6679)
               WRITE(LP,*) 'BC-FILE DATA MUST BE ( STIM ETIM DTIM )'
               WRITE(LP,*) 'VARIABLE=BC-FILE'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            RFILE(1) = RTMP(1)
            RFILE(2) = RTMP(2)
            RFILE(3) = RTMP(3)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'KENSA-MODE' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'NO ' ) THEN
               KENSAMODE = 0
            ELSE IF( CTMP.EQ.'YES' ) THEN
               KENSAMODE = 1
            ELSE
               CALL ERRMSG('INOUTP',6680)
               WRITE(LP,*) 'VALUE MUST BE YES OR NO'
               WRITE(LP,*) 'VARIABLE=KENSA-MODE'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
C=================================================FOR OIL_PARTICLE START
         ELSE IF( CLINE(IS:IE) .EQ. 'OILFILE-TIME' ) THEN
           CALL MGETR(ROILF,NOILF,3)
           IF(NOILF.NE.3) THEN
             CALL ERRMSG('INOUTP',6681)
             WRITE(LP,*) 'OILFILE-TIME DATA MUST BE ( STIM ETIM DTIM )'
             CALL ABORT1('')
           END IF
C=================================================FOR OIL_PARTICLE END
C
         ELSE
            CALL ERRMSG('INOUTP',6682)
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
      CALL ERRMSG('INOUTP',6683)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
      END
