      SUBROUTINE INMODL(IRTRN)
C======================================================================
C     各項やモデルの計算の有無、パラメータ等を読み込む
C     入力データの追加方法は README_INPUT を参照
C======================================================================
      use mod_comm,only:my_model,l_stoc_ic
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'TURBR.h'
      INCLUDE 'MYCNST.h'
      INCLUDE 'GRID.h'
      INCLUDE 'VVMAX.h'
      INCLUDE 'DESTROY.h'
      INCLUDE 'OUTPUT.h'
C
      REAL(8),PARAMETER::PAI = 3.141592653897932D0
      INTEGER,INTENT(INOUT)::IRTRN
      REAL(8)::RTMP(10)
      CHARACTER(3)::CTMP
      CHARACTER(8)::CTMP2
      CHARACTER(5)::CTMP3
C
      INTEGER::I,IAB,IE,IERR,IS,N,NDAT1
C
C
      IAB = 0
      DO 100 N=1,100000
         CALL GET1(IS,IE,IERR)
         IF( IERR.GT.0 ) GO TO 900
c         write(*,*) 'debug:inmodl:variable=',cline(is:ie)
C
         IF( CLINE(IS:IE) .EQ. '%END' ) THEN
            GO TO 200
C
         ELSE IF( CLINE(IS:IE) .EQ. 'SURFACE' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'OFF' ) THEN
               LSURF = 0
            ELSE IF( CTMP.EQ.'ON ' ) THEN
               LSURF = 1
            ELSE
               CALL ERRMSG('INMODL',6590)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE=SURFACE'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'TYPHOON' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'OFF' ) THEN
               LTYPH = 0
            ELSE IF( CTMP.EQ.'ON ' ) THEN
               LTYPH = 1
            ELSE
               CALL ERRMSG('INMODL',6591)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE=TYPHOON'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'TURBULENT' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'OFF' ) THEN
               LTURB = 0
            ELSE IF( CTMP.EQ.'ON ' .OR. CTMP.EQ.'LES' ) THEN
               LTURB = 1
            ELSE IF( CTMP.EQ.'K-E' ) THEN
               LTURB = 2
            ELSE IF( CTMP.EQ.'M-Y' ) THEN
               LTURB = 3
            ELSE IF( CTMP.EQ.'SGS' ) THEN
               LTURB = 4
            ELSE
               CALL ERRMSG('INMODL',6592)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF OR K-E OR M-Y',
     $                                   ' OR SGS'
               WRITE(LP,*) 'VARIABLE=TURBULENT'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'K-EPS.COEF' ) THEN
            CALL MGETR(RTMP,NDAT1,10)
            IF( NDAT1.NE.10 ) THEN
               CALL ERRMSG('INMODL',6593)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 10'
               WRITE(LP,*) 'VARIABLE=K-EPS.COEF'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
            CMU = RTMP(1)
            SGK = RTMP(2)
            SGE = RTMP(3)
            SGT = RTMP(4)
            TC1 = RTMP(5)
            TC2 = RTMP(6)
            TC3 = RTMP(7)
            TCE = RTMP(8)
            AKMIN = RTMP(9)
            EPMIN = RTMP(10)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'TURB-VISC' ) THEN
            CALL MGETR(RTMP,NDAT1,10)
            IF( NDAT1.NE.3 ) THEN
               CALL ERRMSG('INMODL',6594)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 3'
               WRITE(LP,*) 'VARIABLE=TURB-VISC'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
            TVSMAX = RTMP(1)
            TVSMIN = RTMP(2)
            TVSVMX = RTMP(3)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'SGS-COEF' ) THEN
            CALL MGETR(RTMP,NDAT1,10)
            IF( NDAT1.NE.4 ) THEN
               CALL ERRMSG('INMODL',6595)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 4'
               WRITE(LP,*) 'VARIABLE=SGS-COEF'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
            CSMG = RTMP(1)
            TCE  = RTMP(2)
            PRT  = RTMP(3)
            SCT  = RTMP(4)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'TEMPERATURE' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'OFF' ) THEN
               LTEMP = 0
            ELSE IF( CTMP.EQ.'ON ' ) THEN
               LTEMP = 1
            ELSE
               CALL ERRMSG('INMODL',6596)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE=TEMPERATURE'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'CONCENTRATION' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'OFF' ) THEN
               LCONC = 0
            ELSE IF( CTMP.EQ.'ON ' ) THEN
               LCONC = 1
            ELSE
               CALL ERRMSG('INMODL',6597)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE=CONCENTRATION'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'SEA-WALL' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'OFF' ) THEN
               ISEAWL = 0
            ELSE IF( CTMP.EQ.'ON ' ) THEN
               ISEAWL = 1
            ELSE
               CALL ERRMSG('INMODL',6598)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE=SEA-WALL'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'OVERFLOW-HONMA' ) THEN
            CALL GETC(CTMP3,5)
            IF( CTMP3.EQ.'OFF  ' ) THEN
               IHONMA = 0
            ELSE IF( CTMP3.EQ.'FIX  ' ) THEN
               IHONMA = 1
            ELSE IF( CTMP3.EQ.'LIMIT' ) THEN
               IHONMA = 2
            ELSE
               CALL ERRMSG('INMODL',6599)
               WRITE(LP,*) 'VALUE MUST BE LIMIT OR FIX OR OFF'
               WRITE(LP,*) 'VARIABLE=OVERFLOW-HONDA'
               WRITE(LP,*) 'VALUE=',CTMP3
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'OVERFLOW-AIDA' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'OFF' ) THEN
               IAIDA = 0
            ELSE IF( CTMP.EQ.'ON ' ) THEN
               IAIDA = 1
            ELSE
               CALL ERRMSG('INMODL',6600)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE=OVERFLOW-AIDA'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'AIDA-LIMIT' ) THEN
            CALL GETR(HAIDA)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'OVERFLOW-BACKSTEP' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'OFF' ) THEN
               IBKSTP = 0
            ELSE IF( CTMP.EQ.'ON ' ) THEN
               IBKSTP = 1
            ELSE
               CALL ERRMSG('INMODL',6601)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE=OVERFLOW-BACKSTEP'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'IMPLICIT-VERTICAL' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'OFF' ) THEN
               IMVERT = 0
            ELSE IF( CTMP.EQ.'ON ' ) THEN
               IMVERT = 1
            ELSE
               CALL ERRMSG('INMODL',6602)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE=IMPLICIT-VERTICAL'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'BREAK-WAVE' ) THEN
            CALL GETC(CTMP2,7)
            IF( CTMP2.EQ.'OFF    ' ) THEN
               LBRKW = 0
            ELSE IF( CTMP2.EQ.'KENNEDY' ) THEN
               LBRKW = 1
            ELSE IF( CTMP2.EQ.'IWASE  ' ) THEN
               LBRKW = 2
            ELSE
               CALL ERRMSG('INMODL',6603)
               WRITE(LP,*) 'VALUE MUST BE OFF OR KENNEDY OR IWASE'
               WRITE(LP,*) 'VARIABLE=BREAK-WAVE'
               WRITE(LP,*) 'VALUE=',CTMP2
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'KENNEDY-DELTA' ) THEN
            CALL GETR(DKENNEDY)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'KENNEDY-COEF1' ) THEN
            CALL GETR(DKENN1)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'KENNEDY-COEF2' ) THEN
            CALL GETR(DKENN2)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'KENNEDY-COEF3' ) THEN
            CALL GETR(DKENN3)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'IWASE-BETA' ) THEN
            CALL GETR(BETAIWA)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DTSAFE-BREAK' ) THEN
            CALL GETR(SAFEBRKW)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DENSITY' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'OFF' ) THEN
               LDENS = 0
            ELSE IF( CTMP.EQ.'ON ' ) THEN
               LDENS = 1
            ELSE IF( CTMP.EQ.'KND' ) THEN
               LDENS = 2
            ELSE IF( CTMP.EQ.'KN2') THEN
               LDENS = 3
            ELSE IF( CTMP.EQ.'BRY') THEN
               LDENS = 4
            ELSE
               CALL ERRMSG('INMODL',6604)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF OR KND KN2 OR BRY'
               WRITE(LP,*) 'VARIABLE=DENSITY'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'GRAVITY' ) THEN
            CALL GETR(GRAV)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'GAMMAS' ) THEN
            CALL GETR(GM2S)
            IF( GM2S.EQ.0.0D0 ) THEN
               IGM2S = 0
            ELSE IF( GM2S.GT.0.0D0 ) THEN
               IGM2S = 1
            ELSE
               IGM2S = -1
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'GAMMAB' ) THEN
            CALL GETR(GM2B)
            IF( GM2B.EQ.0.0D0 ) THEN
               IGM2B = 0
            ELSE IF( GM2B.GT.0.0D0 ) THEN
               IGM2B = 1
            ELSE
               IGM2B = -1
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'D-POROUS-MODEL' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'CDM' ) THEN
               LDISS = 0
            ELSE IF( CTMP.EQ.'DF ' ) THEN
               LDISS = 1
            ELSE
               CALL ERRMSG('INMODL',6605)
               WRITE(LP,*) 'VALUE MUST BE CDM OR DF'
               WRITE(LP,*) 'VARIABLE=DISSIPATING-MODEL'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'CORIOLI' ) THEN
            CALL GETR(CORI)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'LATITUDE' ) THEN
            CALL GETR(RTMP(1))
            CORI=2.0D0*CEARTH*SIN(RTMP(1)/180.0D0*PAI)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'PARAM-SCHEME-F' ) THEN
            CALL GETR(PARAMF)
            PARAMF2 = 1.0D0-PARAMF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'PARAM-SCHEME-V' ) THEN
            CALL GETR(PARAMV)
            PARAMV2 = 1.0D0-PARAMV
C
         ELSE IF( CLINE(IS:IE) .EQ. 'PARAM-SCHEME-K' ) THEN
            CALL GETR(PARAMK)
            PARAMK2 = 1.0D0-PARAMK
C
         ELSE IF( CLINE(IS:IE) .EQ. 'PARAM-SCHEME-T' ) THEN
            CALL GETR(PARAMT)
            PARAMT2 = 1.0D0-PARAMT
C
         ELSE IF( CLINE(IS:IE) .EQ. 'PARAM-SCHEME-C' ) THEN
            CALL GETR(PARAMC)
            PARAMC2 = 1.0D0-PARAMC
C
         ELSE IF( CLINE(IS:IE) .EQ. 'TURB-PRANDTL' ) THEN
            CALL GETR(PRT)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'TURB-SCHMIDT' ) THEN
            CALL GETR(SCT)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'TURB-SMAGO' ) THEN
            CALL GETR(CSMG)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'RUNUP-GXB' ) THEN
            CALL GETR(GXB)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'RUNUP-GXH' ) THEN
C            CALL GETR(GXH)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'RUNUP-GDH' ) THEN
C            CALL GETR(GDH)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'RUNUP-GLH' ) THEN
            CALL GETR(GLH)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'RUNUP-GZH' ) THEN
            CALL GETR(GZH)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'RUNUP-EPSH' ) THEN
            CALL GETR(EPSH)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'RUNUP-EPST' ) THEN
            CALL GETR(EPST)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'RIMP' ) THEN
            CALL GETR(RIMP)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MANNING-FM' ) THEN
            NFN = NFN + 1
            IF( NFN.GT.NFNSIZ ) THEN
               CALL ERRMSG('INMODL',6606)
               WRITE(LP,*) 'THE NUMBER OF MANNING ROUGH. AREA MAY ',
     $                     'NOT BE OVER NFNSIZ'
               WRITE(LP,*) 'VARIABLE=FN'
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
            CALL MGETR(RTMP,NDAT1,5)
            IF( NDAT1.NE.5 .AND. NDAT1.NE.1 ) THEN
               CALL ERRMSG('INMODL',6607)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 5'
               WRITE(LP,*) 'VARIABLE=FN'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
            FNVAL(NFN) = RTMP(1)
            IF( NDAT1.EQ.5 ) THEN
               IFNTBL(1,NFN) = NINT(RTMP(2))
               IFNTBL(2,NFN) = NINT(RTMP(3))
               IFNTBL(3,NFN) = NINT(RTMP(4))
               IFNTBL(4,NFN) = NINT(RTMP(5))
            ELSE
               IFNTBL(1,NFN) = 0
               IFNTBL(2,NFN) = 0
               IFNTBL(3,NFN) = 0
               IFNTBL(4,NFN) = 0
            END IF
         ELSE IF( CLINE(IS:IE) .EQ. 'ISW' ) THEN
            CALL MGETI(ISW,NDAT1,5)
            IF( NDAT1.NE.5 ) THEN
               CALL ERRMSG('INMODL',6608)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 5'
               WRITE(LP,*) 'VARIABLE=ISW'
               WRITE(LP,*) 'VALUE=',(ISW(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
         ELSE IF( CLINE(IS:IE) .EQ. 'DENS-COEF' ) THEN
            CALL MGETR(RTMP,NDAT1,5)
            IF( NDAT1.NE.4 ) THEN
               CALL ERRMSG('INMODL',6609)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 5'
               WRITE(LP,*) 'VARIABLE=DENS-COEF.'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
            AABB(1) = RTMP(1)
            AABB(2) = RTMP(2)
            AABB(3) = RTMP(3)
            AABB(4) = RTMP(4)
            IAB     = 1
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MAX-VELOCITY' ) THEN
            CALL GETR(VVMAX)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'FALL-WATER-MODEL' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'OFF' ) THEN
               IFALLW = 0
            ELSE IF( CTMP.EQ.'ON ' ) THEN
               IFALLW = 1
            ELSE
               CALL ERRMSG('INMODL',6610)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE=FALL-WATER-MODEL'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'TYPE-FALL-WATER' ) THEN
            CALL GETC(CTMP2,8)
            IF( CTMP2.EQ.'CONSTANT' ) THEN
               JFALLW = 0
            ELSE IF( CTMP2.EQ.'FILE    ' ) THEN
               JFALLW = 1
            ELSE
               CALL ERRMSG('INMODL',6611)
               WRITE(LP,*) 'VALUE MUST BE CONSTANT OR FILE'
               WRITE(LP,*) 'VARIABLE=TYPE-FALL-WATER'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'COEF-FALL-WATER' ) THEN
            CALL GETR(CFALLW0)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DIST-NORM-FALL-WATER' ) THEN
            CALL GETR(DFALLWN0)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DIST-TANG-FALL-WATER' ) THEN
            CALL GETR(DFALLWT0)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DISP-LIMIT' ) THEN
            CALL GETR(DISPLIM)
            IF( DISPLIM.GE.0 ) THEN
               CALL ERRMSG('INMODL',6612)
               WRITE(LP,*) 'DISP-LIMIT MUST BE < 0'
               WRITE(LP,*) 'VARIABLE=DISP-LIMIT'
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DISP-BC-LIMIT' ) THEN
            CALL GETR(DISPBCLIM)
            IF( DISPBCLIM.GE.0 ) THEN
               CALL ERRMSG('INMODL',6613)
               WRITE(LP,*) 'DISP-BC-LIMIT MUST BE < 0'
               WRITE(LP,*) 'VARIABLE=DISP-BC-LIMIT'
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DISPBETA' ) THEN
            CALL GETR(DISPBETA)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DISPERSIVE-WAVE' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'OFF' ) THEN
               LDISP = 0
            ELSE IF( CTMP.EQ.'ON ' ) THEN
               LDISP = 1
            ELSE
               CALL ERRMSG('INMODL',6614)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE=DISPERSIVE-WAVE'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'STOC-DS-MODE' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'OFF' ) THEN
               LSTOCDS = 0
            ELSE IF( CTMP.EQ.'ON ' ) THEN
               LSTOCDS = 1
            ELSE
               CALL ERRMSG('INMODL',6615)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE=STOC-DS-MODE'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               IRTRN = 9
            END IF
            IF( my_model.eq.l_stoc_ic ) then
               CALL ERRMS2('INMODL',6616)
               WRITE(LP,*) 'STOC-DS-MODE IS ONLY APPLICABLE IN STOC-ML'
               LSTOCDS=0
            ENDIF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DST-LIMIT' ) THEN
            CALL MGETR(RTMP,NDAT1,5)
            DSTLMT(1:NDAT1)=RTMP(1:NDAT1)
C
         ELSE
            CALL ERRMSG('INMODL',6617)
            WRITE(LP,*) 'UNKNOWN VARIABLE NAME: ',CLINE(IS:IE)
            IRTRN = 9
         END IF
  100 CONTINUE
  200 CONTINUE
C
C ... チェック用の値を設定
      IF(LDENS.EQ.1.AND.IAB.EQ.0) THEN
        IF(MLNS.EQ.1) THEN
          IF(LTEMP.EQ.1.AND.LCONC.EQ.0) THEN
            AABB(1) = 0.071D0
            AABB(2) = 0.0D0
            AABB(3) = 0.0D0
            AABB(4) = 0.0D0
          END IF
          IF(LTEMP.EQ.0.AND.LCONC.EQ.1) THEN
            AABB(1) = 0.0D0
            AABB(2) = 0.0D0
            AABB(3) = -0.071D0
            AABB(4) = 0.0D0
          END IF
        ELSE
          CALL ERRMSG('INMODL',6618)
          WRITE(LP,*) 'DENSITY  ON  NEED  DENS-COEF DATA'
          IRTRN = 9
        END IF
      END IF
C
      IF(MLNS.EQ.0.AND.LTURB.EQ.2) THEN
        WRITE(LP,*) 'STOC-ML CAN NOT CHOOSE K-E MODEL'
        IRTRN = 9
      ELSE IF(MLNS.EQ.0.AND.LTURB.EQ.4) THEN
        WRITE(LP,*) 'STOC-ML CAN NOT CHOOSE SGS MODEL'
        IRTRN = 9
      ELSE IF(MLNS.GT.0.AND.LTURB.EQ.3) THEN
        WRITE(LP,*) 'STOC-IC CAN NOT CHOOSE M-Y MODEL'
        IRTRN = 9
      END IF
C
      IF(LTURB.EQ.4) THEN
        IF(PRT.EQ.0.9D0) PRT=0.7D0
        IF(SCT.EQ.0.7D0) SCT=PRT
        IF(TCE.EQ.0.0D0) TCE=0.31D0
        IF(CSMG.EQ.0.2D0) CSMG=0.12D0
      END IF
C
      IF( LSURF.EQ.0.AND.IMVERT.NE.0 ) THEN
         IMVERT = 0
      ENDIF
C
      RETURN
C
C ... 読み込みエラー
  900 CONTINUE
      CALL ERRMSG('INMODL',6619)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      IRTRN = 9
      RETURN
      END
