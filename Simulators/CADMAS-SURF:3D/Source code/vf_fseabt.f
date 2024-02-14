      SUBROUTINE VF_FSEABT(XX,YY,ZZ,FF,GGV,DELH,DELH_IN,NF)

CD=== 概要 ===========================================================

CDT   VF_FEULER:VOF関数Fの地形変化による生成消滅項を計算

C==== 宣言 ===========================================================
      USE MOD_FAULT,ONLY: NFLT,NFLTNOW,FPARAM

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSI.h'
      INCLUDE 'VF_ATIMER.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ASEABT.h'

CD    -- 引数 --
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    FF(@FOR-3D@)     : I/O : R*8 : VOF関数F
CD    DELH(NUMI0,NUMJ0)    : INOUT : R*8 : 水位変化
CD    DELH_IN(NUMI0,NUMJ0) : OUT   : R*8 : 水位変化のファイル読み込み値
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION FF(NUMI,NUMJ,NUMK),GGV(NUMI,NUMJ,NUMK)
      DIMENSION DELH(NUMI0,NUMJ0),DELH_IN(NUMI0,NUMJ0)
      DIMENSION NF(NUMI,NUMJ,NUMK)

      DOUBLE PRECISION EPS
      DATA EPS/1.0D-6/

      DOUBLE PRECISION TIME_NEXT
      DATA TIME_NEXT/-1.0D0/
      SAVE TIME_NEXT
cadd20130529(start)
      DATA TIME_NOW /-1.0D0/
      SAVE TIME_NOW
cadd20130529(end)
      INTEGER ISBTFORM,MFILBTX
      CHARACTER(64) SUBFILE
      DATA ISBTFORM /0/
      SAVE ISBTFORM,MFILBTX
      SAVE SUBFILE
      CHARACTER(132) CLINE
      DOUBLE PRECISION TIM1,dummy,DHMIN,DHMAX
C==== 実行 ===========================================================

CD 地形変化ファイルのOPENと1行目の読み込み

C ... 最初に1回だけ行う処理(ここから)
      IF(TIME_NEXT<0.0D0) THEN
      IF( ISEABT.EQ.-1 ) THEN
        TIM1=TNOW-DTNOW
        IF( TNOW.EQ.0.0D0 ) TIM1=-1.0D0
        CALL VF_FAULTI(TNOW,TIME_NEXT,TIM1)
C
      ELSE
        MFILBTX=MFILBT
C
        OPEN(MFILBT,FILE=MGNAME(MGRANK+1)(1:MGNLEN(MGRANK+1))//'.sbt'
     &      ,FORM='FORMATTED',STATUS='OLD',ACTION='READ',IOSTAT=IERR)
        IF(IERR.NE.0) THEN
          WRITE(ILPFIL,*) 'SBT FILE OPEN ERROR'
          CALL VF_A2ERR('VF_FSEABT','OPEN ERROR.')
        ENDIF
C
! 1行目を読み込んでフォーマットをチェック(従来:ISBTFORM=0,分割形式:ISBTFORM=1)                                     
        READ(MFILBT,'(A132)',IOSTAT=IERR) CLINE
        IF(IERR.NE.0) THEN
          WRITE(ILPFIL,*) 'SBT FILE READ ERROR'
          CALL VF_A2ERR('VF_FSEABT','READ ERROR.')
        ENDIF
        REWIND(MFILBT)
        READ(CLINE,*,ERR=70,END=70) TIME_NEXT,IIS,IIE,JJS,JJE,SUBFILE
        ISBTFORM=1
        MFILBTX=MFILBT2
   70   CONTINUE
C
C
        IF(ISBTFORM.EQ.0) THEN
           READ(MFILBT,*,IOSTAT=IERR) TIME_NEXT,IIS,IIE,JJS,JJE
        ELSE
           READ(MFILBT,*,IOSTAT=IERR) TIME_NEXT,IIS,IIE,JJS,JJE,SUBFILE
        ENDIF
C
        IF(IERR.NE.0) THEN
          WRITE(ILPFIL,*) 'SBT FILE READ ERROR'
          CALL VF_A2ERR('VF_FSEABT','READ ERROR.')
        ENDIF

        IF(IIS.NE.2       .OR. JJS.NE.2       .OR.
     &     IIE.NE.NUMI0-1 .OR. JJE.NE.NUMJ0-1 ) THEN
          WRITE(ILPFIL,*) 'SBT FILE DIMENSION ERROR'
          CALL VF_A2ERR('VF_FSEABT','DIMENSION ERROR.')
        ENDIF

C ..... リスタート時はリスタート時刻以前のデータを読み飛ばすためにループ処理を行う
        TIM1=TNOW-DTNOW
        IF( TNOW.EQ.0.0D0 ) TIM1=-1.0D0
 10     CONTINUE
        IF(TIM1.GE.TIME_NEXT) THEN
          if(mgrank.eq.0)
     $      write(ILPFIL,*)
     $      'FSEABT: SKIP AT TNOW,TIME_NEXT =',TNOW,TIME_NEXT

          IF( ISBTFORM.EQ.0 ) THEN
          READ(MFILBT,*,IOSTAT=IERR)
     &      ((dummy,IG=2,NUMI0-1),JG=2,NUMJ0-1)
          IF(IERR.NE.0) THEN
            WRITE(ILPFIL,*) 'SBT FILE READ ERROR'
            CALL VF_A2ERR('VF_FSEABT','READ ERROR.')
          END IF
          END IF

          IF(ISBTFORM.EQ.0) THEN
            READ(MFILBT,*,IOSTAT=IERR) TIME_NEXT,IIS,IIE,JJS,JJE
          ELSE
            READ(MFILBT,*,IOSTAT=IERR) TIME_NEXT,IIS,IIE,JJS,JJE,SUBFILE
          ENDIF

          IF(IERR.LT.0) THEN
            TIME_NEXT= 1.0D30
          ELSE IF(IERR.GT.0) THEN
            WRITE(ILPFIL,*) 'SBT FILE READ ERROR'
            CALL VF_A2ERR('VF_FSEABT','READ ERROR.')
          END IF
          GO TO 10
        END IF

      ENDIF
      ENDIF
C ... 最初に1回だけ行う処理(ここまで)

CD    -- 現時刻がファイルの時刻以上になったら、水位変化を1セット読み込む
      IF(TNOW.GE.TIME_NEXT) THEN
        TIME_NOW = TIME_NEXT
        DELH_IN(:,:)=0.0D0

        IF( ISEABT.EQ.-1 ) THEN
           CALL VF_FAULTT(DELH_IN,TNOW,XX,YY)
           IF(NFLTNOW.GT.NFLT) THEN
              IERR=-1
           ELSE
              TIME_NEXT=FPARAM(10,NFLTNOW)
              IERR=0
           ENDIF

        ELSE
C ..... 分割形式の場合、一時ファイルを開く
        IF( ISBTFORM.EQ.1 )
     $    OPEN(MFILBTX,FILE=trim(SUBFILE),FORM='FORMATTED',STATUS='OLD')

        READ(MFILBTX,*,IOSTAT=IERR)
     &    ((DELH_IN(IG,JG),IG=2,NUMI0-1),JG=2,NUMJ0-1)
        IF(IERR.NE.0) THEN
          WRITE(ILPFIL,*) 'SBT FILE READ ERROR'
          CALL VF_A2ERR('VF_FSEABT','READ ERROR.')
        END IF

C ..... 分割形式の場合、一時ファイルを閉じる
        IF( ISBTFORM.EQ.1 ) CLOSE(MFILBTX)
        ENDIF ! (ISEABT.EQ.-1)

        if(mgrank.eq.0)
     $     write(ILPFIL,*)
     $    'FSEABT: DELH ADD AT TNOW,TIME_NEXT =',TNOW,TIME_NEXT

        DO JG=2,NUMJ0-1
          DO IG=2,NUMI0-1
            DELH(IG,JG) = DELH(IG,JG) + DELH_IN(IG,JG)
          ENDDO
        ENDDO

        IF( ISEABT.NE.-1 ) THEN
        IF(ISBTFORM.EQ.0) THEN
           READ(MFILBT,*,IOSTAT=IERR) TIME_NEXT,IIS,IIE,JJS,JJE
        ELSE
           READ(MFILBT,*,IOSTAT=IERR) TIME_NEXT,IIS,IIE,JJS,JJE,SUBFILE
        ENDIF
        ENDIF ! ( ISEABT.NE.-1 ) 
        if(mgrank.eq.0)
     $     write(ILPFIL,*) 'FSEABT: NEXT TIME_NEXT =',TIME_NEXT
        IF(IERR.LT.0) THEN
          TIME_NEXT= 1.0D30
        ELSE IF(IERR.GT.0) THEN
          WRITE(ILPFIL,*) 'SBT FILE READ ERROR'
          CALL VF_A2ERR('VF_FSEABT','READ ERROR.')
        END IF
      END IF

CD    -- 初期の水位変化分を設定する

      IF(TIME_NOW.EQ.0.0D0) THEN

        DO 410 J=MYJS,MYJE
          DO 400 I=MYIS,MYIE
            IG = I + MYGIS - 1
            JG = J + MYGJS - 1 

            ISURF = 0
            DO 300 K=2,NUMK-1
              IF(FF(I,J,K).GT.0.0D0) THEN
                ISURF = 1
                EXIT
              ENDIF
 300        CONTINUE

            IF(ISURF.EQ.1) THEN
              IF(DELH(IG,JG).GT.0.0D0) THEN
                DO 310 K=2,NUMK-1
                  DF = MIN(DELH(IG,JG)/ZZ(2,K),
     $                     GGV(I,J,K)*(1.0D0 - FF(I,J,K)))
                  FF(I,J,K) = FF(I,J,K) + DF/GGV(I,J,K)
                  DELH(IG,JG) = MAX(DELH(IG,JG) - DF*ZZ(2,K),0.0D0)
 310            CONTINUE
                DELH(IG,JG)=0.0D0
              ELSE IF(DELH(IG,JG).LT.0.0D0) THEN
                DO 320 K=NUMK-1,2,-1
                  DF = MIN(-DELH(IG,JG)/ZZ(2,K),
     $                     GGV(I,J,K)*FF(I,J,K)) ! >0
                  FF(I,J,K) = FF(I,J,K) - DF/GGV(I,J,K)
                  DELH(IG,JG) = MIN(DELH(IG,JG) + DF*ZZ(2,K),0.0D0)
 320            CONTINUE
                DELH(IG,JG)=0.0D0
              ENDIF
              IF(DELH(IG,JG).NE.0.0D0) THEN
                WRITE(ILPFIL,*) 'INITIAL ABS(DELH) IS BIG!'
                CALL VF_A2ERR('VF_FSEABT','DATA ERROR.')
              ENDIF

            ELSE
              DELH(IG,JG) = 0.0D0
            ENDIF

 400      CONTINUE
 410    CONTINUE
cadd20130529(start)
        TIME_NOW=-1.0D0
cadd20130529(end)

      ELSE

CD    -- 表面セルに水位変化分を設定する

        DO 210 J=MYJS,MYJE
          DO 200 I=MYIS,MYIE
            IG = I + MYGIS - 1
            JG = J + MYGJS - 1 

            IF(DELH(IG,JG).GT.0.0D0) THEN
              ISURF = 0
              DO 100 K=2,NUMK-2
                IF(NF(I,J,K).GE.1 .AND. NF(I,J,K).LE.6) THEN
                  DF = MIN(DELH(IG,JG)/ZZ(2,K),
     $                     GGV(I,J,K)*(1.0D0 - FF(I,J,K)))
                  FFOLD = FF(I,J,K)
                  FF(I,J,K) = FF(I,J,K) + DF/GGV(I,J,K)
                  DELH(IG,JG) = MAX(DELH(IG,JG) - DF*ZZ(2,K),0.0D0)
                  ISURF = 1
                ELSE IF(NF(I,J,K-1).GE.1 .AND. NF(I,J,K-1).LE.6
     &                                   .AND. NF(I,J,K).EQ.8) THEN
                  DF = MIN(DELH(IG,JG)/ZZ(2,K),
     $                     1.0D0-GGV(I,J,K)+FFOLD*GGV(I,J,K))
                  FF(I,J,K) = FF(I,J,K) + DF/GGV(I,J,K)
                  DELH(IG,JG) = MAX(DELH(IG,JG) - DF*ZZ(2,K),0.0D0)
                  EXIT
                ELSE IF(ISURF.EQ.1) THEN
                  EXIT
                ENDIF
 100          CONTINUE
              IF(ISURF.EQ.0) THEN      ! 当該(I,J)には表面がない
                DELH(IG,JG) = 0.0D0
              ENDIF

            ELSE IF(DELH(IG,JG).LT.0.0D0) THEN
              ISURF = 0
              DO 110 K=NUMK-1,3,-1
                IF(NF(I,J,K).GE.1 .AND. NF(I,J,K).LE.6) THEN
                  DF = MIN(-DELH(IG,JG)/ZZ(2,K),
     $                     GGV(I,J,K)*FF(I,J,K)) ! >0
                  FFOLD = FF(I,J,K)
                  FF(I,J,K) = FF(I,J,K) - DF/GGV(I,J,K)
                  DELH(IG,JG) = MIN(DELH(IG,JG) + DF*ZZ(2,K),0.0D0)
                  ISURF = 1
                ELSE IF(NF(I,J,K+1).GE.1 .AND. NF(I,J,K+1).LE.6
     &                                   .AND. NF(I,J,K).EQ.0) THEN
                  DF = MIN(-DELH(IG,JG)/ZZ(2,K),
     $                     GGV(I,J,K)*(1.0D0-FFOLD)) ! >0
                  FF(I,J,K) = FF(I,J,K) - DF/GGV(I,J,K)
                  DELH(IG,JG) = MIN(DELH(IG,JG) + DF*ZZ(2,K),0.0D0)
                  EXIT
                ELSE IF(ISURF.EQ.1) THEN
C                 jimen yori sitaheno chinkou ha sasenai
                  IF(NF(I,J,K).LT.0) DELH(IG,JG) = 0.0D0
                  EXIT
                ENDIF
 110          CONTINUE
              IF(ISURF.EQ.0) THEN     ! 当該(I,J)には表面がない
                DELH(IG,JG) = 0.0D0
              ENDIF

            ENDIF
 200      CONTINUE
 210    CONTINUE
c for debug(s)
c        DHMIN=minval(delh(2:NUMI0-1,2:NUMJ0-1))
c        DHMAX=maxval(delh(2:NUMI0-1,2:NUMJ0-1))
c        if(mgrank.eq.0.and.(dhmin.ne.0.d0.or.dhmax.ne.0.d0))
c     $     write(ILPFIL,*) 'FSEABT: DELH REMAIN MIN,MAX =',
c     $     DHMIN,DHMAX
c for debug(e)

      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
