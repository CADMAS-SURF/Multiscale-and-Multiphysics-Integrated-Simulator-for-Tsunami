      SUBROUTINE VF_PMGINP()

CD=== 概要 ===========================================================

CDT   VF_PMGINP:マルチグリッド環境ファイルを読み込む

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_ASTOCI.h'

C==== 実行 ===========================================================

CD    -- マルチグリッド環境ファイルの読み込み --
      IENFIL=0

      IF (MGRANK.EQ.0) THEN
        WRITE(*,9510)
        OPEN(MFILEN,ERR=9010,FILE='data.env',
     &         STATUS='OLD',FORM='FORMATTED' )
        IENFIL=MFILEN
        READ(IENFIL,*,END=9020,ERR=9020) MGARAN
        IF (MGARAN.GT.MAXPRO) GOTO 9030
        IF (MGARAN.LE.0     ) GOTO 9040
        N=0
        DO 120 I=1,MGARAN
          READ(IENFIL,*,END=9020,ERR=9020) MGNAME(I),MGNPIN(I),MGPARE(I)
          MGNLEN(I)=0
          DO 100 L=MAXCHR,1,-1
            IF (MGNAME(I)(L:L).NE.' ') THEN
              MGNLEN(I)=L
              GOTO 110
            ENDIF
 100      CONTINUE
 110      CONTINUE
          IF (MGNLEN(I).LE.0     ) GOTO 9040
          IF (MGNPIN(I).LE.0     ) GOTO 9040
CCC IC-MG COUPLING    IF (MGPARE(I).LT.0    ) GOTO 9040
          IF (MGPARE(I).GT.MGARAN) GOTO 9040
          IF (MGPARE(I).EQ.I     ) GOTO 9040
          N=N+MGNPIN(I)
 120    CONTINUE
        CLOSE(IENFIL)
        IENFIL=0
        IF (N.GT.MAXPRO) GOTO 9030
        IF (N.NE.MGPROC) GOTO 9050
      ENDIF

CD    -- 環境データのパッシング --
      CALL VF_P0BCSI(MGARAN,     1,0)
      CALL VF_P0BCSI(MGNLEN,MGARAN,0)
      CALL VF_P0BCSI(MGNPIN,MGARAN,0)
      CALL VF_P0BCSI(MGPARE,MGARAN,0)
      DO 200 I=1,MGARAN
        CALL VF_P0BCSC(MGNAME(I),MGNLEN(I),0)
 200  CONTINUE

CD    -- 親子関係のループチェック --
      DO 320 I=1,MGARAN
        N=MGPARE(I)
cmod 20131023        IF (N.LT.0) NB_SC = -N
cmod 20160527        IF (N.LT.0.AND.N.GT.-10) NB_SC = -N
        IF (N.GT.0) THEN
          DO 300 L=1,MGARAN-1
            N=MGPARE(N)
            IF (N.LE.0) GOTO 310
 300      CONTINUE
 310      CONTINUE
          IF (N.GT.0) GOTO 9040
        ENDIF
 320  CONTINUE

CD    -- 領域名の重複チェック --
      DO 410 I=1,MGARAN-1
        DO 400 L=I+1,MGARAN
          IF (MGNAME(I).EQ.MGNAME(L)) GOTO 9040
 400    CONTINUE
 410  CONTINUE

CD    -- 領域毎のデータをプロセス毎に展開する --
      N=MGPROC
      NPROCS=0
      MYRANK=-1
      DO 510 I=MGARAN,1,-1
        DO 500 L=1,MGNPIN(I)
          MGAREA(N)=I
          MGNAME(N)=MGNAME(I)
          MGNLEN(N)=MGNLEN(I)
          MGNPIN(N)=MGNPIN(I)
          MGPARE(N)=MGPARE(I)
          IF (N.EQ.MGRANK+1) THEN
            NPROCS=MGNPIN(N)
            MYRANK=NPROCS-L
          ENDIF
          N=N-1
 500    CONTINUE
 510  CONTINUE
      IF (NPROCS.LE.0) GOTO 9060
      IF (MYRANK.LT.0) GOTO 9060
cadd 20160527
c      write(100+mgrank,81)
c   81 format('mgarea  mgname  mgpare  mgnpin  myrank/nprocs')
c      do n=1,mgproc
c         if(n==1) then
c            write(100+mgrank,'(i6,2x,a6,2x,i6,2x,i6,2x,i6,a1,i2)') 
c     $         mgarea(n),mgname(n)(1:mgnlen(n)),mgpare(n),mgnpin(n),
c     $         myrank,'/',nprocs
c         else
c            write(100+mgrank,'(i6,2x,a6,2x,i6,2x,i6)') 
c     $         mgarea(n),mgname(n)(1:mgnlen(n)),mgpare(n),mgnpin(n)
c         endif
c      enddo
c      write(100+mgrank,*) ''
      IF(MGPARE(MGRANK+1).LT.0.AND.MGPARE(MGRANK+1).GT.-10)
     $   NB_SC = -MGPARE(MGRANK+1)
C
CD    -- 領域毎のコミュニケータ作成 --
      MGCOMM=0
      CALL VF_ZXMG_SPLIT(MGAREA(MGRANK+1),MYRANK,MGCOMM,IERR)
      CALL VF_ZXMP_CSIZE(NP,IERR)
      CALL VF_ZXMP_CRANK(MY,IERR)
      IF (NPROCS.NE.NP) GOTO 9060
      IF (MYRANK.NE.MY) GOTO 9060

C     -- 実行文の終了 --
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      WRITE(*,9520) 'VF_PMGINP','CAN NOT OPEN (data.env).'
      WRITE(*,9530)
      CALL VF_A2CLOS()
      CALL VF_P0ENDA()
      STOP
      GOTO 9999

 9020 CONTINUE
      WRITE(*,9520) 'VF_PMGINP','I/O ERROR.'
      WRITE(*,9530)
      CALL VF_A2CLOS()
      CALL VF_P0ENDA()
      STOP
      GOTO 9999

 9030 CONTINUE
      WRITE(*,9520) 'VF_PMGINP','AREA IS FULL.'
      WRITE(*,9530)
      CALL VF_A2CLOS()
      CALL VF_P0ENDA()
      STOP
      GOTO 9999

 9040 CONTINUE
      WRITE(*,9520) 'VF_PMGINP','INVALID VALUE.'
      WRITE(*,9530)
      CALL VF_A2CLOS()
      CALL VF_P0ENDA()
      STOP
      GOTO 9999

 9050 CONTINUE
      WRITE(*,9520) 'VF_PMGINP','MGPROC <> SUM(PE).'
      WRITE(*,9530)
      CALL VF_A2CLOS()
      CALL VF_P0ENDA()
      STOP
      GOTO 9999

 9060 CONTINUE
      WRITE(*,9520) 'VF_PMGINP','P.G ERROR.'
      WRITE(*,9530)
      CALL VF_A2CLOS()
      CALL VF_P0ENDA()
      STOP
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(/' ','>> FILE-ENV : IN : ALL')
 9520 FORMAT(/' ','>>>>> ERROR. [',A,'] : ',A)
 9530 FORMAT(/' ','##### ABNORMAL END. #########################'/)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
