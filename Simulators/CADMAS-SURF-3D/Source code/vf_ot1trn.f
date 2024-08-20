      SUBROUTINE VF_OT1TRN(DT,XX,YY,ZZ,UU,VV,WW,PP,FF,GGV,AK,AE,TT,CC,
     &                     BCU,BCV,BCW,NF,INDX,INDY,INDZ)

CD=== 概要 ===========================================================

CDT   VF_OG1TRN:解析結果を時系列ファイルに出力する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_AFILER.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_ATIMER.h'

CD    -- 引数 --
CD    DT             : IN  : R*8 : 次のステップの時間刻み幅
CD    XX(MAXG1,NUMI) : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ) : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK) : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)   : IN  : R*8 : x方向流速
CD    VV(@FOR-3D@)   : IN  : R*8 : y方向流速
CD    WW(@FOR-3D@)   : IN  : R*8 : z方向流速
CD    PP(@FOR-3D@)   : IN  : R*8 : 圧力
CD    FF(@FOR-3D@)   : IN  : R*8 : VOF関数F
CD    GGV(@FOR-3D@)  : IN  : R*8 : 空隙率
CD    AK(@FOR-3D@)   : IN  : R*8 : 乱流エネルギ
CD    AE(@FOR-3D@)   : IN  : R*8 : 乱流エネルギ散逸
CD    TT(@FOR-3D@)   : IN  : R*8 : 温度
CD    CC(@FOR-3D@,LEQC) : IN : R*8 : 濃度
CD    BCU(NUMB)      : IN  : R*8 : x方向流速の境界値
CD    BCV(NUMB)      : IN  : R*8 : y方向流速の境界値
CD    BCW(NUMB)      : IN  : R*8 : z方向流速の境界値
CD    NF(@FOR-3D@)   : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@) : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@) : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@) : IN  : I*4 : z面の状態を示すインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV   (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),PP   (NUMI,NUMJ,NUMK)
      DIMENSION FF  (NUMI,NUMJ,NUMK),GGV  (NUMI,NUMJ,NUMK)
      DIMENSION AK  (NUMI,NUMJ,NUMK),AE   (NUMI,NUMJ,NUMK)
      DIMENSION TT  (NUMI,NUMJ,NUMK),CC   (NUMI,NUMJ,NUMK,LEQC)
      DIMENSION BCU(NUMB),BCV(NUMB),BCW(NUMB)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 出力の判定 --
      IO=0
C     * ステップ間隔出力の場合
      IF     (ITRTYP.EQ.1) THEN
        IF (NNOW.GE.ITRTRN(1) .AND. NNOW.LE.ITRTRN(2)) THEN
          IF (MOD(NNOW-ITRTRN(1),ITRTRN(3)).EQ.0) IO=1
        ENDIF
C     * 時間間隔出力の場合
      ELSEIF (ITRTYP.EQ.2) THEN
Cmod20130605        IF (TNOW.GE.RTRTRN(1)-ZERO .AND. TNOW.LE.RTRTRN(2)+ZERO) THEN
        IF (TNOW.GE.RTRTRN(1)-0.5D0*DT .AND.
     $      TNOW.LE.RTRTRN(2)+0.5D0*DT) THEN
          W=(TNOW+0.5D0*DT)-RTRTRN(4)
          IF (W.GE.0.0D0) THEN
            IO=1
            RTRTRN(4)=RTRTRN(4)+DBLE(INT(W/RTRTRN(3))+1)*RTRTRN(3)
          ENDIF
        ENDIF
      ENDIF

CD    -- 非出力ならば抜ける --
      IF (IO.EQ.0) GOTO 9000

CD    -- 各種値の計算 --
      DO 100 L=1,ITRNUM
        IF     (ITRPRM(1,L).EQ.0) THEN
          IF     (ITRPRM(3,L).EQ.-1) THEN
            RTRVAL(L)=BCTYP(7,1)
          ELSEIF (ITRPRM(3,L).EQ.-2) THEN
            RTRVAL(L)=BCTYP(7,2)
          ELSEIF (ITRPRM(3,L).EQ.-3) THEN
            RTRVAL(L)=BCTYP(7,3)
          ELSEIF (ITRPRM(3,L).EQ.-4) THEN
            RTRVAL(L)=BCTYP(7,4)
          ELSEIF (ITRPRM(3,L).EQ.-5) THEN
            RTRVAL(L)=SCTYP(7)
          ELSE
            CALL VF_CWLVL(ZZ,FF,RTRVAL(L),NF,ITRPRM(3,L),ITRPRM(4,L))
          ENDIF
        ELSEIF (ITRPRM(1,L).EQ.1) THEN
          IP=ITRPRM(3,L)-(MYGIS-1)
          JP=ITRPRM(4,L)-(MYGJS-1)
          KP=ITRPRM(5,L)
          IF     (ITRPRM(2,L).EQ.1) THEN
            RTRVAL(L)=0.0D0
            IF (MYMIE.NE.1) THEN
              IF (MYIS.LE.IP .AND. IP.LE.MYIE .AND.
     &            MYJS.LE.JP .AND. JP.LE.MYJE      ) THEN
                RTRVAL(L)=UU(IP,JP,KP)
              ENDIF
            ELSE
              IF (MYIS.LE.IP .AND. IP.LE.MYIE+1 .AND.
     &            MYJS.LE.JP .AND. JP.LE.MYJE      ) THEN
                RTRVAL(L)=UU(IP,JP,KP)
              ENDIF
            ENDIF
            W=RTRVAL(L)
            CALL VF_P1SUMD(W,RTRVAL(L))
          ELSEIF (ITRPRM(2,L).EQ.2) THEN
            RTRVAL(L)=0.0D0
            IF (MYMJE.NE.1) THEN
              IF (MYIS.LE.IP .AND. IP.LE.MYIE .AND.
     &            MYJS.LE.JP .AND. JP.LE.MYJE      ) THEN
                RTRVAL(L)=VV(IP,JP,KP)
              ENDIF
            ELSE
              IF (MYIS.LE.IP .AND. IP.LE.MYIE .AND.
     &            MYJS.LE.JP .AND. JP.LE.MYJE+1    ) THEN
                RTRVAL(L)=VV(IP,JP,KP)
              ENDIF
            ENDIF
            W=RTRVAL(L)
            CALL VF_P1SUMD(W,RTRVAL(L))
          ELSEIF (ITRPRM(2,L).EQ.3) THEN
            RTRVAL(L)=0.0D0
            IF (MYIS.LE.IP .AND. IP.LE.MYIE .AND.
     &          MYJS.LE.JP .AND. JP.LE.MYJE      ) THEN
              RTRVAL(L)=WW(IP,JP,KP)
            ENDIF
            W=RTRVAL(L)
            CALL VF_P1SUMD(W,RTRVAL(L))
          ELSEIF (ITRPRM(2,L).EQ.4) THEN
            RTRVAL(L)=0.0D0
            IF (MYIS.LE.IP .AND. IP.LE.MYIE .AND.
     &          MYJS.LE.JP .AND. JP.LE.MYJE      ) THEN
              RTRVAL(L)=PP(IP,JP,KP)
            ENDIF
            W=RTRVAL(L)
            CALL VF_P1SUMD(W,RTRVAL(L))
          ELSEIF (ITRPRM(2,L).EQ.5) THEN
            RTRVAL(L)=0.0D0
            IF (MYIS.LE.IP .AND. IP.LE.MYIE .AND.
     &          MYJS.LE.JP .AND. JP.LE.MYJE      ) THEN
              RTRVAL(L)=FF(IP,JP,KP)
            ENDIF
            W=RTRVAL(L)
            CALL VF_P1SUMD(W,RTRVAL(L))
          ELSEIF (ITRPRM(2,L).EQ.6) THEN
            RTRVAL(L)=0.0D0
            IF (LEQK.NE.0) THEN
              IF (MYIS.LE.IP .AND. IP.LE.MYIE .AND.
     &            MYJS.LE.JP .AND. JP.LE.MYJE      ) THEN
                RTRVAL(L)=AK(IP,JP,KP)
              ENDIF
            ENDIF
            W=RTRVAL(L)
            CALL VF_P1SUMD(W,RTRVAL(L))
          ELSEIF (ITRPRM(2,L).EQ.7) THEN
            RTRVAL(L)=0.0D0
            IF (LEQK.NE.0) THEN
              IF (MYIS.LE.IP .AND. IP.LE.MYIE .AND.
     &            MYJS.LE.JP .AND. JP.LE.MYJE      ) THEN
                RTRVAL(L)=AE(IP,JP,KP)
              ENDIF
            ENDIF
            W=RTRVAL(L)
            CALL VF_P1SUMD(W,RTRVAL(L))
          ELSEIF (ITRPRM(2,L).EQ.8) THEN
            RTRVAL(L)=0.0D0
            IF (LEQT.NE.0) THEN
              IF (MYIS.LE.IP .AND. IP.LE.MYIE .AND.
     &            MYJS.LE.JP .AND. JP.LE.MYJE      ) THEN
                RTRVAL(L)=TT(IP,JP,KP)
              ENDIF
            ENDIF
            W=RTRVAL(L)
            CALL VF_P1SUMD(W,RTRVAL(L))
          ELSEIF (ITRPRM(2,L).LE.-1) THEN
            RTRVAL(L)=0.0D0
            IF (-ITRPRM(2,L).LE.LEQC) THEN
              IF (MYIS.LE.IP .AND. IP.LE.MYIE .AND.
     &            MYJS.LE.JP .AND. JP.LE.MYJE      ) THEN
                RTRVAL(L)=CC(IP,JP,KP,-ITRPRM(2,L))
              ENDIF
            ENDIF
            W=RTRVAL(L)
            CALL VF_P1SUMD(W,RTRVAL(L))
          ELSEIF (ITRPRM(2,L).EQ.17) THEN
            CALL VF_CVORT(XX,YY,ZZ,UU,VV,WW,BCU,BCV,BCW,
     &                    NF,INDX,INDY,INDZ,
     &                    1,1,ITRPRM(3,L),ITRPRM(4,L),ITRPRM(5,L),
     &                    RTRVAL(L))
          ELSEIF (ITRPRM(2,L).EQ.18) THEN
            CALL VF_CVORT(XX,YY,ZZ,UU,VV,WW,BCU,BCV,BCW,
     &                    NF,INDX,INDY,INDZ,
     &                    1,2,ITRPRM(3,L),ITRPRM(4,L),ITRPRM(5,L),
     &                    RTRVAL(L))
          ELSEIF (ITRPRM(2,L).EQ.19) THEN
            CALL VF_CVORT(XX,YY,ZZ,UU,VV,WW,BCU,BCV,BCW,
     &                    NF,INDX,INDY,INDZ,
     &                    1,3,ITRPRM(3,L),ITRPRM(4,L),ITRPRM(5,L),
     &                    RTRVAL(L))
          ELSE
            CALL VF_A2ERR('VF_OT1TRN','P.G ERROR.')
          ENDIF
        ELSEIF (ITRPRM(1,L).GE.2 .AND. ITRPRM(1,L).LE.5) THEN
          IF     (ITRPRM(2,L).EQ.1) THEN
            CALL VF_CAREAP(XX,YY,ZZ,GGV,FF,NF,1,UU,
     &                     ITRPRM(3,L),ITRPRM(4,L),ITRPRM(5,L),
     &                     ITRPRM(6,L),ITRPRM(7,L),ITRPRM(8,L),
     &                     VMIN,VMAX,VAV,VINT)
          ELSEIF (ITRPRM(2,L).EQ.2) THEN
            CALL VF_CAREAP(XX,YY,ZZ,GGV,FF,NF,2,VV,
     &                     ITRPRM(3,L),ITRPRM(4,L),ITRPRM(5,L),
     &                     ITRPRM(6,L),ITRPRM(7,L),ITRPRM(8,L),
     &                     VMIN,VMAX,VAV,VINT)
          ELSEIF (ITRPRM(2,L).EQ.3) THEN
            CALL VF_CAREAP(XX,YY,ZZ,GGV,FF,NF,3,WW,
     &                     ITRPRM(3,L),ITRPRM(4,L),ITRPRM(5,L),
     &                     ITRPRM(6,L),ITRPRM(7,L),ITRPRM(8,L),
     &                     VMIN,VMAX,VAV,VINT)
          ELSEIF (ITRPRM(2,L).EQ.4) THEN
            CALL VF_CAREAP(XX,YY,ZZ,GGV,FF,NF,4,PP,
     &                     ITRPRM(3,L),ITRPRM(4,L),ITRPRM(5,L),
     &                     ITRPRM(6,L),ITRPRM(7,L),ITRPRM(8,L),
     &                     VMIN,VMAX,VAV,VINT)
          ELSEIF (ITRPRM(2,L).EQ.5) THEN
            CALL VF_CAREAP(XX,YY,ZZ,GGV,FF,NF,5,FF,
     &                     ITRPRM(3,L),ITRPRM(4,L),ITRPRM(5,L),
     &                     ITRPRM(6,L),ITRPRM(7,L),ITRPRM(8,L),
     &                     VMIN,VMAX,VAV,VINT)
          ELSEIF (ITRPRM(2,L).EQ.6) THEN
            IF (LEQK.NE.0) THEN
              CALL VF_CAREAP(XX,YY,ZZ,GGV,FF,NF,6,AK,
     &                       ITRPRM(3,L),ITRPRM(4,L),ITRPRM(5,L),
     &                       ITRPRM(6,L),ITRPRM(7,L),ITRPRM(8,L),
     &                       VMIN,VMAX,VAV,VINT)
            ELSE
              VMIN=0.0D0
              VMAX=0.0D0
              VAV =0.0D0
              VINT=0.0D0
            ENDIF
          ELSEIF (ITRPRM(2,L).EQ.7) THEN
            IF (LEQK.NE.0) THEN
              CALL VF_CAREAP(XX,YY,ZZ,GGV,FF,NF,6,AE,
     &                       ITRPRM(3,L),ITRPRM(4,L),ITRPRM(5,L),
     &                       ITRPRM(6,L),ITRPRM(7,L),ITRPRM(8,L),
     &                       VMIN,VMAX,VAV,VINT)
            ELSE
              VMIN=0.0D0
              VMAX=0.0D0
              VAV =0.0D0
              VINT=0.0D0
            ENDIF
          ELSEIF (ITRPRM(2,L).EQ.8) THEN
            IF (LEQT.NE.0) THEN
              CALL VF_CAREAP(XX,YY,ZZ,GGV,FF,NF,6,TT,
     &                       ITRPRM(3,L),ITRPRM(4,L),ITRPRM(5,L),
     &                       ITRPRM(6,L),ITRPRM(7,L),ITRPRM(8,L),
     &                       VMIN,VMAX,VAV,VINT)
            ELSE
              VMIN=0.0D0
              VMAX=0.0D0
              VAV =0.0D0
              VINT=0.0D0
            ENDIF
          ELSEIF (ITRPRM(2,L).LE.-1) THEN
            IF (-ITRPRM(2,L).LE.LEQC) THEN
              CALL VF_CAREAP(XX,YY,ZZ,GGV,FF,NF,6,
     &                       CC(1,1,1,-ITRPRM(2,L)),
     &                       ITRPRM(3,L),ITRPRM(4,L),ITRPRM(5,L),
     &                       ITRPRM(6,L),ITRPRM(7,L),ITRPRM(8,L),
     &                       VMIN,VMAX,VAV,VINT)
            ELSE
              VMIN=0.0D0
              VMAX=0.0D0
              VAV =0.0D0
              VINT=0.0D0
            ENDIF
          ELSE
            CALL VF_A2ERR('VF_OT1TRN','P.G ERROR.')
          ENDIF
          IF     (ITRPRM(1,L).EQ.2) THEN
            RTRVAL(L)=VMIN
          ELSEIF (ITRPRM(1,L).EQ.3) THEN
            RTRVAL(L)=VMAX
          ELSEIF (ITRPRM(1,L).EQ.4) THEN
            RTRVAL(L)=VAV
          ELSEIF (ITRPRM(1,L).EQ.5) THEN
            RTRVAL(L)=VINT
          ENDIF
        ELSEIF (ITRPRM(1,L).EQ.11) THEN
          IF     (ITRPRM(2,L).EQ.11) THEN
            CALL VF_CFORCE(XX,YY,ZZ,PP,FF,NF,1,RTRVAL(L),
     &                     ITRPRM(3,L),ITRPRM(4,L),ITRPRM(5,L),
     &                     ITRPRM(6,L),ITRPRM(7,L),ITRPRM(8,L) )
          ELSEIF (ITRPRM(2,L).EQ.12) THEN
            CALL VF_CFORCE(XX,YY,ZZ,PP,FF,NF,2,RTRVAL(L),
     &                     ITRPRM(3,L),ITRPRM(4,L),ITRPRM(5,L),
     &                     ITRPRM(6,L),ITRPRM(7,L),ITRPRM(8,L) )
          ELSEIF (ITRPRM(2,L).EQ.13) THEN
            CALL VF_CFORCE(XX,YY,ZZ,PP,FF,NF,3,RTRVAL(L),
     &                     ITRPRM(3,L),ITRPRM(4,L),ITRPRM(5,L),
     &                     ITRPRM(6,L),ITRPRM(7,L),ITRPRM(8,L) )
          ELSEIF (ITRPRM(2,L).EQ.14) THEN
            CALL VF_CFORCE(XX,YY,ZZ,PP,FF,NF,4,RTRVAL(L),
     &                     ITRPRM(3,L),ITRPRM(4,L),ITRPRM(5,L),
     &                     ITRPRM(6,L),ITRPRM(7,L),ITRPRM(8,L) )
          ELSEIF (ITRPRM(2,L).EQ.15) THEN
            CALL VF_CFORCE(XX,YY,ZZ,PP,FF,NF,5,RTRVAL(L),
     &                     ITRPRM(3,L),ITRPRM(4,L),ITRPRM(5,L),
     &                     ITRPRM(6,L),ITRPRM(7,L),ITRPRM(8,L) )
          ELSEIF (ITRPRM(2,L).EQ.16) THEN
            CALL VF_CFORCE(XX,YY,ZZ,PP,FF,NF,6,RTRVAL(L),
     &                     ITRPRM(3,L),ITRPRM(4,L),ITRPRM(5,L),
     &                     ITRPRM(6,L),ITRPRM(7,L),ITRPRM(8,L) )
          ELSE
            CALL VF_A2ERR('VF_OT1TRN','P.G ERROR.')
          ENDIF
        ELSE
          CALL VF_A2ERR('VF_OT1TRN','P.G ERROR.')
        ENDIF
 100  CONTINUE

CD    -- ファイルへ出力 --
      IF (MYRANK.EQ.0) WRITE(ITRFIL,9510) TNOW,(RTRVAL(L),L=1,ITRNUM)

C     -- 実行文の終了 --
 9000 CONTINUE
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT( ' ',1PE15.8,10000(' ',1PE15.8:))

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
