      SUBROUTINE VF_PMGP2C(XX,YY,ZZ,UU,VV,WW,FF,
C----------------------------------------------------------2011.04 start
     &                     GGV,GGX,GGY,XPF,YPF,ZPF,IPF,JPF,KPF,
C----------------------------------------------------------2011.04 end
     &                     BCU,BCV,BCW,BCF,DBUF,NF,INDX,INDY,INDB)

CD=== 概要 ===========================================================

CDT   VF_PMGP2C:マルチグリッド環境の親の情報を子へ転送する

C==== 宣言 ===========================================================

C     -- 大域型 --
      use mod_apara,only: set_p2c,ncountssum,nsend,rbuf
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : I/O : R*8 : x方向流速
CD    VV(@FOR-3D@)     : I/O : R*8 : y方向流速
CD    WW(@FOR-3D@)     : IN  : R*8 : z方向流速
CD    FF(@FOR-3D@)     : IN  : R*8 : VOF関数F
C----------------------------------------------------------2011.04 start
CD    XPF(NUMI)        : IN  : R*8 : x方向の親格子に対する補間係数
CD    YPF(NUMJ)        : IN  : R*8 : y方向の親格子に対する補間係数
CD    ZPF(NUMK)        : IN  : R*8 : z方向の親格子に対する補間係数
CD    IPF(MGPINF(1))   : IN  : I*4 : x方向の親格子1に対する格子の数
CD    JPF(MGPINF(2))   : IN  : I*4 : y方向の親格子1に対する格子の数
CD    KPF(MGPINF(3))   : IN  : I*4 : z方向の親格子1に対する格子の数
C----------------------------------------------------------2011.04 end
CD    BCU(NUMB)        : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB)        : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB)        : I/O : R*8 : z方向流速の境界値
CD    BCF(NUMB)        : I/O : R*8 : VOF関数Fの境界値
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),FF  (NUMI,NUMJ,NUMK)
C----------------------------------------------------------2011.04 start
      DIMENSION GGV(NUMI,NUMJ,NUMK)
      DIMENSION GGX(NUMI,NUMJ,NUMK),GGY(NUMI,NUMJ,NUMK)
      DIMENSION XPF(NUMI),YPF(NUMJ),ZPF(NUMK)
      DIMENSION IPF(0:MGPINF(1)),JPF(0:MGPINF(2)),KPF(0:MGPINF(3))
C----------------------------------------------------------2011.04 end
      DIMENSION BCU (NUMB),BCV(NUMB),BCW(NUMB),BCF(NUMB)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)

C==== 実行 ===========================================================

CD    C@ CAKIY ID,JD,KD
C----------------------------------------------------------2011.04 start
C     ID=3
C     JD=3
C     KD=1
      MGNV = 9
C----------------------------------------------------------2011.04 end

CD    -- 親の値を子に転送する --
      DO 300 IC=1,MGCNUM
C----------------------------------------------------------2011.04 start
C       DO 290 LL=1,4
C----------------------------------------------------------2011.04 end
          IS=MGCPOS(1,IC)
          JS=MGCPOS(2,IC)
          KS=MGCPOS(3,IC)
          IE=MGCPOS(4,IC)
          JE=MGCPOS(5,IC)
          KE=MGCPOS(6,IC)
cadd20160729(s)
c ..... ローカルインデックスに変換
          IS=IS-MYGIS+1
          JS=JS-MYGJS+1
          IE=IE-MYGIS+1
          JE=JE-MYGJS+1
          ISG=MYIS
          IEG=MYIE
          JSG=MYJS
          JEG=MYJE
c          NN=0
          if(ic.eq.1) nn=0
cadd20160729(e)
          IF (MGCINF(4,IC).EQ.0) THEN
            DO 110 K=KS,KE
              DO 100 J=JS,JE
cmod20160729(s)
                I=IS
                IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $              JSG.LE.J.AND.J.LE.JEG ) THEN
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               IF (LL.EQ.1) THEN
C                 DBUF(NN)=0.5D0*(FF(IS-1,J,K)+FF(IS,J,K))
C               ENDIF
C               IF (LL.EQ.2) THEN
C                 DBUF(NN)=UU(IS,J,K)
C               ENDIF
C               IF (LL.EQ.3) THEN
C                 DBUF(NN)=0.25D0*(VV(IS-1,J  ,K)+VV(IS,J  ,K)+
C    &                             VV(IS-1,J+1,K)+VV(IS,J+1,K))
C               ENDIF
C               IF (LL.EQ.4) THEN
C                 DBUF(NN)=0.25D0*(WW(IS-1,J,K  )+WW(IS,J,K  )+
C    &                             WW(IS-1,J,K+1)+WW(IS,J,K+1))
C               ENDIF
                CALL VF_PMGP2C_PF(DBUF(NN+1),
     &                            XX,YY,ZZ,GGV,GGX,GGY,UU,VV,WW,FF,
     &                            BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                            IS,J,K,-1,MGNV)
cdebug 20170512(s)
c                I=IS
c                X1=XX(1,I)
c                Y1=0.5d0*(YY(1,J)+YY(1,J+1))
c                Z1=0.5d0*(ZZ(1,K)+ZZ(1,K+1))
c                write(100,'(7f10.2)') X1,Y1,Z1,DBUF(NN+1:NN+4)
cdebug 20170512(e)
                NN = NN + MGNV
C----------------------------------------------------------2011.04 end
              ENDIF
cmod20160729(e)
 100          CONTINUE
 110        CONTINUE
          ENDIF
          IF (MGCINF(7,IC).EQ.0) THEN
            DO 160 K=KS,KE
              DO 150 J=JS,JE
cmod20160729(s)
                I=IE+1
                IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $              JSG.LE.J.AND.J.LE.JEG ) THEN
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               IF (LL.EQ.1) THEN
C                 DBUF(NN)=0.5D0*(FF(IE,J,K)+FF(IE+1,J,K))
C               ENDIF
C               IF (LL.EQ.2) THEN
C                 DBUF(NN)=UU(IE+1,J,K)
C               ENDIF
C               IF (LL.EQ.3) THEN
C                 DBUF(NN)=0.25D0*(VV(IE,J  ,K)+VV(IE+1,J  ,K)+
C    &                             VV(IE,J+1,K)+VV(IE+1,J+1,K))
C               ENDIF
C               IF (LL.EQ.4) THEN
C                 DBUF(NN)=0.25D0*(WW(IE,J,K  )+WW(IE+1,J,K  )+
C    &                             WW(IE,J,K+1)+WW(IE+1,J,K+1))
C               ENDIF
                CALL VF_PMGP2C_PF(DBUF(NN+1),
     &                            XX,YY,ZZ,GGV,GGX,GGY,UU,VV,WW,FF,
     &                            BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                            IE+1,J,K,+1,MGNV)
cdebug 20170512(s)
c                I=IE+1
c                X1=XX(1,I)
c                Y1=0.5d0*(YY(1,J)+YY(1,J+1))
c                Z1=0.5d0*(ZZ(1,K)+ZZ(1,K+1))
c                write(100,'(7f10.2)') X1,Y1,Z1,DBUF(NN+1:NN+4)
cdebug 20170512(e)
                NN = NN + MGNV
C----------------------------------------------------------2011.04 end
              ENDIF
cmod20160729(e)
 150          CONTINUE
 160        CONTINUE
          ENDIF
          IF (MGCINF(5,IC).EQ.0) THEN
            DO 210 K=KS,KE
              DO 200 I=IS,IE
cmod20160729(s)
                J=JS
                IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $              JSG.LE.J.AND.J.LE.JEG ) THEN
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               IF (LL.EQ.1) THEN
C                 DBUF(NN)=0.5D0*(FF(I,JS-1,K)+FF(I,JS,K))
C               ENDIF
C               IF (LL.EQ.2) THEN
C                 DBUF(NN)=0.25D0*(UU(I  ,JS-1,K)+UU(I  ,JS,K)+
C    &                             UU(I+1,JS-1,K)+UU(I+1,JS,K))
C                ENDIF
C               IF (LL.EQ.3) THEN
C                 DBUF(NN)=VV(I,JS,K)
C               ENDIF
C               IF (LL.EQ.4) THEN
C                 DBUF(NN)=0.25D0*(WW(I,JS-1,K  )+WW(I,JS,K  )+
C    &                             WW(I,JS-1,K+1)+WW(I,JS,K+1))
C               ENDIF
                CALL VF_PMGP2C_PF(DBUF(NN+1),
     &                            XX,YY,ZZ,GGV,GGX,GGY,UU,VV,WW,FF,
     &                            BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                            I,JS,K,-2,MGNV)
cdebug 20170512(s)
c                J=JS
c                Y1=YY(1,J)
c                X1=0.5d0*(XX(1,I)+XX(1,I+1))
c                Z1=0.5d0*(ZZ(1,K)+ZZ(1,K+1))
c                write(100,'(7f10.2)') X1,Y1,Z1,DBUF(NN+1:NN+4)
cdebug 20170512(e)
                NN = NN + MGNV
C----------------------------------------------------------2011.04 end
              ENDIF
cmod20160729(e)
 200          CONTINUE
 210        CONTINUE
          ENDIF
          IF (MGCINF(8,IC).EQ.0) THEN
            DO 260 K=KS,KE
              DO 250 I=IS,IE
cmod20160729(s)
                J=JE+1
                IF( ISG.LE.I.AND.I.LE.IEG .AND.
     $              JSG.LE.J.AND.J.LE.JEG ) THEN
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               IF (LL.EQ.1) THEN
C                 DBUF(NN)=0.5D0*(FF(I,JE,K)+FF(I,JE+1,K))
C               ENDIF
C               IF (LL.EQ.2) THEN
C                 DBUF(NN)=0.25D0*(UU(I  ,JE,K)+UU(I  ,JE+1,K)+
C    &                             UU(I+1,JE,K)+UU(I+1,JE+1,K))
C               ENDIF
C               IF (LL.EQ.3) THEN
C                 DBUF(NN)=VV(I,JE+1,K)
C               ENDIF
C               IF (LL.EQ.4) THEN
C                 DBUF(NN)=0.25D0*(WW(I,JE,K  )+WW(I,JE+1,K  )+
C    &                             WW(I,JE,K+1)+WW(I,JE+1,K+1))
C               ENDIF
                CALL VF_PMGP2C_PF(DBUF(NN+1),
     &                            XX,YY,ZZ,GGV,GGX,GGY,UU,VV,WW,FF,
     &                            BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                            I,JE+1,K,+2,MGNV)
cdebug 20170512(s)
c                J=JE+1
c                Y1=YY(1,J)
c                X1=0.5d0*(XX(1,I)+XX(1,I+1))
c                Z1=0.5d0*(ZZ(1,K)+ZZ(1,K+1))
c                write(100,'(7f10.2)') X1,Y1,Z1,DBUF(NN+1:NN+4)
cdebug 20170512(e)
                NN = NN + MGNV
C----------------------------------------------------------2011.04 end
              ENDIF
cmod20160729(e)
 250          CONTINUE
 260        CONTINUE
          ENDIF
cmod20160729(e)
c          CALL VF_ZXMG_ISENDD(DBUF,NN,MGCRNK(IC),IREQ,IERR)
c          CALL VF_ZXMG_WAIT(IREQ,IERR)
cmod20160729(e)
C----------------------------------------------------------2011.04 start
C290    CONTINUE
C----------------------------------------------------------2011.04 end
 300  CONTINUE
cadd20160729(s)
      if( mgcnum.gt.0 ) then
         CALL SET_P2C(rbuf,DBUF,MYRANK,NPROCS)
C
         if( myrank.eq.0 ) then
            NN=1
            DO 310 IC=1,MGCNUM
c               write(100+mgrank,*) 'p2c:',nn,ic,nsend(ic),MGCRNK(IC)
c               write(1500+mgrank,*) rbuf(nn:nn+nsend(ic)-1)
               CALL VF_ZXMG_ISENDD(RBUF(NN),NSEND(IC),MGCRNK(IC),
     $                             IREQ,IERR)
               CALL VF_ZXMG_WAIT(IREQ,IERR)
               NN=NN+NSEND(IC)
  310       CONTINUE
         endif
      endif
cadd20160729(e)

CD    -- 親の値を子が受信する --
      IF (MGPRNK.GE.0) THEN
C----------------------------------------------------------2011.04 start
C       DO 590 LL=1,4
C----------------------------------------------------------2011.04 end
          IS=2
          JS=2
          IE=NUMI-1
          JE=NUMJ-1
          NN=0
          IF (MGPINF(4).EQ.0) NN=NN+MGPINF(2)*MGPINF(3)
          IF (MGPINF(7).EQ.0) NN=NN+MGPINF(2)*MGPINF(3)
          IF (MGPINF(5).EQ.0) NN=NN+MGPINF(1)*MGPINF(3)
          IF (MGPINF(8).EQ.0) NN=NN+MGPINF(1)*MGPINF(3)
C----------------------------------------------------------2011.04 start
          NN = MGNV*NN
C----------------------------------------------------------2011.04 end
c          write(100+mgrank,*) 'p2c:',nn,MGPRNK
          CALL VF_ZXMG_IRECVD(DBUF,NN,MGPRNK,IREQ,IERR)
          CALL VF_ZXMG_WAIT(IREQ,IERR)
          NN=0
          IF (MGPINF(4).EQ.0) THEN
            DO 430 KK=1,MGPINF(3)
              DO 420 JJ=1,MGPINF(2)
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=DBUF(NN)
C               K0=1+KD*(KK-1)
C               J0=1+JD*(JJ-1)
C               DO 410 KL=1,KD
C                 K=K0+KL
C                 DO 400 JL=1,JD
C                   J=J0+JL
C                   IF (INDX(IS,J,K).GE.1) THEN
C                     L=INDX(IS,J,K)
C                     IF (LL.EQ.1) BCF(L)=VAL
C                     IF (LL.EQ.2) THEN 
C                       BCU(L)=VAL
C                       UU(IS,J,K)=VAL
C                     ENDIF
C                     IF (LL.EQ.3) BCV(L)=VAL
C                     IF (LL.EQ.4) BCW(L)=VAL
C                   ENDIF
C400              CONTINUE
C410            CONTINUE
                CALL VF_PMGP2C_CF(DBUF(NN+1),
     &                            XX,YY,ZZ,UU,VV,WW,FF,
     &                            XPF,YPF,ZPF,IPF,JPF,KPF,
     &                            BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                            IS,JJ,KK,-1,MGNV)
cdebug 20170512(s)
c                I=IS
c                X1=XX(1,I)
c                Y1=0.5d0*(YY(1,JPF(JJ-1)+1)+YY(1,JPF(JJ)+1))
c                Z1=0.5d0*(ZZ(1,KPF(KK-1)+1)+ZZ(1,KPF(KK)+1))
c                write(101,'(7f10.2)') X1,Y1,Z1,DBUF(NN+1:NN+4)
cdebug 20170512(e)
                NN = NN + MGNV
C----------------------------------------------------------2011.04 end
 420          CONTINUE
 430        CONTINUE
          ENDIF
          IF (MGPINF(7).EQ.0) THEN
            DO 480 KK=1,MGPINF(3)
              DO 470 JJ=1,MGPINF(2)
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=DBUF(NN)
C               K0=1+KD*(KK-1)
C               J0=1+JD*(JJ-1)
C               DO 460 KL=1,KD
C                 K=K0+KL
C                 DO 450 JL=1,JD
C                   J=J0+JL
C                   IF (INDX(IE+1,J,K).GE.1) THEN
C                     L=INDX(IE+1,J,K)
C                     IF (LL.EQ.1) BCF(L)=VAL
C                     IF (LL.EQ.2) THEN
C                       BCU(L)=VAL
C                       UU(IE+1,J,K)=VAL
C                     ENDIF
C                     IF (LL.EQ.3) BCV(L)=VAL
C                     IF (LL.EQ.4) BCW(L)=VAL
C                   ENDIF
C450              CONTINUE
C460            CONTINUE
                CALL VF_PMGP2C_CF(DBUF(NN+1),
     &                            XX,YY,ZZ,UU,VV,WW,FF,
     &                            XPF,YPF,ZPF,IPF,JPF,KPF,
     &                            BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                            IE+1,JJ,KK,+1,MGNV)
cdebug 20170512(s)
c                I=IE+1
c                X1=XX(1,I)
c                Y1=0.5d0*(YY(1,JPF(JJ-1)+1)+YY(1,JPF(JJ)+1))
c                Z1=0.5d0*(ZZ(1,KPF(KK-1)+1)+ZZ(1,KPF(KK)+1))
c                write(101,'(7f10.2)') X1,Y1,Z1,DBUF(NN+1:NN+4)
cdebug 20170512(e)
                NN = NN + MGNV
C----------------------------------------------------------2011.04 end
 470          CONTINUE
 480        CONTINUE
          ENDIF
          IF (MGPINF(5).EQ.0) THEN
            DO 530 KK=1,MGPINF(3)
              DO 520 II=1,MGPINF(1)
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=DBUF(NN)
C               K0=1+KD*(KK-1)
C               I0=1+ID*(II-1)
C               DO 510 KL=1,KD
C                 K=K0+KL
C                 DO 500 IL=1,ID
C                   I=I0+IL
C                   IF (INDY(I,JS,K).GE.1) THEN
C                     L=INDY(I,JS,K)
C                     IF (LL.EQ.1) BCF(L)=VAL
C                     IF (LL.EQ.2) BCU(L)=VAL
C                     IF (LL.EQ.3) THEN
C                       BCV(L)=VAL
C                       VV(I,JS,K)=VAL
C                     ENDIF
C                     IF (LL.EQ.4) BCW(L)=VAL
C                   ENDIF
C500              CONTINUE
C510            CONTINUE
                CALL VF_PMGP2C_CF(DBUF(NN+1),
     &                            XX,YY,ZZ,UU,VV,WW,FF,
     &                            XPF,YPF,ZPF,IPF,JPF,KPF,
     &                            BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                            II,JS,KK,-2,MGNV)
cdebug 20170512(s)
c                J=JS
c                Y1=YY(1,J)
c                X1=0.5d0*(XX(1,IPF(II-1)+1)+XX(1,IPF(II)+1))
c                Z1=0.5d0*(ZZ(1,KPF(KK-1)+1)+ZZ(1,KPF(KK)+1))
c                write(101,'(7f10.2)') X1,Y1,Z1,DBUF(NN+1:NN+4)
cdebug 20170512(e)
                NN = NN + MGNV
C----------------------------------------------------------2011.04 end
 520          CONTINUE
 530        CONTINUE
          ENDIF
          IF (MGPINF(8).EQ.0) THEN
            DO 580 KK=1,MGPINF(3)
              DO 570 II=1,MGPINF(1)
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=DBUF(NN)
C               K0=1+KD*(KK-1)
C               I0=1+ID*(II-1)
C               DO 560 KL=1,KD
C                 K=K0+KL
C                 DO 550 IL=1,ID
C                   I=I0+IL
C                   IF (INDY(I,JE+1,K).GE.1) THEN
C                     L=INDY(I,JE+1,K)
C                     IF (LL.EQ.1) BCF(L)=VAL
C                     IF (LL.EQ.2) BCU(L)=VAL
C                     IF (LL.EQ.3) THEN
C                       BCV(L)=VAL
C                       VV(I,JE+1,K)=VAL
C                     ENDIF
C                     IF (LL.EQ.4) BCW(L)=VAL
C                   ENDIF
C550              CONTINUE
C560            CONTINUE
                CALL VF_PMGP2C_CF(DBUF(NN+1),
     &                            XX,YY,ZZ,UU,VV,WW,FF,
     &                            XPF,YPF,ZPF,IPF,JPF,KPF,
     &                            BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                            II,JE+1,KK,+2,MGNV)
cdebug 20170512(s)
c                J=JE+1
c                Y1=YY(1,J)
c                X1=0.5d0*(XX(1,IPF(II-1)+1)+XX(1,IPF(II)+1))
c                Z1=0.5d0*(ZZ(1,KPF(KK-1)+1)+ZZ(1,KPF(KK)+1))
c                write(101,'(7f10.2)') X1,Y1,Z1,DBUF(NN+1:NN+4)
cdebug 20170512(e)
                NN = NN + MGNV
C----------------------------------------------------------2011.04 end
 570          CONTINUE
 580        CONTINUE
c            write(1000+mgrank,*) bcu
c            write(1100+mgrank,*) bcv
c            write(1200+mgrank,*) bcw
c            write(1300+mgrank,*) bcf
          ENDIF
C----------------------------------------------------------2011.04 start
C590    CONTINUE
C----------------------------------------------------------2011.04 end
      ENDIF

c       call VF_ZXMG_BARRI(IERR)
c       call VF_ZXMG_ABORT(IERR)
C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END

