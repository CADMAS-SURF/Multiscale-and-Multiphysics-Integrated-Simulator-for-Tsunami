      SUBROUTINE VF_PMGGGT(GGV,GGX,GGY,GGZ,XPF,YPF,ZPF,NF,IP)
C----------------------------------------------------------2011.04 end

CD=== 概要 ===========================================================

CDT   VF_PMGGGT:マルチグリッド環境の親からのポーラス値の子の受信

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    GGV(@FOR-3D@)    : I/O : R*8 : 空隙率
CD    GGX(@FOR-3D@)    : I/O : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : I/O : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : I/O : R*8 : z方向面積透過率
CD    XPF(NUMI)        : I   : R*8 : 親の格子におけるx方向の補間係数
CD    YPF(NUMJ)        : I   : R*8 : 親の格子におけるy方向の補間係数
CD    ZPF(NUMK)        : I   : R*8 : 親の格子におけるz方向の補間係数
CD    NF(@FOR-3D@)     : I/O : I*4 : セルの状態を示すインデックス
      DIMENSION GGV (NUMI,NUMJ,NUMK),GGX (NUMI,NUMJ,NUMK)
      DIMENSION GGY (NUMI,NUMJ,NUMK),GGZ (NUMI,NUMJ,NUMK)
      DIMENSION XPF(NUMI),YPF(NUMJ),ZPF(NUMK)
      DIMENSION NF(NUMI,NUMJ,NUMK)

CD    -- 局所変数 --
      INTEGER, DIMENSION(:), ALLOCATABLE :: IPF, JPF, KPF
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: GWRK
cmod20160805(s)
      logical lsou,lwes,leas,lnor
cmod20160805(e)

C==== 実行 ===========================================================

      NX = MGPINF(1)
      NY = MGPINF(2)
      NZ = MGPINF(3)

      ALLOCATE(IPF(0:NX),STAT=IERR)
      ALLOCATE(JPF(0:NY),STAT=IERR)
      ALLOCATE(KPF(0:NZ),STAT=IERR)
      IF (IERR.NE.0) CALL VF_A2ERR('VF_PMGGGT','CAN NOT ALLOC.')
      CALL VF_PMGST2(XPF,YPF,ZPF,IPF,JPF,KPF)

CD    -- GGV & NF --
      NGWRK = (NX*NY - MAX((NX-2)*(NY-2),0))*NZ *2
      ALLOCATE(GWRK(NGWRK))
      CALL VF_ZXMG_IRECVD(GWRK,NGWRK,IP,IREQ,IERR)
      CALL VF_ZXMG_WAIT(IREQ,IERR)
      IGWRK = 0
      DO 100 K=1,NZ
        DO 110 J=1,NY
          DO 120 I=1,NX
            IF(I.LE.1 .OR. I.GE.NX .OR. J.LE.1 .OR. J.GE.NY) THEN
              GGVW = GWRK(IGWRK+1)
              NFW  = GWRK(IGWRK+2)
              IGWRK = IGWRK + 2
cmod20160805(s)
              lsou=.false.
              lwes=.false.
              leas=.false.
              lnor=.false.
              if(j.eq.1 ) lsou=.true.
              if(i.eq.1 ) lwes=.true.
              if(i.eq.nx) leas=.true.
              if(j.eq.ny) lnor=.true.
              if( lwes.and.lsou ) then
                 if( mgpinf(4).eq.1.and.mgpinf(5).eq.1 ) goto 120
              elseif( leas.and.lsou ) then
                 if( mgpinf(7).eq.1.and.mgpinf(5).eq.1 ) goto 120
              elseif( leas.and.lnor ) then
                 if( mgpinf(7).eq.1.and.mgpinf(8).eq.1 ) goto 120
              elseif( lwes.and.lnor ) then
                 if( mgpinf(4).eq.1.and.mgpinf(8).eq.1 ) goto 120
              elseif( lwes ) then
                 if( mgpinf(4).eq.1 ) goto 120
              elseif( lsou ) then
                 if( mgpinf(5).eq.1 ) goto 120
              elseif( leas ) then
                 if( mgpinf(7).eq.1 ) goto 120
              elseif( lnor ) then
                 if( mgpinf(8).eq.1 ) goto 120
              endif
cmod20160805(e)
              DO 130 KC=KPF(K-1)+1,KPF(K)
                DO 140 JC=JPF(J-1)+1,JPF(J)
                  DO 150 IC=IPF(I-1)+1,IPF(I)
                    GGV(IC,JC,KC) = GGVW
                    NF (IC,JC,KC) = NFW
 150              CONTINUE
 140              CONTINUE
 130          CONTINUE
            END IF
 120      CONTINUE
 110    CONTINUE
 100  CONTINUE
      IF(NGWRK.NE.IGWRK) THEN
        CALL VF_A2ERR('VF_PMGGGT','PROGRAM ERROR(GGV).')
      END IF
      DEALLOCATE(GWRK)

CD    -- GGX --
      NGWRK = ((NX+1)*NY - MAX((NX-3)*(NY-2),0))*NZ
      ALLOCATE(GWRK(NGWRK))
      CALL VF_ZXMG_IRECVD(GWRK,NGWRK,IP,IREQ,IERR)
      CALL VF_ZXMG_WAIT(IREQ,IERR)
      IGWRK = 0
      DO 200 K=1,NZ
        DO 210 J=1,NY
          DO 220 I=1,NX+1
            IF(I.LE.2 .OR. I.GE.NX .OR. J.LE.1 .OR. J.GE.NY) THEN
              GGXW = GWRK(IGWRK+1)
              IGWRK = IGWRK + 1
cmod20160805(s)
              lsou=.false.
              lwes=.false.
              leas=.false.
              lnor=.false.
              if(j.eq.1 ) lsou=.true.
              if(i.le.2 ) lwes=.true.
              if(i.ge.nx) leas=.true.
              if(j.eq.ny) lnor=.true.
              if( lwes.and.lsou ) then
                 if( mgpinf(4).eq.1.and.mgpinf(5).eq.1 ) goto 220
              elseif( leas.and.lsou ) then
                 if( mgpinf(7).eq.1.and.mgpinf(5).eq.1 ) goto 220
              elseif( leas.and.lnor ) then
                 if( mgpinf(7).eq.1.and.mgpinf(8).eq.1 ) goto 220
              elseif( lwes.and.lnor ) then
                 if( mgpinf(4).eq.1.and.mgpinf(8).eq.1 ) goto 220
              elseif( lwes ) then
                 if( mgpinf(4).eq.1 ) goto 220
              elseif( lsou ) then
                 if( mgpinf(5).eq.1 ) goto 220
              elseif( leas ) then
                 if( mgpinf(7).eq.1 ) goto 220
              elseif( lnor ) then
                 if( mgpinf(8).eq.1 ) goto 220
              endif
cmod20160805(e)
              IC1 = IPF(I-1)+1
              DO 230 KC=KPF(K-1)+1,KPF(K)
                DO 240 JC=JPF(J-1)+1,JPF(J)
                  GGX(IC1,JC,KC) = GGXW
                  IF(I.GT.1 .AND.
     &              (I.EQ.2 .OR. I.EQ.NX+1 .OR.
     &               J.LE.1 .OR. J.GE.NY       )) THEN
                    IC0 = IPF(I-2)+1
                    DO 250 IC=IC0+1,IC1-1
                      FACT = XPF(IC)-XPF(IC0)
                      GGX(IC,JC,KC) =          FACT *GGX(IC1,JC,KC)
     &                              + (1.0D0 - FACT)*GGX(IC0,JC,KC)
 250                CONTINUE
                  END IF
 240            CONTINUE
 230          CONTINUE
            END IF
 220      CONTINUE
 210    CONTINUE
 200  CONTINUE
      IF(NGWRK.NE.IGWRK) THEN
        CALL VF_A2ERR('VF_PMGGGT','PROGRAM ERROR(GGX).')
      END IF
      DEALLOCATE(GWRK)

CD    -- GGY --
      NGWRK = (NX*(NY+1) - MAX((NX-2)*(NY-3),0))*NZ
      ALLOCATE(GWRK(NGWRK))
      CALL VF_ZXMG_IRECVD(GWRK,NGWRK,IP,IREQ,IERR)
      CALL VF_ZXMG_WAIT(IREQ,IERR)
      IGWRK = 0
      DO 300 K=1,NZ
        DO 310 J=1,NY+1
          DO 320 I=1,NX
            IF(I.LE.1 .OR. I.GE.NX .OR. J.LE.2 .OR. J.GE.NY) THEN
              GGYW = GWRK(IGWRK+1)
              IGWRK = IGWRK + 1
cmod20160805(s)
              lsou=.false.
              lwes=.false.
              leas=.false.
              lnor=.false.
              if(j.le.2 ) lsou=.true.
              if(i.eq.1 ) lwes=.true.
              if(i.eq.nx) leas=.true.
              if(j.ge.ny) lnor=.true.
              if( lwes.and.lsou ) then
                 if( mgpinf(4).eq.1.and.mgpinf(5).eq.1 ) goto 320
              elseif( leas.and.lsou ) then
                 if( mgpinf(7).eq.1.and.mgpinf(5).eq.1 ) goto 320
              elseif( leas.and.lnor ) then
                 if( mgpinf(7).eq.1.and.mgpinf(8).eq.1 ) goto 320
              elseif( lwes.and.lnor ) then
                 if( mgpinf(4).eq.1.and.mgpinf(8).eq.1 ) goto 320
              elseif( lwes ) then
                 if( mgpinf(4).eq.1 ) goto 320
              elseif( lsou ) then
                 if( mgpinf(5).eq.1 ) goto 320
              elseif( leas ) then
                 if( mgpinf(7).eq.1 ) goto 320
              elseif( lnor ) then
                 if( mgpinf(8).eq.1 ) goto 320
              endif
cmod20160805(e)
              JC1 = JPF(J-1)+1
              DO 330 KC=KPF(K-1)+1,KPF(K)
                DO 340 IC=IPF(I-1)+1,IPF(I)
                  GGY(IC,JC1,KC) = GGYW
                  IF(J.GT.1 .AND.
     &              (J.EQ.2 .OR. J.EQ.NY+1 .OR.
     &               I.LE.1 .OR. I.GE.NX       )) THEN
                    JC0 = JPF(J-2)+1
                    DO 350 JC=JC0+1,JC1-1
                      FACT = YPF(JC)-YPF(JC0)
                      GGY(IC,JC,KC) =          FACT *GGY(IC,JC1,KC)
     &                              + (1.0D0 - FACT)*GGY(IC,JC0,KC)
 350                CONTINUE
                  END IF
 340            CONTINUE
 330          CONTINUE
            END IF
 320      CONTINUE
 310    CONTINUE
 300  CONTINUE
      IF(NGWRK.NE.IGWRK) THEN
        CALL VF_A2ERR('VF_PMGGGT','PROGRAM ERROR(GGY).')
      END IF
      DEALLOCATE(GWRK)

CD    -- GGZ --
      NGWRK = (NX*NY - MAX((NX-2)*(NY-2),0))*(NZ+1)
      ALLOCATE(GWRK(NGWRK))
      CALL VF_ZXMG_IRECVD(GWRK,NGWRK,IP,IREQ,IERR)
      CALL VF_ZXMG_WAIT(IREQ,IERR)
      IGWRK = 0
      DO 400 K=1,NZ+1
        DO 410 J=1,NY
          DO 420 I=1,NX
            IF(I.LE.1 .OR. I.GE.NX .OR. J.LE.1 .OR. J.GE.NY) THEN
              GGZW = GWRK(IGWRK+1)
              IGWRK = IGWRK + 1
cmod20160805(s)
              lsou=.false.
              lwes=.false.
              leas=.false.
              lnor=.false.
              if(j.eq.1 ) lsou=.true.
              if(i.eq.1 ) lwes=.true.
              if(i.eq.nx) leas=.true.
              if(j.eq.ny) lnor=.true.
              if( lwes.and.lsou ) then
                 if( mgpinf(4).eq.1.and.mgpinf(5).eq.1 ) goto 420
              elseif( leas.and.lsou ) then
                 if( mgpinf(7).eq.1.and.mgpinf(5).eq.1 ) goto 420
              elseif( leas.and.lnor ) then
                 if( mgpinf(7).eq.1.and.mgpinf(8).eq.1 ) goto 420
              elseif( lwes.and.lnor ) then
                 if( mgpinf(4).eq.1.and.mgpinf(8).eq.1 ) goto 420
              elseif( lwes ) then
                 if( mgpinf(4).eq.1 ) goto 420
              elseif( lsou ) then
                 if( mgpinf(5).eq.1 ) goto 420
              elseif( leas ) then
                 if( mgpinf(7).eq.1 ) goto 420
              elseif( lnor ) then
                 if( mgpinf(8).eq.1 ) goto 420
              endif
cmod20160805(e)
              KC1 = KPF(K-1)+1
              DO 430 JC=JPF(J-1)+1,JPF(J)
                DO 440 IC=IPF(I-1)+1,IPF(I)
                  GGZ(IC,JC,KC1) = GGZW
                  IF(K.GT.1) THEN
                    KC0 = KPF(K-2)+1
                    DO 450 KC=KC0+1,KC1-1
                      FACT = ZPF(KC)-ZPF(KC0)
                      GGZ(IC,JC,KC) =          FACT *GGY(IC,JC,KC1)
     &                              + (1.0D0 - FACT)*GGY(IC,JC,KC0)
 450                CONTINUE
                  END IF
 440            CONTINUE
 430          CONTINUE
            END IF
 420      CONTINUE
 410    CONTINUE
 400  CONTINUE
      IF(NGWRK.NE.IGWRK) THEN
        CALL VF_A2ERR('VF_PMGGGT','PROGRAM ERROR(GGZ).')
      END IF
      DEALLOCATE(GWRK)

      DEALLOCATE(IPF,JPF,KPF)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
