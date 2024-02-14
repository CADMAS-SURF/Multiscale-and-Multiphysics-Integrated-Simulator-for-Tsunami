      SUBROUTINE OUTPUT(XC_REF,YC_REF,ZC,GV,GX,GY,GZ,UU,VV,WW,PP,RHOW,
     $                  TT,CC,AK,EP,TMU,FF,HH,
     $                  SHLSD,CSEDI,CSDAVE,ZBED,ZBED0,WEXSD,QBX,QBY,
     $                  EXSDE,EXSDD,WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,
     $                  INDP,KF,KP,KG,WX,WY,PATM,VLEND,TMEND,FREND,
     $                  HDEP,HHBCN,UUBCN,VVBCN,IDST,TMDST,
     $                  UUA,VVA,WWA,PPA,AKA,EPA,TMUA,FFA,GVA,INDPA,
     $                  IFLAG)
C======================================================================
C     出力の制御を行う
C
C     IFLAG  = 0 : ファイルを開く
C            = 1 : ファイルに出力する
C            = 2 : ファイルを閉じる
C
C     VLEND(*,*, 1) : 水深                 ! not output
C     VLEND(*,*, 2) : 初期水位             ! not output
C     VLEND(*,*, 3) : 最高水位             ! EVERY
C     VLEND(*,*, 4) : 最低水位             ! EVERY
C     VLEND(*,*, 5) : 最大流速(セル中心)   ! EVERY
C     VLEND(*,*, 6) : 最大流速U(セル中心)  ! not output
C     VLEND(*,*, 7) : 最大流速V(セル中心)  ! not output
C     VLEND(*,*, 8) : 最大風速(セル中心)   ! if LTYPH.EQ.1
C     VLEND(*,*, 9) : 最大風速WX(セル中心) ! not output
C     VLEND(*,*,10) : 最大風速WY(セル中心) ! not output
C     VLEND(*,*,11) : 最低気圧(セル中心)   ! if LTYPH.EQ.1
C     VLEND(*,*,12) : 最大浮遊砂濃度       ! if LSEDI.EQ.1
C     VLEND(*,*,13) : 最大平均浮遊砂濃度   ! if LSEDI.EQ.1
C     VLEND(*,*,14) : 最大シールズ数       ! if LSEDI.EQ.1
C     VLEND(*,*,15) : 最大侵食[m]          ! if LSEDI.EQ.1
C     VLEND(*,*,16) : 最大堆積[m]          ! if LSEDI.EQ.1
C
C     TMEND(*,*,1)  : 最大水位時刻         ! EVERY
C     TMEND(*,*,2)  : 最低水位時刻         ! EVERY
C     TMEND(*,*,3)  : 到達時刻             ! EVERY
C     TMEND(*,*,4)  : 最大流速時刻         ! EVERY
C
C     TMEND(*,*,5)  : 最大風速時刻         ! if LTYPH.EQ.1
C     TMEND(*,*,6)  : 最低気圧時刻         ! if LTYPH.EQ.1
C
C     FREND(*,*,1)  : 最大浸水深           ! EVERY
C     FREND(*,*,2)  : 最大浸水深時刻       ! EVERY
C     FREND(*,*,N+2): 大浸水深DFRAGL(N)となる時刻 ! if( NFRAGL>=N )
C
C     IDST          : 建物破壊フラグ       ! if LSTOCDS.EQ.1
C     TMDST         : 建物破壊時刻         ! if LSTOCDS.EQ.1
C
C======================================================================
      use mod_gather,only: gatherh_pre,gatherv_pre,gatherv,gathervi
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'FILEI.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'MODELR.h'
      INCLUDE 'OUTPUT.h'
      INCLUDE 'TIMEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'CP_NESTBC.h'
c      INCLUDE 'CONSRV.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
      INCLUDE 'AIRI.h'
C
      REAL(8),INTENT(INOUT)::XC_REF(8,MX),YC_REF(8,MY),ZC(8,MZ)
      REAL(8),INTENT(INOUT)::GV(MX,MY,MZ),GX(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::GY(MX,MY,MZ),GZ(MX,MY,MZ)
C
      REAL(8),INTENT(INOUT)::UU(MX,MY,MZ),VV(MX,MY,MZ),WW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::PP(MX,MY,MZ),TT(MX,MY,MZ),CC(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::AK(MX,MY,MZ),EP(MX,MY,MZ),TMU(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::FF(MX,MY,MZ),RHOW(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HH(MX,MY)
      REAL(8),INTENT(INOUT)::WX(MX,MY),WY(MX,MY),PATM(MX,MY)
      REAL(8),INTENT(INOUT)::VLEND(MX,MY,16),TMEND(MX,MY,6)
      REAL(8),INTENT(INOUT)::FREND(MX,MY,2+NFRAGL)
      REAL(8),INTENT(IN)::CSEDI(MX,MY,MZ),CSDAVE(MX,MY)
      REAL(8),INTENT(IN)::ZBED(MX,MY),ZBED0(MX,MY)
      REAL(8),INTENT(IN)::SHLSD(MX,MY),WEXSD(MX,MY)
      REAL(8),INTENT(IN)::EXSDE(MX,MY),EXSDD(MX,MY)
      REAL(8),INTENT(IN)::QBX(MX,MY),QBY(MX,MY)
      REAL(8),INTENT(INOUT)::WRK1(MX,MY,MZ),WRK2(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WRK3(MX,MY,MZ),WRK4(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WRK5(MX,MY,MZ),WRK6(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::WRK7(MX,MY,MZ)
      REAL(8),INTENT(INOUT)::HDEP(MX,MY),HHBCN(MX,MY)
      REAL(8),INTENT(INOUT)::UUBCN(NXY,MZ,4),VVBCN(NXY,MZ,4)
C
C ... for last output of data.grp_*
      REAL(8),ALLOCATABLE::UU_1(:,:,:),VV_1(:,:,:),WW_1(:,:,:)
      REAL(8),ALLOCATABLE::FF_1(:,:,:)
C
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::KF(MX,MY),KP(MX,MY),KG(MX,MY)
      INTEGER,INTENT(INOUT)::IFLAG
C
      INTEGER,INTENT(INOUT)::IDST(MX,MY)
      REAL(8),INTENT(INOUT)::TMDST(MX,MY)
C
      REAL(8):: UUA(MX,MY,MZA),VVA(MX,MY,MZA),WWA(MX,MY,MZA)
      REAL(8):: PPA(MX,MY,MZA),AKA(MX,MY,MZA),EPA(MX,MY,MZA)
      REAL(8):: TMUA(MX,MY,MZA),FFA(MX,MY,MZA),GVA(MX,MY,MZA)
      INTEGER:: INDPA(MX,MY,MZA)
C
C      DATA EPST / 1.0D-2 /
C
      REAL(8)::TIME1,UC,VC,VEL,WVL,WXX,WYY,TIME_1,DEP
      INTEGER::I,IERR,IGRFIL,IXXX,J,KPIJ,M,N,ISTEP_1,K,IFLNM1
      CHARACTER(8)::STR
      LOGICAL::L0,L1,L2
      INTEGER,SAVE::IRTN=0
      CHARACTER(80):: FORM1
C
C ... 地形変化量計算用
      REAL(8)::DZBED
c--debug-write-2012----------------------------------------------------
      INTEGER,ALLOCATABLE::NF(:,:,:)
      REAL(8),ALLOCATABLE::QBXC(:,:),QBYC(:,:)
      REAL(8),ALLOCATABLE::UUC(:,:),VVC(:,:),UVC(:,:)
      CHARACTER(2)::TEXTP
c--debug-write-2012----------------------------------------------------
      ALLOCATE(NF(MX,MY,MZ),QBXC(MX,MY),QBYC(MX,MY),
     $         UUC(MX,MY),VVC(MX,MY),UVC(MX,MY),STAT=IERR)
      IF(IERR.NE.0)THEN
         CALL ERRMSG('OUTPUT',7170)
         WRITE(LP,*) 'CANNOT ALLOCATE NF,...'
         CALL ABORT1('')
      ENDIF
C
C
      L0=.FALSE.
      L1=.FALSE.
      L2=.FALSE.
      IF(MYPROC.EQ.1) L0=.TRUE.
      IF(IAUTOD.EQ.0) L1=.TRUE.
      IF(IAUTOD.EQ.1) L2=.TRUE.
C
      IFLNM1=IFLNM
      if( IAUTOD.eq.1 ) IFLNM1=IFLNM+3
C----------------------------------------------------------------------
C     (1) ファイルを開く
C----------------------------------------------------------------------
      IF( IFLAG.EQ.0 ) THEN
C
C ...... リスタートファイル
         IF( IREST0.GT.0 ) THEN
            call flnam('.rso')
            OPEN(IFLRO,FILE=trim(CFLNM),STATUS='NEW',
     $           FORM='UNFORMATTED',ERR=900)
            WRITE(LP,*) 'OPEN RESTART OUTPUT FILE'
            WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM1)
            WRITE(LP,*) 'FILE NUMBER=',IFLRO
         END IF
C
C ...... リストファイル
         IF( ILIST0.GT.0.AND.LISTT.EQ.1 ) THEN
            call flnam('.lst')
            OPEN(IFLLP,FILE=trim(CFLNM),STATUS='NEW',
     $           FORM='UNFORMATTED',ERR=900)
            WRITE(LP,*) 'OPEN LIST OUTPUT FILE'
            WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM1)
            WRITE(LP,*) 'FILE NUMBER=',IFLLP
C
            WRITE(IFLLP) MXM
            WRITE(IFLLP) (REAL(XC_REF(1,I)),I=1,MXM)
            WRITE(IFLLP) MYM
            WRITE(IFLLP) (REAL(YC_REF(1,J)),J=1,MYM)
            WRITE(IFLLP) MZM
            WRITE(IFLLP) (REAL(ZC(1,K)),K=1,MZM)
            WRITE(IFLLP) MX-1,MY-1,1
            WRITE(IFLLP) ((REAL(HDEP(I,J)),I=1,MXM),J=1,MYM)
         END IF
C
C ...... グラフィックファイル
         IF( IGRPH0.GT.0 ) THEN
            IF( IALFAFLOW.EQ.1 ) THEN
            call flnam('.grp')
            OPEN(IFLGR,FILE=trim(CFLNM),STATUS='NEW',
     $           FORM='UNFORMATTED',ERR=910)
            WRITE(LP,*) 'OPEN GRAPHIC FILE'
            WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM1)
            WRITE(LP,*) 'FILE NUMBER=',IFLGR
            ENDIF
            IF( LAIR.EQ.1 ) THEN
            call flnam('.gra')
            OPEN(39,FILE=trim(CFLNM),STATUS='NEW',
     $           FORM='UNFORMATTED',ERR=910)
            WRITE(LP,*) 'OPEN GRAPHIC FILE: AIR'
            WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM1)
            WRITE(LP,*) 'FILE NUMBER=',39
            ENDIF
CCCCCCCCCCCCCCCCCCCCCCCCCC デバッグ用ファイル出力CCCCC
            igrfil=lp+14
            call db_ini(xc_ref,yc_ref,zc,gv,lp,igrfil)
c--debug-write-2012----------------------------------------------------
            igrfil=lp+15
      IF (LSEDI.NE.0) THEN
CD    -- 図化ファイルのオープンとメッセージの出力 --
      WRITE(TEXTP,'(I2.2)') NRANK+1
      IF(IPECON(8,NRANK+1).EQ.1) THEN
        OPEN(IGRFIL,FILE='sddata.grp_ns'//TEXTP,
     &       STATUS='NEW',FORM='UNFORMATTED' )
      ELSE
        OPEN(IGRFIL,FILE='sddata.grp_ml'//TEXTP,
     &       STATUS='NEW',FORM='UNFORMATTED' )
      END IF
CD    -- バージョンを出力 --
      WRITE(IGRFIL) 1,0
CD    -- 解析領域を出力 --
      WRITE(IGRFIL) MXM-1,MYM-1,MZM-1
      WRITE(IGRFIL) xc_ref(1,  1),yc_ref(1,  1),zc(1,  1)
      WRITE(IGRFIL) xc_ref(1,MXM),yc_ref(1,MYM),zc(1,MZM)
CD    -- 出力領域を出力 --
      WRITE(IGRFIL) 1,1,1,MXM-1,MYM-1,MZM-1
      WRITE(IGRFIL) 0,0,0
CD    -- 時間毎に出力する物理量のフラグを出力 --
      WRITE(IGRFIL) 1,1,0,1,0,0,0,0,0,0
CD    -- 格子座標を出力 --
      WRITE(IGRFIL) (xc_ref(1,I),I=1,MXM)
      WRITE(IGRFIL) (yc_ref(1,J),J=1,MYM)
      WRITE(IGRFIL) (zc(1,K),K=1,MZM)
CD    -- 空隙率を出力 --
      WRITE(IGRFIL) (((GV(I,J,K),I=2,MXM),J=2,MYM),K=2,MZM)
      ENDIF
c--debug-write-2012----------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC END
         END IF
C
C ...... 時系列ファイル
         IF( IHIST0.GT.0 ) THEN
            IF(L0) THEN
               CFLNM(IFLNM-3:IFLNM) = '.hst'
               OPEN(IFLHS,FILE=CFLNM(1:IFLNM),STATUS='NEW',
     $              FORM='FORMATTED',ERR=920)
               WRITE(LP,*) 'OPEN HISTORY FILE'
               WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM)
               WRITE(LP,*) 'FILE NUMBER=',IFLHS
            ENDIF
            IF(IAUTOD.EQ.0)THEN
C ......       Write header part
               NHCELLSUM=NHCELL
               WRITE(IFLHS,600) NHCELL*MHIST+5
  600          FORMAT('# START IMPORT AT ROW',I5,/,
     $                '# COLUMN: VARIABLE     I     J     K')
               I=0
               DO M=1,MHIST
               DO N=1,NHCELL
                  I=I+1
                  WRITE(IFLHS,601) I,CHIST(M),
     $               IHCELL(1,N),IHCELL(2,N),IHCELL(3,N)
  601             FORMAT('#',1X,I6,':',1X,A8,3I6)
               ENDDO
               ENDDO
               FORM1='(''#'',/,''#        TIME'',        )'
               IF(I.GT.0) WRITE(FORM1(24:31),'(I5,A3)') I,'I13'
               WRITE(IFLHS,FORM1) (J,J=1,I)
            ELSE
               CALL GATHERH_PRE(NHCELL,NHCELLSUM,LHCELL,IHCELL,
     $                          MYPROC,NPROC,MYIS,MYJS,CHILDCOMM,IFLHS,
     $                          CHIST,MHIST)
            ENDIF
         END IF
      END IF
C
C
C----------------------------------------------------------------------
C     (2) ファイルに出力する
C----------------------------------------------------------------------
C
      TIME1 = TIME + 0.5D0*DT
C
CXXXXXXXXXXXXXXXXXXXXXXXXX 出力用ワークをクリア
      DO 109 J=1,MY
      DO 108 I=1,MX
        UU(I,J,MZ) = 0.0D0
        VV(I,J,MZ) = 0.0D0
 108  CONTINUE
 109  CONTINUE
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX END
C
C ... リスタートファイル
      IF( IREST0.GT.0 ) THEN
         IF( ( LREST.EQ.0 .AND. ISTEP.GE.IREST(IREST0) ) .OR.
     $       ( LREST.EQ.1 .AND. TIME1.GE.RREST(IREST0)  ) .OR.
     $       IFLAG.EQ.2 ) THEN
C
            CALL OUTRST(UU,VV,WW,PP,TT,CC,AK,EP,FF,HH,
     $                  VLEND,TMEND,FREND,HDEP,HHBCN,UUBCN,VVBCN,KF,KP)
            IREST0 = IREST0 + 1
C
C ......... 指定した時刻(ステップ)を全て出力した場合、次の出力時刻を到達しえない時刻にする時刻にする
            IF( IREST0.GT.NREST ) THEN
               IREST0=MAX(NREST,1)
               IF( LREST.EQ.0 ) IREST(IREST0)=99999999
               IF( LREST.EQ.1 ) RREST(IREST0)=1.0D30
            ENDIF
C ......... 指定した時刻(ステップ)を全て出力した場合、ファイルを閉じる
c            IF( IREST0.GT.NREST ) THEN
c               CLOSE(IFLRO,STATUS='KEEP')
c               WRITE(LP,*) 'CLOSE RESTART OUTPUT FILE'
c               WRITE(LP,*) 'FILE NUMBER=',IFLRO
c               IREST0 = 0
c            END IF
         END IF
      END IF
C
C ... リスト出力
      IF( ILIST0.GT.0 ) THEN
         IF( ( LLIST.EQ.0 .AND. ISTEP.GE.ILIST(ILIST0) ) .OR.
     $       ( LLIST.EQ.1 .AND. TIME1.GE.RLIST(ILIST0)  ) .OR.
     $       IFLAG.EQ.2 ) THEN
C
            CALL OUTLST(UU,VV,WW,PP,TT,CC,AK,EP,TMU,FF,RHOW,HH,HDEP,
     $                  SHLSD,CSEDI,CSDAVE,ZBED,ZBED0,
     $                  WEXSD,QBX,QBY,EXSDE,EXSDD,INDP,KF,KP,KG)
            ILIST0 = ILIST0 + 1
            IF( ILIST0.GT.NLIST ) THEN
               ILIST0 = 0
            END IF
         END IF
      END IF
C
C ... グラフィックファイル
      IF( IGRPH0.GT.0 ) THEN
         IF( ( LGRPH.EQ.0 .AND. ISTEP.GE.IGRPH(IGRPH0) ) .OR.
     $       ( LGRPH.EQ.1 .AND. TIME1.GE.RGRPH(IGRPH0)  ) .OR.
     $       IFLAG.EQ.2 ) THEN
C
CXXXXXXXXXXXXX デバッグ用グラフィックファイル 出力
            if(lsedi.eq.1)then
            do j=2,mym
            do i=2,mxm
               wrk2(i,j,1)=ZBED(I,J)-ZBED0(I,J)
            enddo
            enddo
            endif
            igrfil=lp+14
            call db_trn(uu,vv,ww,pp,ff,tt,cc,gv,wrk1,wrk2,
     $                  indp,kf,lp,igrfil)
c--debug-write-2012----------------------------------------------------
            igrfil=lp+15
      IF (LSEDI.NE.0) THEN
CD    -- 計算情報を出力 --
      WRITE(IGRFIL) ISTEP,TIME
      NF=-1
      DO J=2,MYM
      DO I=2,MXM
      DO K=2,MZM
        IF(INDP(I,J,K).EQ.1) THEN
          IF(K.GT.KF(I,J)) THEN
            NF(I,J,K)=8
          ELSE IF(K.EQ.KF(I,J)) THEN
            NF(I,J,K)=5
          ELSE
            NF(I,J,K)=0
          END IF
        END IF
      ENDDO
      ENDDO
      ENDDO
CD    -- セルの状態を示すインデックスを出力 --
      WRITE(IGRFIL) (((NF(I,J,K),I=2,MXM),J=2,MYM),K=2,MZM)
CD    -- 土砂移動関係物理量を出力 --
      WRITE(IGRFIL) (((CSEDI(I,J,K),I=2,MXM),J=2,MYM),K=2,MZM)
      WRITE(IGRFIL)  ((CSDAVE(I,J),I=2,MXM),J=2,MYM)
      WRITE(IGRFIL)  ((ZBED(I,J)-ZBED0(I,J), I=2,MXM),J=2,MYM)
      WRITE(IGRFIL)  ((SHLSD(I,J),I=2,MXM),J=2,MYM)
      WRITE(IGRFIL)  ((WEXSD(I,J),I=2,MXM),J=2,MYM)
      DO J=2,MYM
      DO I=2,MXM
        QBXC(I,J)=(QBX(I-1,J)+QBX(I,J))*0.5D0
        QBYC(I,J)=(QBY(I-1,J)+QBY(I,J))*0.5D0
      ENDDO
      ENDDO
      WRITE(IGRFIL)  ((QBXC(I,J),I=2,MXM),J=2,MYM)
      WRITE(IGRFIL)  ((QBYC(I,J),I=2,MXM),J=2,MYM)
CD    -- 念のためセル中心流速も出力 --
      DO J=2,MYM
      DO I=2,MXM
        UUC(I,J)=(UU(I-1,J,KG(I,J))+UU(I,J,KG(I,J)))*0.5D0
        VVC(I,J)=(VV(I,J-1,KG(I,J))+VV(I,J,KG(I,J)))*0.5D0
        UVC(I,J)=SQRT(UUC(I,J)**2+VVC(I,J)**2)
      ENDDO
      ENDDO
      WRITE(IGRFIL) ((UUC(I,J),I=2,MXM),J=2,MYM)
      WRITE(IGRFIL) ((VVC(I,J),I=2,MXM),J=2,MYM)
      WRITE(IGRFIL) ((UVC(I,J),I=2,MXM),J=2,MYM)
      ENDIF
c--debug-write-2012----------------------------------------------------
CXXXXXXXXXXXXX END
         IGRPH0 = IGRPH0 + 1
         IF( IALFAFLOW.EQ.1 ) THEN
CXXXXXXXXXXXXX ALFA-FLOWのポスト利用(FF(MZM)=HH)
            DO 111 J=1,MY
            DO 111 I=1,MX
            WW(I,J,MZ ) = FF(I,J,MZM)
            FF(I,J,MZM) = HH(I,J)
  111       CONTINUE
CXXXXXXXXXXXXX END
            CALL OUTGRP(UU,VV,WW,PP,TT,CC,AK,EP,TMU,FF,RHOW,
     $                  CSEDI,WRK1,WRK2,WRK3,WRK4,
     $                  WRK5,WRK6,WRK7,INDP)
CXXXXXXXXXXXXX ALFA-FLOWのポスト利用(FF(MZM)=HH,PP(MZM)=ZFC)
            DO 112 J=1,MY
            DO 112 I=1,MX
            FF(I,J,MZM) = WW(I,J,MZ)
            WW(I,J,MZ ) = 0.0D0
  112       CONTINUE
         ENDIF
CXXXXXXXXXXXXX END
         IF( LAIR.EQ.1 ) THEN
            CALL OUTGRP_AIR(UUA,VVA,WWA,PPA,AKA,EPA,TMUA,FFA,GVA,
     $                     WRK1,WRK2,WRK3,WRK4,WRK5,WRK6,WRK7,INDPA)
         ENDIF
C
C ......... 指定した時刻(ステップ)を全て出力した場合、ファイルを閉じる
            IF( IGRPH0.GT.NGRPH ) THEN
            IF( IALFAFLOW.EQ.1 ) THEN
               CLOSE(IFLGR,STATUS='KEEP')
               WRITE(LP,*) 'CLOSE GRAPHIC FILE'
               WRITE(LP,*) 'FILE NUMBER=',IFLGR
            ENDIF
            IF( LAIR.EQ.1 ) THEN
               CLOSE(39,STATUS='KEEP')
               WRITE(LP,*) 'CLOSE GRAPHIC FILE: AIR'
               WRITE(LP,*) 'FILE NUMBER=',39
            ENDIF
            IGRPH0=MIN(IGRPH0,NOUTSZ)
            IGRPH(IGRPH0)=999999999
            RGRPH(IGRPH0)=1.0D99
CCC               IGRPH0 = 0
            END IF
         END IF
      END IF
C
C ... 時系列ファイル
      IF( IHIST0.GT.0 ) THEN
         IF( ( LHIST.EQ.0 .AND. ISTEP.GE.IHIST0 ) .OR.
     $       ( LHIST.EQ.1 .AND. TIME1.GE.RHIST0 ) .OR.
     $       IFLAG.EQ.2 ) THEN
C
            CALL OUTHST(UU,VV,WW,PP,TT,CC,AK,EP,TMU,FF,RHOW,HH,HDEP,
     $                  SHLSD,CSEDI,CSDAVE,ZBED,ZBED0,
     $                  WEXSD,QBX,QBY,EXSDE,EXSDD,INDP,WX,WY,PATM)
            IF( LHIST.EQ.0 ) THEN
               IHIST0 = IHIST0 + IHIST
            ELSE
               RHIST0 = RHIST0 + RHIST
            END IF
         END IF
      END IF
C
C ... 途中経過のendファイル
      IF( IENDF0.GT.0 ) THEN
         IF( ( LENDF.EQ.0 .AND. ISTEP.GE.IENDF(IENDF0) ) .OR.
     $       ( LENDF.EQ.1 .AND. TIME1.GE.RENDF(IENDF0)  ) ) THEN
C
            IF(L0) THEN
               WRITE(STR,'(A1,I7.7)') '_',NINT(TIME)
               CFLNM(IFLNM-3:IFLNM) = '.end'
               OPEN(IFLEN,FILE=CFLNM(1:IFLNM)//STR,STATUS='UNKNOWN',
     $              FORM='FORMATTED',ERR=900)
               WRITE(LP,*) 'OPEN TSUNAMI-RESULT FILE AT ',TIME
               WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM)//STR
            ENDIF
            IF(IAUTOD.EQ.1.AND.IRTN.EQ.0)
     $       CALL GATHERV_PRE(MX,MY,MXG,MYG,MYPROC,NPROC,CHILDCOMM,IRTN)
C
            IF(L0) WRITE(IFLEN,*) '# MAX FREE-SURFACE VALUE '
            IF(L1) WRITE(IFLEN,610) ((VLEND(I,J,3),I=1,MX),J=1,MY)
            IF(L2) CALL GATHERV(VLEND(1,1,3),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
            IF(L0) WRITE(IFLEN,*) '# MAX FREE-SURFACE TIME'
            IF(L1) WRITE(IFLEN,610) ((TMEND(I,J,1),I=1,MX),J=1,MY)
            IF(L2) CALL GATHERV(TMEND(1,1,1),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
            IF(L0) WRITE(IFLEN,*) '# MIN FREE-SURFACE VALUE'
            IF(L1) WRITE(IFLEN,610) ((VLEND(I,J,4),I=1,MX),J=1,MY)
            IF(L2) CALL GATHERV(VLEND(1,1,4),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
            IF(L0) WRITE(IFLEN,*) '# MIN FREE-SURFACE TIME'
            IF(L1) WRITE(IFLEN,610) ((TMEND(I,J,2),I=1,MX),J=1,MY)
            IF(L2) CALL GATHERV(TMEND(1,1,2),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
            IF(L0) WRITE(IFLEN,*) '# TSUNAMI TIME'
            IF(L1) WRITE(IFLEN,610) ((TMEND(I,J,3),I=1,MX),J=1,MY)
            IF(L2) CALL GATHERV(TMEND(1,1,3),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
            IF(L0) WRITE(IFLEN,*) '# MAX VELOCITY VALUE'
            IF(L1) WRITE(IFLEN,610) ((VLEND(I,J,5),I=1,MX),J=1,MY)
            IF(L2) CALL GATHERV(VLEND(1,1,5),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
            IF(L0) WRITE(IFLEN,*) '# MAX VELOCITY TIME'
            IF(L1) WRITE(IFLEN,610) ((TMEND(I,J,4),I=1,MX),J=1,MY)
            IF(L2) CALL GATHERV(TMEND(1,1,4),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
            IF(L0) WRITE(IFLEN,*) '# MAX INUNDATION VALUE'
            IF(L1) WRITE(IFLEN,610) ((FREND(I,J,1),I=1,MX),J=1,MY)
            IF(L2) CALL GATHERV(FREND(1,1,1),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
            IF(L0) WRITE(IFLEN,*) '# MAX INUNDATION TIME'
            IF(L1) WRITE(IFLEN,610) ((FREND(I,J,2),I=1,MX),J=1,MY)
            IF(L2) CALL GATHERV(FREND(1,1,2),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
            DO N=1,NFRAGL
            IF(L0) WRITE(IFLEN,*) '# INUNDATION TIME D=',DFRAGL(N)
            IF(L1) WRITE(IFLEN,610) ((FREND(I,J,N+2),I=1,MX),J=1,MY)
            IF(L2) CALL GATHERV(FREND(1,1,N+2),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
            ENDDO
C<<<<< (START) STOC-DS VERSION  <<<<<<<
            IF( LSTOCDS.EQ.1 ) THEN
               IF(L0) WRITE(IFLEN,*) '# DESTROY FLAG'
               IF(L1) WRITE(IFLEN,'(10I10)') ((IDST(I,J),I=1,MX),J=1,MY)
               IF(L2) CALL GATHERVI(IDST,wrk1,
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
               IF(L0) WRITE(IFLEN,*) '# DESTROY TIME'
               IF(L1) WRITE(IFLEN,610) ((TMDST(I,J),I=1,MX),J=1,MY)
               IF(L2) CALL GATHERV(TMDST,
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
            ENDIF
C<<<<<  (END)  STOC-DS VERSION  <<<<<<<
            IF(LTYPH.EQ.1) THEN
               IF(L0) WRITE(IFLEN,*) '# MAX WIND VALUE'
               IF(L1) WRITE(IFLEN,610) ((VLEND(I,J,8),I=1,MX),J=1,MY)
               IF(L2) CALL GATHERV(VLEND(1,1,8),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
               IF(L0) WRITE(IFLEN,*) '# MAX WIND TIME'
               IF(L1) WRITE(IFLEN,610) ((TMEND(I,J,5),I=1,MX),J=1,MY)
               IF(L2) CALL GATHERV(TMEND(1,1,5),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
               IF(L0) WRITE(IFLEN,*) '# MIN SURFACE PRESSURE '
               IF(L1) WRITE(IFLEN,610) ((VLEND(I,J,11),I=1,MX),J=1,MY)
               IF(L2) CALL GATHERV(VLEND(1,1,11),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
               IF(L0) WRITE(IFLEN,*) '# MIN PRESSURE TIME '
               IF(L1) WRITE(IFLEN,610) ((TMEND(I,J,6),I=1,MX),J=1,MY)
               IF(L2) CALL GATHERV(TMEND(1,1,6),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
            END IF
C
            IF(L0) CLOSE(IFLEN,STATUS='KEEP')
            IF(L0) WRITE(LP,*) 'CLOSE ENDFILE OUTPUT FILE'
C
            IENDF0 = IENDF0 + 1
C
C ......... 指定した時刻(ステップ)を全て出力した場合
            IF( IENDF0.GT.NENDF ) THEN
               IENDF0 = 0
            END IF
         END IF
      END IF
C
C
C----------------------------------------------------------------------
C     (3) ファイルを閉じる
C----------------------------------------------------------------------
      IF( IFLAG.EQ.2 ) THEN
C
C ...... リスタートファイル
         IF( IREST0.GT.0 ) THEN
            CLOSE(IFLRO,STATUS='KEEP')
            WRITE(LP,*) 'CLOSE RESTART OUTPUT FILE'
            WRITE(LP,*) 'FILE NUMBER=',IFLRO
         END IF
CC ...... グラフィックファイル
         IF( IGRPH0.GT.0 ) THEN
            CLOSE(IFLGR,STATUS='KEEP')
            IF( IALFAFLOW.EQ.1 ) THEN
            WRITE(LP,*) 'CLOSE GRAPHIC FILE'
            WRITE(LP,*) 'FILE NUMBER=',IFLGR
            ENDIF
            IF( LAIR.EQ.1 ) THEN
            CLOSE(39,STATUS='KEEP')
            WRITE(LP,*) 'CLOSE GRAPHIC FILE: AIR'
            WRITE(LP,*) 'FILE NUMBER=',39
            ENDIF
C
CXXXXXXXXXXXXX デバッグ用グラフィックファイル 出力
            istep_1=istep
            time_1=time
            igrfil=lp+14
C
            ALLOCATE(uu_1(MX,MY,MZ),vv_1(MX,MY,MZ),ww_1(MX,MY,MZ),
     $               ff_1(MX,MY,MZ),STAT=IERR)
            IF(IERR.NE.0)THEN
               CALL ERRMSG('OUTPUT',7171)
               WRITE(LP,*) 'CANNOT ALLOCATE uu_1,...'
               CALL ABORT1('')
            ENDIF
C
            uu_1(:,:,:)=0.0D0
            vv_1(:,:,:)=0.0D0
            ww_1(:,:,:)=0.0D0
            ff_1(:,:,:)=0.0D0
            DO J=2,MYM
            DO I=2,MXM
            DO K=2,MZM
C     VLEND(*,*, 3) : 最高水位 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               IF(ZC(1,K).LE.VLEND(I,J,3)) THEN
                  ff_1(I,J,K) = 1.0D0
               ELSE IF(ZC(1,K-1).GT.VLEND(I,J,3)) THEN
                  ff_1(I,J,K) = 0.0D0
               ELSE
                  ff_1(I,J,K) = (VLEND(I,J,3)-ZC(1,K-1))*ZC(6,K)
               END IF
C     VLEND(*,*, 6) : 最大流速U(セル中心)
C     VLEND(*,*, 7) : 最大流速V(セル中心)
C     VLEND(*,*, 5) : 最大流速(セル中心) !!!!!!!!!!!!!!!!!!!!!!!!!
               uu_1(i,j,k)=VLEND(I,J,6)
               vv_1(i,j,k)=VLEND(I,J,7)
               ww_1(i,j,k)=VLEND(I,J,5)
            ENDDO
            ENDDO
            ENDDO
C
            istep=1000000
            time=10000.0d0
            if(lsedi.eq.1)then
            do j=2,mym
            do i=2,mxm
               wrk2(i,j,1)=ZBED(I,J)-ZBED0(I,J)
            enddo
            enddo
            endif
            call db_trn(uu_1,vv_1,ww_1,pp,ff_1,tt,cc,gv,wrk1,wrk2,
     $                  indp,kf,lp,igrfil)
C
            uu_1(:,:,:)=0.0D0
            vv_1(:,:,:)=0.0D0
            ww_1(:,:,:)=0.0D0
            ff_1(:,:,:)=0.0D0
            DO J=2,MYM
            DO I=2,MXM
            DO K=2,MZM
               IF(ZC(1,K).LE.VLEND(I,J,4)) THEN
                  ff_1(I,J,K) = 1.0D0
               ELSE IF(ZC(1,K-1).GT.VLEND(I,J,4)) THEN
                  ff_1(I,J,K) = 0.0D0
               ELSE
                  ff_1(I,J,K) = (VLEND(I,J,4)-ZC(1,K-1))*ZC(6,K)
               END IF
C     TMEND(*,*,3)  : 到達時刻
C     TMEND(*,*,1)  : 最大水位時刻
C     TMEND(*,*,2)  : 最低水位時刻
               uu_1(i,j,k)=TMEND(I,J,3)
               vv_1(i,j,k)=TMEND(I,J,1)
               ww_1(i,j,k)=TMEND(I,J,2)
            ENDDO
            ENDDO
            ENDDO
C
            istep=2000000
            time=20000.0d0
            call db_trn(uu_1,vv_1,ww_1,pp,ff_1,tt,cc,gv,wrk1,wrk2,
     $                  indp,kf,lp,igrfil)
C
C           元に戻す
            istep=istep_1
            time=time_1
C
            DEALLOCATE(uu_1,vv_1,ww_1,ff_1,STAT=IERR)
CXXXXXXXXXXXXX END
C
         END IF
C
C ...... 時系列ファイル
         IF( IHIST0.GT.0 ) THEN
            CLOSE(IFLHS,STATUS='KEEP')
            WRITE(LP,*) 'CLOSE HISTORY FILE'
            WRITE(LP,*) 'FILE NUMBER=',IFLHS
         END IF
      END IF
C
      DO 100 J=2,MYM
      DO 110 I=2,MXM
        KPIJ = KP(I,J)
        UC = 0.5D0*(UU(I-1,J,KPIJ)+UU(I,J,KPIJ))
        VC = 0.5D0*(VV(I,J-1,KPIJ)+VV(I,J,KPIJ))
        VEL= DSQRT(UC*UC+VC*VC)
        WXX= 0.5D0*(WX(I-1,J)+WX(I,J))
        WYY= 0.5D0*(WY(I,J-1)+WY(I,J))
        WVL= DSQRT(WXX*WXX+WYY*WYY)
        DEP= MAX(HH(I,J)-HDEP(I,J),0.D0)
C
C ..... 初期海域のセルは浸水判定から除く
cmod16/03/30        IF( VLEND(I,J,2)-VLEND(I,J,1).GE.EPSH ) DEP=0.D0
        IF( VLEND(I,J,2)-VLEND(I,J,1).GE.EPST ) DEP=0.D0
C
        IF(VLEND(I,J,3).LT.HH(I,J)) THEN
          VLEND(I,J,3)=HH(I,J)
          TMEND(I,J,1)=TIME
        ENDIF
        IF(VLEND(I,J,4).GT.HH(I,J)) THEN
          VLEND(I,J,4)=HH(I,J)
          TMEND(I,J,2)=TIME
        ENDIF
ccc        IF(DABS(VLEND(I,J,2)-HH(I,J)).GT.EPST) THEN
        IF(DEP.GT.EPST) THEN
          IF(TMEND(I,J,3).LE.0.0D0) TMEND(I,J,3)=TIME
        ENDIF
        IF(FREND(I,J,1).LT.DEP) THEN
           FREND(I,J,1)=DEP
           FREND(I,J,2)=TIME
        ENDIF
        DO N=1,NFRAGL
           IF(DEP.GT.DFRAGL(N)) THEN
              IF(FREND(I,J,N+2).LE.0.0D0) FREND(I,J,N+2)=TIME
           ENDIF
        ENDDO
        IF(VLEND(I,J,5).LT.VEL) THEN
          VLEND(I,J,5)=VEL
          VLEND(I,J,6)=UC
          VLEND(I,J,7)=VC
          TMEND(I,J,4)=TIME
        ENDIF
        IF(VLEND(I,J,8).LT.WVL) THEN
          VLEND(I,J,8)=WVL
          VLEND(I,J,9)=WXX
          VLEND(I,J,10)=WYY
          TMEND(I,J,5)=TIME
        ENDIF
        IF(VLEND(I,J,11).GT.PATM(I,J)) THEN
          VLEND(I,J,11)=PATM(I,J)
          TMEND(I,J,6)=TIME
        END IF
        IF(LSEDI.EQ.1) THEN
          DO 120 K=2,MZM
          IF(VLEND(I,J,12).LT.CSEDI(I,J,K)) VLEND(I,J,12)=CSEDI(I,J,K)
 120      CONTINUE
          IF(VLEND(I,J,13).LT.CSDAVE(I,J)) VLEND(I,J,13)=CSDAVE(I,J)
          IF(VLEND(I,J,14).LT.SHLSD(I,J)) VLEND(I,J,14)=SHLSD(I,J)
          DZBED=ZBED(I,J)-ZBED0(I,J)
          IF(VLEND(I,J,15).LT.-DZBED) VLEND(I,J,15)=-DZBED
          IF(VLEND(I,J,16).LT.DZBED) VLEND(I,J,16)=DZBED
        END IF
 110  CONTINUE
 100  CONTINUE
C
C
      IF(IFLAG.EQ.2.AND.LSURF.EQ.1) THEN
        IF(L0)THEN
           CFLNM(IFLNM-3:IFLNM) = '.end'
           OPEN(IFLEN,FILE=CFLNM(1:IFLNM),STATUS='NEW',
     $          FORM='FORMATTED',ERR=925)
           WRITE(LP,*) 'OPEN TSUNAMI-RESULT FILE'
           WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM)
           WRITE(LP,*) 'FILE NUMBER=',IFLEN
        ENDIF
        IF(IAUTOD.EQ.1.AND.IRTN.EQ.0)
     $     CALL GATHERV_PRE(MX,MY,MXG,MYG,MYPROC,NPROC,CHILDCOMM,IRTN)
C
        IF(L0) WRITE(IFLEN,*) '# MAX FREE-SURFACE VALUE '
        IF(L1) WRITE(IFLEN,610) ((VLEND(I,J,3),I=1,MX),J=1,MY)
        IF(L2) CALL GATHERV(VLEND(1,1,3),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
        IF(L0) WRITE(IFLEN,*) '# MAX FREE-SURFACE TIME'
        IF(L1) WRITE(IFLEN,610) ((TMEND(I,J,1),I=1,MX),J=1,MY)
        IF(L2) CALL GATHERV(TMEND(1,1,1),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
        IF(L0) WRITE(IFLEN,*) '# MIN FREE-SURFACE VALUE'
        IF(L1) WRITE(IFLEN,610) ((VLEND(I,J,4),I=1,MX),J=1,MY)
        IF(L2) CALL GATHERV(VLEND(1,1,4),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
        IF(L0) WRITE(IFLEN,*) '# MIN FREE-SURFACE TIME'
        IF(L1) WRITE(IFLEN,610) ((TMEND(I,J,2),I=1,MX),J=1,MY)
        IF(L2) CALL GATHERV(TMEND(1,1,2),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
        IF(L0) WRITE(IFLEN,*) '# TSUNAMI TIME'
        IF(L1) WRITE(IFLEN,610) ((TMEND(I,J,3),I=1,MX),J=1,MY)
        IF(L2) CALL GATHERV(TMEND(1,1,3),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
        IF(L0) WRITE(IFLEN,*) '# MAX VELOCITY VALUE'
        IF(L1) WRITE(IFLEN,610) ((VLEND(I,J,5),I=1,MX),J=1,MY)
        IF(L2) CALL GATHERV(VLEND(1,1,5),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
        IF(L0) WRITE(IFLEN,*) '# MAX VELOCITY TIME'
        IF(L1) WRITE(IFLEN,610) ((TMEND(I,J,4),I=1,MX),J=1,MY)
        IF(L2) CALL GATHERV(TMEND(1,1,4),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
        IF(L0) WRITE(IFLEN,*) '# MAX INUNDATION VALUE'
        IF(L1) WRITE(IFLEN,610) ((FREND(I,J,1),I=1,MX),J=1,MY)
        IF(L2) CALL GATHERV(FREND(1,1,1),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
        IF(L0) WRITE(IFLEN,*) '# MAX INUNDATION TIME'
        IF(L1) WRITE(IFLEN,610) ((FREND(I,J,2),I=1,MX),J=1,MY)
        IF(L2) CALL GATHERV(FREND(1,1,2),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
        DO N=1,NFRAGL
        IF(L0) WRITE(IFLEN,*) '# INUNDATION TIME D=',DFRAGL(N)
        IF(L1) WRITE(IFLEN,610) ((FREND(I,J,N+2),I=1,MX),J=1,MY)
        IF(L2) CALL GATHERV(FREND(1,1,N+2),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
        ENDDO
C<<<<< (START) STOC-DS VERSION  <<<<<<<
        IF( LSTOCDS.EQ.1 ) THEN
           IF(L0) WRITE(IFLEN,*) '# DESTROY FLAG'
           IF(L1) WRITE(IFLEN,'(10I10)') ((IDST(I,J),I=1,MX),J=1,MY)
           IF(L2) CALL GATHERVI(IDST,wrk1,
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
           IF(L0) WRITE(IFLEN,*) '# DESTROY TIME'
           IF(L1) WRITE(IFLEN,610) ((TMDST(I,J),I=1,MX),J=1,MY)
           IF(L2) CALL GATHERV(TMDST,
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
        ENDIF
C<<<<<  (END)  STOC-DS VERSION  <<<<<<<
        IF(LTYPH.EQ.1) THEN
          IF(L0) WRITE(IFLEN,*) '# MAX WIND VALUE'
          IF(L1) WRITE(IFLEN,610) ((VLEND(I,J,8),I=1,MX),J=1,MY)
          IF(L2) CALL GATHERV(VLEND(1,1,8),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
          IF(L0) WRITE(IFLEN,*) '# MAX WIND TIME'
          IF(L1) WRITE(IFLEN,610) ((TMEND(I,J,5),I=1,MX),J=1,MY)
          IF(L2) CALL GATHERV(TMEND(1,1,5),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
          IF(L0) WRITE(IFLEN,*) '# MIN SURFACE PRESSURE '
          IF(L1) WRITE(IFLEN,610) ((VLEND(I,J,11),I=1,MX),J=1,MY)
          IF(L2) CALL GATHERV(VLEND(1,1,11),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
          IF(L0) WRITE(IFLEN,*) '# MIN PRESSURE TIME '
          IF(L1) WRITE(IFLEN,610) ((TMEND(I,J,6),I=1,MX),J=1,MY)
          IF(L2) CALL GATHERV(TMEND(1,1,6),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
        END IF
        IF(LSEDI.EQ.1) THEN
          IF(L0) WRITE(IFLEN,*) '# END DEPTH'
          IF(L1) WRITE(IFLEN,610) ((HDEP(I,J),I=1,MX),J=1,MY)
          IF(L2) CALL GATHERV(HDEP,
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
          IF(L0) WRITE(IFLEN,*) '# MAX SUSPENDED LOAD CONCENTRATION'
          IF(L1) WRITE(IFLEN,610) ((VLEND(I,J,12),I=1,MX),J=1,MY)
          IF(L2) CALL GATHERV(VLEND(1,1,12),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
          IF(L0) WRITE(IFLEN,*) '# MAX SUSPENDED LOAD CONC.(Z-AVE)'
          IF(L1) WRITE(IFLEN,610) ((VLEND(I,J,13),I=1,MX),J=1,MY)
          IF(L2) CALL GATHERV(VLEND(1,1,13),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
          IF(L0) WRITE(IFLEN,*) '# MAX SHIELDS NUMBER'
          IF(L1) WRITE(IFLEN,610) ((VLEND(I,J,14),I=1,MX),J=1,MY)
          IF(L2) CALL GATHERV(VLEND(1,1,14),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
          IF(L0) WRITE(IFLEN,*) '# MAX EROSION'
          IF(L1) WRITE(IFLEN,610) ((VLEND(I,J,15),I=1,MX),J=1,MY)
          IF(L2) CALL GATHERV(VLEND(1,1,15),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
          IF(L0) WRITE(IFLEN,*) '# MAX DEPOSITION'
          IF(L1) WRITE(IFLEN,610) (( VLEND(I,J,16),I=1,MX),J=1,MY)
          IF(L2) CALL GATHERV( VLEND(1,1,16),
     $                MX,MY,MXG,MYG,MYPROC,NPROC,INDCOM,CHILDCOMM,IFLEN)
c--debug-write-2012----------------------------------------------------
          write(80,'(/,a)') '# END DEPTH'
          do j=MY,1,-1
             write(80,'(10000e15.7)')(HDEP(i,j),i=1,MX)
          enddo
          write(80,'(/,a)') '# MAX SUSPENDED LOAD CONCENTRATION'
          do j=MY,1,-1
             write(80,'(10000e15.7)')(VLEND(i,j,12),i=1,MX)
          enddo
          write(80,'(/,a)') '# MAX SUSPENDED LOAD CONCENTRATION(Z-AVE)'
          do j=MY,1,-1
             write(80,'(10000e15.7)')(VLEND(i,j,13),i=1,MX)
          enddo
          write(80,'(/,a)') '# MAX SHIELDS NUMBER'
          do j=MY,1,-1
             write(80,'(10000e15.7)')(VLEND(i,j,14),i=1,MX)
          enddo
          write(80,'(/,a)') '# MAX EROSION'
          do j=MY,1,-1
             write(80,'(10000e15.7)')(-VLEND(i,j,15),i=1,MX)
          enddo
          write(80,'(/,a)') '# MAX DEPOSITION'
          do j=MY,1,-1
             write(80,'(10000e15.7)')(VLEND(i,j,16),i=1,MX)
          enddo
c--debug-write-2012----------------------------------------------------
        END IF
  610   FORMAT(1P,10E10.3)
C
        IXXX=0
        IF(IXXX.NE.0) THEN
        WRITE(LP,*) '# RESULT MERGE TIME =',TIME
        WRITE(LP,*) '# MAX FREE-SURFACE VALUE'
        CALL DBWR2D(VLEND,3,3,MX,MY,11,LP)
        WRITE(LP,*) '# MAX FREE-SURFACE TIME'
        CALL DBWR2D(TMEND,3,1,MX,MY,6,LP)
        WRITE(LP,*) '# MIN FREE-SURFACE VALUE'
        CALL DBWR2D(VLEND,3,4,MX,MY,11,LP)
        WRITE(LP,*) '# MIN FREE-SURFACE TIME'
        CALL DBWR2D(TMEND,3,2,MX,MY,6,LP)
        WRITE(LP,*) '# TSUNAMI TIME'
        CALL DBWR2D(TMEND,3,3,MX,MY,6,LP)
        WRITE(LP,*) '# MAX VELOCITY VALUE'
        CALL DBWR2D(VLEND,3,5,MX,MY,11,LP)
        WRITE(LP,*) '# MAX VELOCITY TIME'
        CALL DBWR2D(TMEND,3,4,MX,MY,6,LP)
        IF(LTYPH.EQ.1) THEN
          WRITE(LP,*) '# MAX WIND VALUE'
          CALL DBWR2D(VLEND,3,8,MX,MY,11,LP)
          WRITE(LP,*) '# MAX WIND TIME'
          CALL DBWR2D(TMEND,3,5,MX,MY,6,LP)
          WRITE(LP,*) '# MIN SURFACE PRESSURE '
          CALL DBWR2D(VLEND,3,11,MX,MY,11,LP)
          WRITE(LP,*) '# MIN PRESSURE TIME '
          CALL DBWR2D(TMEND,3,6,MX,MY,6,LP)
        END IF
        END IF
        IF(L0) CLOSE(IFLEN,STATUS='KEEP')
        IF(L0) WRITE(LP,*) 'CLOSE TSUNAMI-RESULT FILE'
        IF(L0) WRITE(LP,*) 'FILE NUMBER=',IFLEN
      END IF
C
c      IF( IFLAG.EQ.0 ) THEN
c         WRITE(31,800)
c      ELSE
c         write(31,810) TIME,SUMVL,SUMTT,SUMCC,
c     $           QISUM,QLSUM,QESUM,QCSUM,QWSUM
c      ENDIF
c  800 FORMAT('# TIM  SUMVL SUMTT SUMCC')
c  810 FORMAT(F18.13,1PE22.15,E22.15,E22.15,
c     $       E12.4,E12.4,E12.4,E12.4,E12.4)
      DEALLOCATE(NF,QBXC,QBYC,UUC,VVC,UVC,STAT=IERR)
      RETURN
C
C ... ファイルオープンエラー
  900 CONTINUE
      CALL ERRMSG('OUTPUT',7172)
      WRITE(LP,*) 'FILE OPEN ERROR: RESTART OUTPUT FILE'
      WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM)
      WRITE(LP,*) 'FILE NUMBER=',IFLRO
      CALL ABORT1('')
C
  910 CONTINUE
      CALL ERRMSG('OUTPUT',7173)
      WRITE(LP,*) 'FILE OPEN ERROR: GRAPHIC FILE'
      WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM)
      WRITE(LP,*) 'FILE NUMBER=',IFLGR
      CALL ABORT1('')
C
  920 CONTINUE
      CALL ERRMSG('OUTPUT',7174)
      WRITE(LP,*) 'FILE OPEN ERROR: HISTORY FILE'
      WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM)
      WRITE(LP,*) 'FILE NUMBER=',IFLHS
      CALL ABORT1('')
C
  925 CONTINUE
      CALL ERRMSG('OUTPUT',7175)
      WRITE(LP,*) 'FILE OPEN ERROR: TSUNAMI-RESULT FILE'
      WRITE(LP,*) 'FILE NAME  =',CFLNM(1:IFLNM)
      WRITE(LP,*) 'FILE NUMBER=',IFLHS
      CALL ABORT1('')
      END
