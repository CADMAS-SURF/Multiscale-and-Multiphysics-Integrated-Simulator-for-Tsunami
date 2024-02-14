      SUBROUTINE CADMAS_AREA
C----------------------------------------------------------------------
C     CADMASとの接続領域及び格子点の対応について設定する
C     IWCAD, IECAD, JSCAD, JNCAD, KBCAD, KTCAD
C----------------------------------------------------------------------
      use mod_comm,only: comm_ic_mg
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'GRID.h'
      INCLUDE 'AREA.h'
      INCLUDE 'OBSTI.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'mpif.h'
      INCLUDE 'CADMAS.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
      INCLUDE 'AUTODECOMP.h'
      INCLUDE 'FILE.h'
C
C ... WORK VARIABLES
      INTEGER M,N,IRANK,ISIZE,ISTAT(MPI_STATUS_SIZE),IREQ,ITAG
C
      REAL(8):: EPS,X,Y,Z
      INTEGER:: NDIV(8),NX,NY,NZ
      INTEGER:: I,J,K,II,JJ,KK,JSFT,KSFT,IERR,IERR2,JERR
      INTEGER:: MIST(MAX_NIST+1)
      INTEGER:: MJST(MAX_NJST+1)
      INTEGER:: MKST(MAX_NKST+1)
      REAL(8):: WXGRID(NGRDSZ),WYGRID(NGRDSZ)
      INTEGER:: MXMW,MYMW
      INTEGER:: IS,IE,JS,JE
      INTEGER:: IWCAD0,IECAD0,JSCAD0,JNCAD0
C
C
C----------------------------------------------------------------------
C     (0) WXGRID,WYGRIDを設定
C----------------------------------------------------------------------
      II=MOD(LB_STOC-1,NDIVX)+1
      JJ=(LB_STOC-1)/NDIVX+1
C
      IF( LB_STOC.EQ.1 ) THEN ! II=1,JJ=1
         WXGRID(1:MXM)=XGRID(1:MXM)
         WYGRID(1:MYM)=YGRID(1:MYM)
         MXMW=MXM
         MYMW=MYM
         DO II=2,NDIVX
            IRANK=II-1
            ITAG=2000+II
            CALL MPI_IRECV(ISIZE,1,MPI_INTEGER,IRANK,
     $                     ITAG,comm_ic_mg,IREQ,IERR)
            CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
            ITAG=2100+II
            CALL MPI_IRECV(WXGRID(MXMW),ISIZE,MPI_DOUBLE_PRECISION,
     $                     IRANK,ITAG,comm_ic_mg,IREQ,IERR)
            CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
            MXMW=MXMW+ISIZE-1
         ENDDO
C
         DO JJ=2,NDIVY
            IRANK=(JJ-1)*NDIVX
            ITAG=2200+JJ
            CALL MPI_IRECV(ISIZE,1,MPI_INTEGER,IRANK,
     $                     ITAG,comm_ic_mg,IREQ,IERR)
            CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
            ITAG=2300+JJ
            CALL MPI_IRECV(WYGRID(MYMW),ISIZE,MPI_DOUBLE_PRECISION,
     $                     IRANK,ITAG,comm_ic_mg,IREQ,IERR)
            CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
            MYMW=MYMW+ISIZE-1
         ENDDO
      ELSE IF( JJ.EQ.1 ) THEN
         IRANK=IB_STOC(1)
         ITAG=2000+II
         CALL MPI_ISEND(MXM,1,MPI_INTEGER,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
         ITAG=2100+II
         CALL MPI_ISEND(XGRID,MXM,MPI_DOUBLE_PRECISION,
     $                  IRANK,ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
      ELSE IF( II.EQ.1 ) THEN
         IRANK=IB_STOC(1)
         ITAG=2200+JJ
         CALL MPI_ISEND(MYM,1,MPI_INTEGER,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
         ITAG=2300+JJ
         CALL MPI_ISEND(YGRID,MYM,MPI_DOUBLE_PRECISION,
     $                  IRANK,ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
      ENDIF 
C
C----------------------------------------------------------------------
C     (A) CADMASの格子データを受信し、全体用のインデックス(IWCADからKTCAD)
C         と接続サイズ(NIST,NJST,NKST)を設定
C----------------------------------------------------------------------
C ... CADMASと接続する領域の担当のうち、一番先頭のPEが代表して受信する
      IF( LB_STOC.EQ.1 ) THEN
      JERR=0
C
C----------------------------------------
C     (A.1) 格子点数の受信
C----------------------------------------
      IRANK = IB_CADMAS(1)   ! 接続しているCADMASのうち、RANKの最も小さいものから受信
      ISIZE = 3
      ITAG  = ITAGSC
      CALL MPI_IRECV(NDIV,ISIZE,MPI_INTEGER,IRANK,
     $              ITAG,comm_ic_mg,IREQ,IERR)
      CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
      NX = NDIV(1)
      NY = NDIV(2)
      NZ = NDIV(3)
C
C
C----------------------------------------
C     (A.2) X座標の受信とX方向の設定
C----------------------------------------
C ... (2) 格子点座標の受信(X)
      IRANK = IB_CADMAS(1)
      ISIZE = NX
      ITAG  = ITAGSC*2
      CALL MPI_IRECV(CADBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $               ITAG,comm_ic_mg,IREQ,IERR)
      CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
      EPS = (WXGRID(2)-WXGRID(1))*1.0D-3
C
C ... (2b) IWCADの設定
      X   = CADBUF(1)
      DO I=1,MXMW
         IF( ABS(X-WXGRID(I)).LT.EPS ) THEN
            IWCAD = I+1
            EXIT
         ENDIF
      ENDDO
C
C ... (2c) IECADの設定
      X   = CADBUF(NX)
      DO I=1,MXMW
         IF( ABS(X-WXGRID(I)).LT.EPS ) THEN
            IECAD = I
            EXIT
         ENDIF
      ENDDO
C
      IF( IWCAD.EQ.2   ) IWCAD = IWCAD-1
      IF( IECAD.EQ.MXMW) IECAD = IECAD+1
C
C ... (2d) NISTの設定
      NIST = IECAD-IWCAD+1
C
C ... (2e) MISTの設定
      DO I=1,NIST+1
         DO II=1,NX
            IF( ABS(WXGRID(I+IWCAD-2)-CADBUF(II)).LT.EPS ) THEN
               MIST(I) = II+1 ! '+1'はCADMAS側の仮想セル分の補正
               EXIT
            ENDIF
            IF( II.EQ.NX ) THEN
               CALL ERRMSG('CADMAS_AREA',6830)
               WRITE(LP,*) 'ERROR: GRID DATA ARE NOT MATCH',
     $                    ' BETWEEN STOC AND CADMAS'
               WRITE(LP,*) '       X = ',WXGRID(I+IWCAD-2)
               JERR=1
            ENDIF
         ENDDO
      ENDDO
C
C
C----------------------------------------
C     (A.3) Y座標の受信とY方向の設定
C----------------------------------------
C ... (3a) 格子点座標の受信(Y)
      IRANK = IB_CADMAS(1)
      ISIZE = NY
      ITAG  = ITAGSC*3
      CALL MPI_IRECV(CADBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $               ITAG,comm_ic_mg,IREQ,IERR)
      CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
      EPS = (WYGRID(2)-WYGRID(1))*1.0D-3
C
C ... (3b) JSCADの設定
      Y   = CADBUF(1)
      DO J=1,MYMW
         IF( ABS(Y-WYGRID(J)).LT.EPS ) THEN
            JSCAD = J+1
            EXIT
         ENDIF
      ENDDO
C
C ... (3c) JNCADの設定
      Y   = CADBUF(NY)
      DO J=1,MYMW
         IF( ABS(Y-WYGRID(J)).LT.EPS ) THEN
            JNCAD = J
            EXIT
         ENDIF
      ENDDO
C
      IF( JSCAD.EQ.2   ) JSCAD = JSCAD-1
      IF( JNCAD.EQ.MYMW) JNCAD = JNCAD+1
C
C ... (3d) NJSTの設定
      NJST = JNCAD-JSCAD+1
C
C ... (3e) MJSTの設定
      DO J=1,NJST+1
         DO JJ=1,NY
            IF( ABS(WYGRID(J+JSCAD-2)-CADBUF(JJ)).LT.EPS ) THEN
               MJST(J) = JJ+1 ! '+1'はCADMAS側の仮想セル分の補正
               EXIT
            ENDIF
            IF( JJ.EQ.NY ) THEN
               CALL ERRMSG('CADMAS_AREA',6831)
               WRITE(LP,*) 'ERROR: GRID DATA ARE NOT MATCH',
     $                    ' BETWEEN STOC AND CADMAS'
               WRITE(LP,*) '       Y = ',WYGRID(J+JSCAD-2)
               JERR=1
            ENDIF
         ENDDO
      ENDDO
C
C
C----------------------------------------
C     (A.4) Z座標の受信とZ方向の設定
C----------------------------------------
C ... (4a) 格子点座標の受信(Z)
      IRANK = IB_CADMAS(1)
      ISIZE = NZ
      ITAG  = ITAGSC*4
      CALL MPI_IRECV(CADBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $               ITAG,comm_ic_mg,IREQ,IERR)
      CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
      EPS = (ZGRID(2)-ZGRID(1))*1.0D-3
C
C ... (4b) KBCADの設定
      Z   = CADBUF(1)
      DO K=1,MZM
         IF( ABS(Z-ZGRID(K)).LT.EPS ) THEN
            KBCAD = K+1
            EXIT
         ENDIF
      ENDDO
C
C ... (4c) KTCADの設定
      Z   = CADBUF(NZ)
      DO K=1,MZM
         IF( ABS(Z-ZGRID(K)).LT.EPS ) THEN
            KTCAD = K
            EXIT
         ENDIF
      ENDDO
C
C ... (4c) NKSTの設定
      NKST = KTCAD-KBCAD+1
C
C ... (4d) MKSTの設定
      DO K=1,NKST+1
         DO KK=1,NZ
            IF( ABS(ZGRID(K+KBCAD-2)-CADBUF(KK)).LT.EPS ) THEN
               MKST(K) = KK+1 ! '+1'はCADMAS側の仮想セル分の補正
               EXIT
            ENDIF
            IF( KK.EQ.NZ ) THEN
               CALL ERRMSG('CADMAS_AREA',6832)
               WRITE(LP,*) 'ERROR: GRID DATA ARE NOT MATCH',
     $                    ' BETWEEN STOC AND CADMAS'
               WRITE(LP,*) '       Z = ',ZGRID(K+KBCAD-2)
               JERR=1
            ENDIF
         ENDDO
      ENDDO
C
      WRITE(*,*) 'IWCAD,IECAD,JSCAD,JNCAD,KBCAD,KTCAD=',
     $            IWCAD,IECAD,JSCAD,JNCAD,KBCAD,KTCAD
      WRITE(*,*) 'NIST,NJST,NKST=',NIST,NJST,NKST
      WRITE(*,*) 'MIST=',(MIST(I),I=1,NIST+1)
      WRITE(*,*) 'MJST=',(MJST(J),J=1,NJST+1)
      WRITE(*,*) 'MKST=',(MKST(K),K=1,NKST+1)
C
      IF(JERR.EQ.1) THEN
         WRITE(LP,*) 'ERROR: GRID DATA ARE NOT MATCH',
     $              '       BETWEEN STOC-IC AND CADMAS'
         CALL ABORT1('')
      ENDIF
C
c      IF( IWCAD.EQ.3 .OR. IECAD.EQ.MXMW-1 .OR.
c     $    JSCAD.EQ.3 .OR. JNCAD.EQ.MYMW-1 ) THEN
c        CALL ERRMSG('CADMAS_AREA',6833)
c        WRITE(LP,*) 'ERROR: CADMAS REGION IS SET TOO ',
c     $                     'CLOSE TO THE STOC''S BOUNDARY'
c        WRITE(LP,*) '       ROUTINE     = CADMAS_AREA'
c        CALL ABORT1('')
c      ENDIF
      IF( LB_STOC.EQ.1.AND.NDIVX*NDIVY.GT.1 ) THEN
         IERR=0
         II=1
         DO I=1,NDIVX
            II=II+IDIVX(I)
            IF( II.EQ.IWCAD ) THEN
               IERR=1
               IDIVX(I  )=IDIVX(I  )+1
               II=II+1
               IDIVX(I+1)=IDIVX(I+1)-1
            ENDIF
            IF( II.EQ.IECAD-1 ) THEN
               IERR=1
               IDIVX(I  )=IDIVX(I  )-1
               II=II-1
               IDIVX(I+1)=IDIVX(I+1)+1
            ENDIF
         ENDDO
         JJ=1
         DO J=1,NDIVY
            JJ=JJ+JDIVY(J)
            IF( JJ.EQ.JSCAD ) THEN
               IERR=1
               JDIVY(J  )=JDIVY(J  )+1
               JJ=JJ+1
               JDIVY(J+1)=JDIVY(J+1)-1
            ENDIF
            IF( JJ.EQ.JNCAD-1 ) THEN
               IERR=1
               JDIVY(J  )=JDIVY(J  )-1
               JJ=JJ-1
               JDIVY(J+1)=JDIVY(J+1)+1
            ENDIF
         ENDDO
         IF( IERR.EQ.1 ) THEN
            CALL ERRMSG('CADMAS_AREA',6834)
            WRITE(LP,*)'### ERROR: STOC-IC DOMAIN DECOMPSITION LINE IS',
     $         '          SET JUST ON THE INNER BOUNDARY OF CADMAS AREA'
            WRITE(LP,*) ' '
            WRITE(LP,*)'PLEASE MODIFY INPUT DATA OF STOC-IC AS FOLLOING'
            IF( NDIVX.GT.0 )
     $         WRITE(LP,*) 'I-DIV= (',(IDIVX(I),I=1,NDIVX),')'
            IF( NDIVY.GT.0 )
     $         WRITE(LP,*) 'J-DIV= (',(JDIVY(J),J=1,NDIVY),')'
            CALL ABORT1('')
         ENDIF
      ENDIF
C
C
C----------------------------------------------------------------------
C     (B) CADMASへインデックスNIST,NJST,NKST,MIST,MJST,MKSTの送信
C----------------------------------------------------------------------
C ... (5a) 境界同士が接しているかどうかのフラグの送信
      NDIV(1) = 1
      NDIV(2) = 1
      NDIV(3) = 1
      NDIV(4) = 1
      IF( IWCAD.EQ.2    ) NDIV(1) = 0
      IF( IECAD.EQ.MXMW ) NDIV(2) = 0
      IF( JSCAD.EQ.2    ) NDIV(3) = 0
      IF( JNCAD.EQ.MYMW ) NDIV(4) = 0
C
      ENDIF
C
cmod 20141022s
      IRANK=IB_STOC(1)
cmod 20141022e
c bcast(...0) -> bcast(...irank)
      CALL MPI_BCAST(IWCAD,1,MPI_INTEGER,irank,comm_ic_mg,ierr)
      CALL MPI_BCAST(IECAD,1,MPI_INTEGER,irank,comm_ic_mg,ierr)
      CALL MPI_BCAST(JSCAD,1,MPI_INTEGER,irank,comm_ic_mg,ierr)
      CALL MPI_BCAST(JNCAD,1,MPI_INTEGER,irank,comm_ic_mg,ierr)
      CALL MPI_BCAST(KBCAD,1,MPI_INTEGER,irank,comm_ic_mg,ierr)
      CALL MPI_BCAST(KTCAD,1,MPI_INTEGER,irank,comm_ic_mg,ierr)
      CALL MPI_BCAST(NDIV(1),1,MPI_INTEGER,irank,comm_ic_mg,ierr)
      CALL MPI_BCAST(NDIV(2),1,MPI_INTEGER,irank,comm_ic_mg,ierr)
      CALL MPI_BCAST(NDIV(3),1,MPI_INTEGER,irank,comm_ic_mg,ierr)
      CALL MPI_BCAST(NDIV(4),1,MPI_INTEGER,irank,comm_ic_mg,ierr)
C
CDEBUG      write(*,*) 'IWST,IEST,JSST,JNST:',NDIV(1),NDIV(2),NDIV(3),NDIV(4)
C
C
C ... (5b) NIST,NJST,NKSTの送信
      CALL MPI_BCAST(NIST,1,MPI_INTEGER,irank,comm_ic_mg,ierr)
      CALL MPI_BCAST(NJST,1,MPI_INTEGER,irank,comm_ic_mg,ierr)
      CALL MPI_BCAST(NKST,1,MPI_INTEGER,irank,comm_ic_mg,ierr)
C
C ... (6) MISTの送信
      CALL MPI_BCAST(MIST,NIST+1,MPI_INTEGER,irank,comm_ic_mg,ierr)
C
C ... (7) MJSTの送信
      CALL MPI_BCAST(MJST,NJST+1,MPI_INTEGER,irank,comm_ic_mg,ierr)
C
C ... (8) MKSTの送信
      CALL MPI_BCAST(MKST,NKST+1,MPI_INTEGER,irank,comm_ic_mg,ierr)
C
C
C----------------------------------------------------------------------
C     (C) CADMASから各部分領域のサイズを受信
C----------------------------------------------------------------------
C ... CADMASと接続する領域の担当のうち、一番先頭のPEが代表して受信する
      IF( LB_STOC.EQ.1 ) THEN
C
      DO N=1,NB_CADMAS
         IRANK = IB_CADMAS(N)
         ISIZE = 8
         ITAG  = ITAGSC*5+N-1
         CALL MPI_IRECV(NDIV,ISIZE,MPI_INTEGER,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
CDEBUG         write(*,*) 'JJCAD,IICAD:1:',(NDIV(I),I=1,8)
         JJCAD(1,N) = NDIV(1)
         JJCAD(2,N) = NDIV(2)
         JJCAD(3,N) = NDIV(3)+JSCAD-2
         JJCAD(4,N) = NDIV(4)+JSCAD-2
         JJCAD(5,N) = 0
         JJCAD(6,N) = 0
C
         IICAD(1,N) = NDIV(5)
         IICAD(2,N) = NDIV(6)
         IICAD(3,N) = NDIV(7)+IWCAD-2
         IICAD(4,N) = NDIV(8)+IWCAD-2
         IICAD(5,N) = 0
         IICAD(6,N) = 0
CDEBUG         write(*,*) 'N,JJCAD:2:',N,JJCAD(1,N),JJCAD(2,N),
CDEBUG     $                             JJCAD(3,N),JJCAD(4,N)
CDEBUG         write(*,*) 'N,IICAD:2:',N,IICAD(1,N),IICAD(2,N),
CDEBUG     $                             IICAD(3,N),IICAD(4,N)
C
         IF( JJCAD(3,N).EQ.JSCAD.AND.JSCAD.NE.2   ) JJCAD(5,N) = 1
         IF( JJCAD(4,N).EQ.JNCAD.AND.JNCAD.NE.MYMW) JJCAD(6,N) = 1
         IF( IICAD(3,N).EQ.IWCAD.AND.IWCAD.NE.2   ) IICAD(5,N) = 1
         IF( IICAD(4,N).EQ.IECAD.AND.IECAD.NE.MXMW) IICAD(6,N) = 1
CDEBUG         write(*,*) 'N,JJCAD:3:',N,JJCAD(5,N),JJCAD(6,N)
CDEBUG         write(*,*) 'N,IICAD:4:',N,IICAD(5,N),IICAD(6,N)
      ENDDO
C
      ENDIF
cmod 20141022s
      IRANK=IB_STOC(1)
cmod 20141022e
c bcast(...0) -> bcast(...irank)
      CALL MPI_BCAST(JJCAD,6*NB_CADMAS,MPI_INTEGER,irank,
     $               comm_ic_mg,ierr)
      CALL MPI_BCAST(IICAD,6*NB_CADMAS,MPI_INTEGER,irank,
     $               comm_ic_mg,ierr)
C
C
C----------------------------------------------------------------------
C     (D) 自領域分の抽出
C     IWCAD,IECAD,JSCAD,JNCAD,
C     NIST,NJST,
C     JJCAD,IICAD
C----------------------------------------------------------------------
C
      IWCAD0=IWCAD
      IECAD0=IECAD
      JSCAD0=JSCAD
      JNCAD0=JNCAD
C
      NIST = MIN(IECAD,MYIE)-MAX(IWCAD,MYIS)+1
      NJST = MIN(JNCAD,MYJE)-MAX(JSCAD,MYJS)+1
C
C ... CADMAS領域をSTOCの部分領域の内部に含まない場合
C     または、CADMAS領域の完全に内部にSTOCの部分領域がある場合
C     接続用フラグを全て0クリアする
      IF( MYIE.LT.IWCAD.OR.MYIS.GT.IECAD.OR.
     $    MYJE.LT.JSCAD.OR.MYJS.GT.JNCAD ) THEN
         IWCAD=0
         IECAD=0
         JSCAD=0
         JNCAD=0
         NIST=0
         NJST=0
      ELSEIF( MYIS.GT.IWCAD.AND.MYIE.LT.IECAD.AND.
     $        MYJS.GT.JSCAD.AND.MYJE.LT.JNCAD ) THEN
         IWCAD=0
         IECAD=0
         JSCAD=0
         JNCAD=0
         NIST=0
         NJST=0
      ENDIF
C
      IF( IWCAD.LT.MYIS.OR.IWCAD.GT.MYIE ) IWCAD=0
      IF( IECAD.LT.MYIS.OR.IECAD.GT.MYIE ) IECAD=0
      IF( JSCAD.LT.MYJS.OR.JSCAD.GT.MYJE ) JSCAD=0
      IF( JNCAD.LT.MYJS.OR.JNCAD.GT.MYJE ) JNCAD=0
C
C ... 西側と東側でCADMAS境界と接しない場合
      IF( IWCAD.EQ.0.AND.IECAD.EQ.0 ) NJST=0
C ... 南側と北側でCADMAS境界と接しない場合
      IF( JSCAD.EQ.0.AND.JNCAD.EQ.0 ) NIST=0
C
      IIOFF(1)=0
      IIOFF(2)=0
      JJOFF(1)=0
      JJOFF(2)=0
      DO N=1,NB_CADMAS
         IS=IICAD(3,N)
         IE=IICAD(4,N)
         JS=JJCAD(3,N)
         JE=JJCAD(4,N)
C
         IF( NJST.EQ.0.OR.(IWCAD.NE.IS.AND.IECAD.NE.IE) ) THEN
            JJCAD(1:6,N)=0
         ELSE
            JJCAD(3,N)=MAX(JS,MYJS)
            JJCAD(4,N)=MIN(JE,MYJE)
            JJCAD(1,N)=0
            JJCAD(2,N)=0
            IF( JJCAD(4,N).LT.JJCAD(3,N) ) THEN
               JJCAD(3,N)=0
               JJCAD(4,N)=0
            ELSE
               IF( IWCAD.EQ.IS ) JJCAD(1,N)=JJCAD(4,N)-JJCAD(3,N)+1
               IF( IECAD.EQ.IE ) JJCAD(2,N)=JJCAD(4,N)-JJCAD(3,N)+1
            ENDIF
            IF( JSCAD.NE.JS ) JJCAD(5,N)=0
            IF( JNCAD.NE.JE ) JJCAD(6,N)=0
            IF( JJCAD(5,N).EQ.1 ) JJOFF(1)=1
            IF( JJCAD(6,N).EQ.1 ) JJOFF(2)=1
         ENDIF
C
         IF( NIST.EQ.0.OR.(JSCAD.NE.JS.AND.JNCAD.NE.JE) ) THEN
            IICAD(1:6,N)=0
         ELSE
            IICAD(3,N)=MAX(IS,MYIS)
            IICAD(4,N)=MIN(IE,MYIE)
            IICAD(1,N)=0
            IICAD(2,N)=0
            IF( IICAD(4,N).LT.IICAD(3,N) ) THEN
               IICAD(3,N)=0
               IICAD(4,N)=0
            ELSE
               IF( JSCAD.EQ.JS ) IICAD(1,N)=IICAD(4,N)-IICAD(3,N)+1
               IF( JNCAD.EQ.JE ) IICAD(2,N)=IICAD(4,N)-IICAD(3,N)+1
            ENDIF
            IF( IWCAD.NE.IS ) IICAD(5,N)=0
            IF( IECAD.NE.IE ) IICAD(6,N)=0
            IF( IICAD(5,N).EQ.1 ) IIOFF(1)=1
            IF( IICAD(6,N).EQ.1 ) IIOFF(2)=1
         ENDIF
C
      ENDDO
C
C
C----------------------------------------------------------------------
C     (D) IICAD,JJCADをCADMASに送信
C----------------------------------------------------------------------
c      write(180+lb_stoc,*) 'lb_stoc,ib_stoc=',lb_stoc,ib_stoc(1:nb_stoc)
c      write(180+lb_stoc,*) 'lb_cadmas,ib_cadmas=',lb_cadmas,
c     $   ib_cadmas(1:nb_cadmas)
c      write(180+lb_stoc,*) 'nb_cadmas=',nb_cadmas
c      call flush(180+lb_stoc)
c
      DO N=1,NB_CADMAS
         IRANK = IB_CADMAS(N)
         ISIZE = 6
         M     = LB_STOC
         ITAG  = ITAGSC*6+NB_CADMAS*(M-1)+N-1
         CALL MPI_ISEND(JJCAD(1,N),ISIZE,MPI_INTEGER,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
         ITAG  = ITAGSC*7+NB_CADMAS*(M-1)+N-1
         CALL MPI_ISEND(IICAD(1,N),ISIZE,MPI_INTEGER,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
      ENDDO
C
C
C----------------------------------------------------------------------
C     (E) グローバルインデックスからローカルインデックスへの変換
C     IWCAD,IECAD,JSCAD,JNCAD,
C     JJCAD,IICAD
C----------------------------------------------------------------------
C
C ... 境界線のインデックスをローカルインデックスに変換
      IF( IWCAD.NE.0 ) IWCAD=IWCAD-MYIS+2
      IF( IECAD.NE.0 ) IECAD=IECAD-MYIS+2
      IF( JSCAD.NE.0 ) JSCAD=JSCAD-MYJS+2
      IF( JNCAD.NE.0 ) JNCAD=JNCAD-MYJS+2
C
      DO N=1,NB_CADMAS
         IF( JJCAD(3,N).GT.0 ) JJCAD(3,N)=JJCAD(3,N)-MYJS+2
         IF( JJCAD(4,N).GT.0 ) JJCAD(4,N)=JJCAD(4,N)-MYJS+2
         IF( IICAD(3,N).GT.0 ) IICAD(3,N)=IICAD(3,N)-MYIS+2
         IF( IICAD(4,N).GT.0 ) IICAD(4,N)=IICAD(4,N)-MYIS+2
      ENDDO
C
c      write(180+LB_STOC,'(1x,a14,/,2I12)')
c     $   'new:NIST,NJST=',NIST,NJST
c      write(180+LB_STOC,'(1x,a28,/,4I12)')
c     $   'new:IWCAD,IECAD,JSCAD,JNCAD=',
c     $   IWCAD,IECAD,JSCAD,JNCAD
c      write(180+LB_STOC,'(1x,a6,/,9(6i12,:,/))')
c     $   'JJCAD=',((JJCAD(I,J),I=1,6),J=1,NB_CADMAS)
c      write(180+LB_STOC,'(1x,a6,/,9(6i12,:,/))')
c     $   'IICAD=',((IICAD(I,J),I=1,6),J=1,NB_CADMAS)
c      call flush(180+lb_stoc)
C
C
C----------------------------------------------------------------------
C     (F) CADMASとの境界を速度固定境界として設定
C----------------------------------------------------------------------
C
      IOBSS(1,1) = MAX((IWCAD0-MYIS+2)+1,2)
      IOBSS(2,1) = MIN((IECAD0-MYIS+2)-1,MXM)
      IOBSS(3,1) = MAX((JSCAD0-MYJS+2)+1,2)
      IOBSS(4,1) = MIN((JNCAD0-MYJS+2)-1,MYM)
      IF( IOBSS(1,1).GT.IOBSS(2,1).OR.IOBSS(3,1).GT.IOBSS(4,1) )THEN
         IOBSS(1,1) = 0
         IOBSS(2,1) = -999
         IOBSS(3,1) = 0
         IOBSS(4,1) = -999
      ENDIF
      IOBSS(5,1) = KBCAD
      IOBSS(6,1) = KTCAD
C
      IF( IWCAD.EQ.0 ) THEN
         IAREA(1,1) = 0
         IAREA(2,1) = 0
         IAREA(3,1) = 0
         IAREA(4,1) = -999
      ELSE
         IAREA(1,1) = (IWCAD0-MYIS+2)
         IAREA(2,1) = (IWCAD0-MYIS+2)
         IAREA(3,1) = MAX((JSCAD0-MYJS+2)+1,2)
         IAREA(4,1) = MIN((JNCAD0-MYJS+2)-1,MYM)
      ENDIF
      IAREA(5,1) = KBCAD
      IAREA(6,1) = KTCAD
      IAREA(7,1) = 1
      MINLT(1)   = 1
C
      IF( IECAD.EQ.0 ) THEN
         IAREA(1,2) = 0
         IAREA(2,2) = 0
         IAREA(3,2) = 0
         IAREA(4,2) = -999
      ELSE
         IAREA(1,2) = (IECAD0-MYIS+2)-1
         IAREA(2,2) = (IECAD0-MYIS+2)-1
         IAREA(3,2) = MAX((JSCAD0-MYJS+2)+1,2)
         IAREA(4,2) = MIN((JNCAD0-MYJS+2)-1,MYM)
      ENDIF
      IAREA(5,2) = KBCAD
      IAREA(6,2) = KTCAD
      IAREA(7,2) = 1
      MINLT(2)   = 2
C
      IF( JSCAD.EQ.0 ) THEN
         IAREA(1,3) = 0
         IAREA(2,3) = -999
         IAREA(3,3) = 0
         IAREA(4,3) = 0
      ELSE
         IAREA(1,3) = MAX((IWCAD0-MYIS+2)+1,2)
         IAREA(2,3) = MIN((IECAD0-MYIS+2)-1,MXM)
         IAREA(3,3) = (JSCAD0-MYJS+2)
         IAREA(4,3) = (JSCAD0-MYJS+2)
      ENDIF
      IAREA(5,3) = KBCAD
      IAREA(6,3) = KTCAD
      IAREA(7,3) = 2
      MINLT(3)   = 3
C
      IF( JNCAD.EQ.0 ) THEN
         IAREA(1,4) = 0
         IAREA(2,4) = -999
         IAREA(3,4) = 0
         IAREA(4,4) = 0
      ELSE
         IAREA(1,4) = MAX((IWCAD0-MYIS+2)+1,2)
         IAREA(2,4) = MIN((IECAD0-MYIS+2)-1,MXM)
         IAREA(3,4) = (JNCAD0-MYJS+2)-1
         IAREA(4,4) = (JNCAD0-MYJS+2)-1
      ENDIF
      IAREA(5,4) = KBCAD
      IAREA(6,4) = KTCAD
      IAREA(7,4) = 2
      MINLT(4)   = 4
C
c      write(180+LB_STOC,'(1x,a14,/,2I12)')
c     $   'new2:NIST,NJST=',NIST,NJST
c      write(180+LB_STOC,'(a6,/,6I4)') 'IOBSS=',(IOBSS(I,1),I=1,6)
c      write(180+LB_STOC,'(a6,/,4(7I4,/))') 'IAREA=',
c     $   ((IAREA(I,J),I=1,7),J=1,4)
c      call flush(180+lb_stoc)
C
C
      ISIZE = MAX(NIST,NJST)*NKST*6
      IF( ISIZE.GT.MAX_CADBUF ) THEN
        CALL ERRMSG('CADMAS_AREA',6835)
        WRITE(LP,*) 'ERROR: BUFFER SIZE IS OVER LIMIT(MAX_CADBUF)'
        WRITE(LP,*) '       BUFFER SIZE = ',ISIZE
        WRITE(LP,*) '       MAX_CADBUF  = ',MAX_CADBUF
        WRITE(LP,*) '       ROUTINE     = CADMAS_AREA'
        CALL ABORT1('')
      ENDIF
C
      RETURN
      END
