      SUBROUTINE VF_STOC_AREA(XXWK,YYWK,ZZWK)

CD=== 概要 ===========================================================

CDT   VF_STOC_AREA:STOCとの格子情報等の交換

C==== 宣言 ===========================================================

      use mod_comm,only: comm_ic_mg
C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'mpif.h'
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_ASTOCI.h'
      INCLUDE 'VF_AFILEI.h'

CD    -- 引数 --
      DIMENSION XXWK(NUMI0),YYWK(NUMJ0),ZZWK(NUMK)
      REAL*8 XXWK,YYWK,ZZWK

CD    -- 局所変数 --
      INTEGER I,J,K,N,NWK(8)
      INTEGER IRANK,ISIZE,ISTAT(MPI_STATUS_SIZE),ITAG,IREQ,IERR
      INTEGER ISTT(MPI_STATUS_SIZE)
      INTEGER I1G,I2G,IS,IE
      INTEGER J1G,J2G,JS,JE
      INTEGER IWCAD,IECAD,JSCAD,JNCAD
      INTEGER IIDUM(6,MAX_CADMAS),JJDUM(6,MAX_CADMAS)
      INTEGER IICAD(6),JJCAD(6)

C==== 実行 ===========================================================

C ... STOCと接続する領域の担当PEのうち、一番先頭のPEが代表して送信する
      IF( NB_SC.GT.0 .AND. LB_CADMAS.EQ.1) THEN
        NWK(1)=NUMI0-1
        NWK(2)=NUMJ0-1
        NWK(3)=NUMK-1
        IRANK = IB_STOC(1)
        ISIZE = 3
        ITAG = ITAGSC
        CALL MPI_ISEND(NWK,ISIZE,MPI_INTEGER,IRANK,
     $                 ITAG,comm_ic_mg,IREQ,IERR)
        CALL MPI_WAIT(IREQ,ISTT,IERR)
        IRANK = IB_STOC(1)
        ISIZE = NUMI0-1
        ITAG = ITAGSC*2
        CALL MPI_ISEND(XXWK,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                 ITAG,comm_ic_mg,IREQ,IERR)
        CALL MPI_WAIT(IREQ,ISTT,IERR)
        IRANK = IB_STOC(1)
        ISIZE = NUMJ0-1
        ITAG = ITAGSC*3
        CALL MPI_ISEND(YYWK,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                 ITAG,comm_ic_mg,IREQ,IERR)
        CALL MPI_WAIT(IREQ,ISTT,IERR)
        IRANK = IB_STOC(1)
        ISIZE = NUMK-1
        ITAG = ITAGSC*4
        CALL MPI_ISEND(ZZWK,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                 ITAG,comm_ic_mg,IREQ,IERR)
        CALL MPI_WAIT(IREQ,ISTT,IERR)
C
C
      ENDIF
C
C
C
      IF( NB_SC.GT.0 ) THEN
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
        CALL MPI_BCAST(IWST,1,MPI_INTEGER,irank,comm_ic_mg,ierr)
        CALL MPI_BCAST(IEST,1,MPI_INTEGER,irank,comm_ic_mg,ierr)
        CALL MPI_BCAST(JSST,1,MPI_INTEGER,irank,comm_ic_mg,ierr)
        CALL MPI_BCAST(JNST,1,MPI_INTEGER,irank,comm_ic_mg,ierr)
        IF( MYMIS.EQ.2 ) IWST = 0
        IF( MYMIE.EQ.2 ) IEST = 0
        IF( MYMJS.EQ.2 ) JSST = 0
        IF( MYMJE.EQ.2 ) JNST = 0
C
        CALL MPI_BCAST(NIST,1,MPI_INTEGER,irank,comm_ic_mg,ierr)
        CALL MPI_BCAST(NJST,1,MPI_INTEGER,irank,comm_ic_mg,ierr)
        CALL MPI_BCAST(NKST,1,MPI_INTEGER,irank,comm_ic_mg,ierr)
C
        CALL MPI_BCAST(MIST,NIST+1,MPI_INTEGER,irank,comm_ic_mg,ierr)
        CALL MPI_BCAST(MJST,NJST+1,MPI_INTEGER,irank,comm_ic_mg,ierr)
        CALL MPI_BCAST(MKST,NKST+1,MPI_INTEGER,irank,comm_ic_mg,ierr)
C
C ..... NIST,NJST,MIST,MJSTから自領域分のみ抽出する
C
        I1G = MYGIS+MYMIS
        I2G = MYGIE-MYMIE+1
        IS = 0
        IE = 0
        DO I=1,NIST+1
           IF( MIST(I).EQ.I1G ) IS = I
           IF( MIST(I).EQ.I2G ) IE = I
        ENDDO
C
c        write(80+lb_cadmas,*) 'IW,IE,JS,JE=',IWST,IEST,JSST,JNST
c        write(80+lb_cadmas,*) 'MYGIS,MYMIS=',MYGIS,MYMIS
c        write(80+lb_cadmas,*) 'MYGIE,MYMIE=',MYGIE,MYMIE
c        write(80+lb_cadmas,*) 'MYGJS,MYMJS=',MYGJS,MYMJS
c        write(80+lb_cadmas,*) 'MYGJE,MYMJE=',MYGJE,MYMJE
c        write(80+lb_cadmas,*) 'I1G,I2G=',I1G,I2G
c        write(80+lb_cadmas,*) 'IS,IE=',IS,IE,MIST(1),MIST(NIST+1)
c        call flush(80+lb_cadmas)
C
        IF(IS.GT.0.AND.IE.GT.0) THEN
        NIST = IE - IS
        DO I=1,NIST+1
           MIST(I) = MIST(I-1+IS)-(MYGIS-1)
        ENDDO
        DO I=NIST+2,MAX_NIST
           MIST(I) = 0
        ENDDO
        ENDIF
C
        J1G = MYGJS+MYMJS
        J2G = MYGJE-MYMJE+1
        JS = 0
        JE = 0
        DO J=1,NJST+1
           IF( MJST(J).EQ.J1G ) JS = J
           IF( MJST(J).EQ.J2G ) JE = J
        ENDDO
C
c        write(80+lb_cadmas,*) 'J1G,J2G=',J1G,J2G
c        write(80+lb_cadmas,*) 'JS,JE=',JS,JE,MJST(1),MJST(NJST+1)
c        call flush(80+lb_cadmas)
C
        IF(JS.GT.0.AND.JE.GT.0) THEN
        NJST = JE - JS
        DO J=1,NJST+1
           MJST(J) = MJST(J-1+JS)-(MYGJS-1)
        ENDDO
        DO J=NJST+2,MAX_NJST
           MJST(J) = 0
        ENDDO
        ENDIF
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! NEED AUTO MODIFY
        IF( IS*IE*JS*JE.EQ.0 ) THEN
           WRITE(*,*) 'ERROR: CADMAS DOMAIN DIVISION LINE MUST BE',
     $        '       PLACED ON STOC-IC GRID LINE.'
           WRITE(ILPFIL,*) 'ERROR: CADMAS DOMAIN DIVISION LINE MUST BE',
     $        ' PLACED ON STOC-IC GRID LINE.'
           WRITE(ILPFIL,*) ' MODIFY BELLOW'
           IF(IS*IE.EQ.0 ) WRITE(ILPFIL,810) (IPROCS(I)-1,I=1,NUMNPI-1)
           IF(JS*JE.EQ.0 ) WRITE(ILPFIL,820) (JPROCS(J)-1,J=1,NUMNPJ-1)
  810      format('PARALLEL X',I5)
  820      format('PARALLEL Y',I5)
           CALL VF_ZXMG_ABORT(IERR)
        ENDIF
C
        NWK(1) = NJST
        IF( IWST.EQ.0 ) NWK(1) = 0
        NWK(2) = NJST
        IF( IEST.EQ.0 ) NWK(2) = 0
        NWK(3) = JS+1
        NWK(4) = JE
C
        NWK(5) = NIST
        IF( JSST.EQ.0 ) NWK(5) = 0
        NWK(6) = NIST
        IF( JNST.EQ.0 ) NWK(6) = 0
        NWK(7) = IS+1
        NWK(8) = IE
C
        N = LB_CADMAS
        IRANK = IB_STOC(1)
        ISIZE = 8
        ITAG = ITAGSC*5+N-1
        CALL MPI_ISEND(NWK,ISIZE,MPI_INTEGER,IRANK,
     $                 ITAG,comm_ic_mg,IREQ,IERR)
        CALL MPI_WAIT(IREQ,ISTT,IERR)
C
        CALL MPI_BCAST(JJDUM,6*NB_CADMAS,MPI_INTEGER,irank,
     $                 comm_ic_mg,ierr)
        CALL MPI_BCAST(IIDUM,6*NB_CADMAS,MPI_INTEGER,irank,
     $                 comm_ic_mg,ierr)
C
C ..... STOCからJJCAD,IICADを受信してJJST,IISTを設定する
c
        DO M=1,NB_STOC
           IRANK = IB_STOC(M)
           ISIZE = 6
           N     = LB_CADMAS
           ITAG  = ITAGSC*6+NB_CADMAS*(M-1)+N-1
           CALL MPI_IRECV(JJCAD,ISIZE,MPI_INTEGER,IRANK,
     $                    ITAG,comm_ic_mg,IREQ,IERR)
           CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
           ITAG  = ITAGSC*7+NB_CADMAS*(M-1)+N-1
           CALL MPI_IRECV(IICAD,ISIZE,MPI_INTEGER,IRANK,
     $                    ITAG,comm_ic_mg,IREQ,IERR)
           CALL MPI_WAIT(IREQ,ISTAT,IERR)
C
           JJST(1,M) = JJCAD(1)
           JJST(2,M) = JJCAD(2)
           IF( JJCAD(1).GT.0.OR.JJCAD(2).GT.0 ) THEN
              JJST(3,M) = JJCAD(3)-JSCAD-JS+2
              JJST(4,M) = JJCAD(4)-JSCAD-JS+2
           ENDIF
           IIST(1,M) = IICAD(1)
           IIST(2,M) = IICAD(2)
           IF( IICAD(1).GT.0.OR.IICAD(2).GT.0 ) THEN
              IIST(3,M) = IICAD(3)-IWCAD-IS+2
              IIST(4,M) = IICAD(4)-IWCAD-IS+2
           ENDIF
        ENDDO
c
c        write(80+lb_cadmas,*) 'NB_STOC=',NB_STOC
c        write(80+lb_cadmas,'(a24,/,4I4)')
c     $     'IWCAD,IECAD,JSCAD,JECAD=',IWCAD,IECAD,JSCAD,JNCAD
c        write(80+lb_cadmas,'(a5,/,9(4I4,/))')
c     $     'JJST=',((JJST(I,J),I=1,4),J=1,NB_STOC)
c        write(80+lb_cadmas,'(a5,/,9(4I4,/))')
c     $     'IIST=',((IIST(I,J),I=1,4),J=1,NB_STOC)
c        call flush(80+lb_cadmas)
C
      ENDIF
C
      RETURN
      END
