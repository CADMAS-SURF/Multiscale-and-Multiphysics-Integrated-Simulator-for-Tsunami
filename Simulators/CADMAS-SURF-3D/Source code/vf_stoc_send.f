      SUBROUTINE VF_STOC_SEND(XX,YY,ZZ,UU,VV,WW,FF,NF,STBUF,NBUF,
     $                        GGV,FFLXX,FFLXY)

CD=== 概要 ===========================================================
CDT   
C==== 宣言 ===========================================================

      use mod_comm,only: comm_ic_mg
C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'mpif.h'
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_ATIMER.h'
      include 'VF_ASTOCI.h'
      include 'VF_ASTOCR.h'
      include 'VF_AFILEI.h'

CD    -- 引数 --
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),FF  (NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),STBUF(NBUF)
      DIMENSION FFLXX(NUMI,NUMJ,NUMK),FFLXY(NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK)

      DIMENSION ISTT(MPI_STATUS_SIZE)

C==== 実行 ===========================================================

      DTWORK=DTNOW
      IF (NNOW.EQ.0) DTWORK=1.0D0

C     --- 西側U,V,W,HU ---
      IF( IWST.EQ.1 ) THEN
C
      DO M=1,NB_STOC
      NJCAD1 = JJST(1,M)
      IF( NJCAD1.EQ.0 ) CYCLE
C
      I=MIST(2)
      NSFT=NKST*NJCAD1
      NP=0
      DO 130 KST=1,NKST
        DO 120 JST=JJST(3,M),JJST(4,M)
          U=0.0D0
          V=0.0D0
          W=0.0D0
          H=0.0D0
          S=0.0D0
          Q=0.0D0
          DO 110 K=MKST(KST),MKST(KST+1)-1
            DO 100 J=MJST(JST),MJST(JST+1)-1
              F=(FF(I-1,J,K)+FF(I,J,K))*0.5D0
              A=YY(2,J)*ZZ(2,K)
              U=U+A*F*UU(I,J,K)
              V=V+A*F*( VV(I-1,J  ,K)+VV(I  ,J  ,K)
     &                 +VV(I-1,J+1,K)+VV(I  ,J+1,K))*0.25D0
              W=W+A*F*( WW(I-1,J,K  )+WW(I  ,J,K  )
     &                 +WW(I-1,J,K+1)+WW(I  ,J,K+1))*0.25D0
C             H=H+A*F*UU(I,J,K)
              H=H-A*FFLXX(I,J,K)/DTWORK
              S=S+A
              Q=Q+A*F
 100        CONTINUE
 110      CONTINUE
          NP=NP+1
          IF (Q.LE.0.0D0) Q=1.0D0
          STBUF(0*NSFT+NP)=U/Q
          STBUF(1*NSFT+NP)=V/Q
          STBUF(2*NSFT+NP)=W/Q
          STBUF(3*NSFT+NP)=H/S
 120    CONTINUE
 130  CONTINUE
C     --- 水位 ---
      NP=0
      IST=2
      DO 730 JST=JJST(3,M),JJST(4,M)
        H=0.0D0
        S=0.0D0
        DO 720 J=MJST(JST),MJST(JST+1)-1
          DO 710 I=MIST(IST),MIST(IST+1)-1
            SF=0.0D0
            DO 700 K=2,NUMK-1
              IF (NF(I,J,K).EQ.-1) THEN
                SF=SF+ZZ(2,K)*1.0D0
              ELSE
ccc                SF=SF+ZZ(2,K)*(1.0D0-GGV(I,J,K)+FF(I,J,K)*GGV(I,J,K))
                SF=SF+ZZ(2,K)*FF(I,J,K)
              ENDIF
 700        CONTINUE
            A=XX(2,I)*YY(2,J)
            H=H+A*(SF+ZZ(1,2))
            S=S+A
 710      CONTINUE
 720    CONTINUE
        NP=NP+1
        STBUF(4*NSFT+NP)=H/S
 730  CONTINUE
C     --- 水位2 ---
      NP=0
      IST=1
      DO 735 JST=JJST(3,M),JJST(4,M)
        H=0.0D0
        S=0.0D0
        DO 725 J=MJST(JST),MJST(JST+1)-1
          DO 715 I=MIST(IST),MIST(IST+1)-1
            SF=0.0D0
            DO 705 K=2,NUMK-1
              IF (NF(I,J,K).EQ.-1) THEN
                SF=SF+ZZ(2,K)*1.0D0
              ELSE
ccc                SF=SF+ZZ(2,K)*(1.0D0-GGV(I,J,K)+FF(I,J,K)*GGV(I,J,K))
                SF=SF+ZZ(2,K)*FF(I,J,K)
              ENDIF
 705        CONTINUE
            A=XX(2,I)*YY(2,J)
            H=H+A*(SF+ZZ(1,2))
            S=S+A
 715      CONTINUE
 725    CONTINUE
        NP=NP+1
        STBUF(5*NSFT+NP)=H/S
 735  CONTINUE
C
C@    SEND STBUF,6*NSFT
      N = LB_CADMAS
      IRANK = IB_STOC(M)
      ISIZE = 6*NSFT
      ITAG  = ITAGSC*21+NB_CADMAS*(M-1)+N-1
      CALL MPI_ISEND(STBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $               ITAG,comm_ic_mg,IREQ,IERR)
      CALL MPI_WAIT(IREQ,ISTT,IERR)
C
      ENDDO
      ENDIF
C
C     --- 東側U,V,W,HU ---
      IF( IEST.EQ.1 ) THEN
C
      DO M=1,NB_STOC
      NJCAD1 = JJST(2,M)
      IF( NJCAD1.EQ.0 ) CYCLE
C
      I=MIST(NIST)
      NSFT=NKST*NJCAD1
      NP=0
      DO 230 KST=1,NKST
        DO 220 JST=JJST(3,M),JJST(4,M)
          U=0.0D0
          V=0.0D0
          W=0.0D0
          H=0.0D0
          S=0.0D0
          Q=0.0D0
          DO 210 K=MKST(KST),MKST(KST+1)-1
            DO 200 J=MJST(JST),MJST(JST+1)-1
              F=(FF(I-1,J,K)+FF(I,J,K))*0.5D0
              A=YY(2,J)*ZZ(2,K)
              U=U+A*F*UU(I,J,K)
              V=V+A*F*( VV(I-1,J  ,K)+VV(I  ,J  ,K)
     &                 +VV(I-1,J+1,K)+VV(I  ,J+1,K))*0.25D0
              W=W+A*F*( WW(I-1,J,K  )+WW(I  ,J,K  )
     &                 +WW(I-1,J,K+1)+WW(I  ,J,K+1))*0.25D0
C             H=H+A*F*UU(I,J,K)
              H=H-A*FFLXX(I,J,K)/DTWORK
              S=S+A
              Q=Q+A*F
 200        CONTINUE
 210      CONTINUE
          NP=NP+1
          IF (Q.LE.0.0D0) Q=1.0D0
          STBUF(0*NSFT+NP)=U/Q
          STBUF(1*NSFT+NP)=V/Q
          STBUF(2*NSFT+NP)=W/Q
          STBUF(3*NSFT+NP)=H/S
 220    CONTINUE
 230  CONTINUE
C     --- 水位 ---
      NP=0
      IST=NIST-1
      DO 830 JST=JJST(3,M),JJST(4,M)
        H=0.0D0
        S=0.0D0
        DO 820 J=MJST(JST),MJST(JST+1)-1
          DO 810 I=MIST(IST),MIST(IST+1)-1
            SF=0.0D0
            DO 800 K=2,NUMK-1
              IF (NF(I,J,K).EQ.-1) THEN
                SF=SF+ZZ(2,K)*1.0D0
              ELSE
ccc                SF=SF+ZZ(2,K)*(1.0D0-GGV(I,J,K)+FF(I,J,K)*GGV(I,J,K))
                SF=SF+ZZ(2,K)*FF(I,J,K)
              ENDIF
 800        CONTINUE
            A=XX(2,I)*YY(2,J)
            H=H+A*(SF+ZZ(1,2))
            S=S+A
 810      CONTINUE
 820    CONTINUE
        NP=NP+1
        STBUF(4*NSFT+NP)=H/S
 830  CONTINUE
C     --- 水位2 ---
      NP=0
      IST=NIST
      DO 835 JST=JJST(3,M),JJST(4,M)
        H=0.0D0
        S=0.0D0
        DO 825 J=MJST(JST),MJST(JST+1)-1
          DO 815 I=MIST(IST),MIST(IST+1)-1
            SF=0.0D0
            DO 805 K=2,NUMK-1
              IF (NF(I,J,K).EQ.-1) THEN
                SF=SF+ZZ(2,K)*1.0D0
              ELSE
ccc                SF=SF+ZZ(2,K)*(1.0D0-GGV(I,J,K)+FF(I,J,K)*GGV(I,J,K))
                SF=SF+ZZ(2,K)*FF(I,J,K)
              ENDIF
 805        CONTINUE
            A=XX(2,I)*YY(2,J)
            H=H+A*(SF+ZZ(1,2))
            S=S+A
 815      CONTINUE
 825    CONTINUE
        NP=NP+1
        STBUF(5*NSFT+NP)=H/S
 835  CONTINUE
C
C@    SEND STBUF,6*NSFT
      N = LB_CADMAS
      IRANK = IB_STOC(M)
      ISIZE = 6*NSFT
      ITAG  = ITAGSC*22+NB_CADMAS*(M-1)+N-1
      CALL MPI_ISEND(STBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $               ITAG,comm_ic_mg,IREQ,IERR)
      CALL MPI_WAIT(IREQ,ISTT,IERR)
C
      ENDDO
      ENDIF

C     --- 南側U,V,W,HU ---
      IF( JSST.EQ.1 ) THEN
C
      DO M=1,NB_STOC
      NICAD1 = IIST(1,M)
      IF( NICAD1.EQ.0 ) CYCLE
C
      J=MJST(2)
      NSFT=NKST*NICAD1
      NP=0
      DO 330 KST=1,NKST
        DO 320 IST=IIST(3,M),IIST(4,M)
          U=0.0D0
          V=0.0D0
          W=0.0D0
          H=0.0D0
          S=0.0D0
          Q=0.0D0
          DO 310 K=MKST(KST),MKST(KST+1)-1
            DO 300 I=MIST(IST),MIST(IST+1)-1
              F=(FF(I,J-1,K)+FF(I,J,K))*0.5D0
              A=XX(2,I)*ZZ(2,K)
              U=U+A*F*( UU(I  ,J-1,K)+UU(I  ,J,K)
     &                 +UU(I+1,J-1,K)+UU(I+1,J,K))*0.25D0
              V=V+A*F*VV(I,J,K)
              W=W+A*F*( WW(I,J-1,K  )+WW(I,J,K  )
     &                 +WW(I,J-1,K+1)+WW(I,J,K+1))*0.25D0
C             H=H+A*F*VV(I,J,K)
              H=H-A*FFLXY(I,J,K)/DTWORK
              S=S+A
              Q=Q+A*F
 300        CONTINUE
 310      CONTINUE
          NP=NP+1
          IF (Q.LE.0.0D0) Q=1.0D0
          STBUF(0*NSFT+NP)=U/Q
          STBUF(1*NSFT+NP)=V/Q
          STBUF(2*NSFT+NP)=W/Q
          STBUF(3*NSFT+NP)=H/S
 320    CONTINUE
 330  CONTINUE
C     --- 水位 ---
      NP=0
      JST=2
      DO 530 IST=IIST(3,M),IIST(4,M)
        H=0.0D0
        S=0.0D0
        DO 520 J=MJST(JST),MJST(JST+1)-1
          DO 510 I=MIST(IST),MIST(IST+1)-1
            SF=0.0D0
            DO 500 K=2,NUMK-1
              IF (NF(I,J,K).EQ.-1) THEN
                SF=SF+ZZ(2,K)*1.0D0
              ELSE
ccc                SF=SF+ZZ(2,K)*(1.0D0-GGV(I,J,K)+FF(I,J,K)*GGV(I,J,K))
                SF=SF+ZZ(2,K)*FF(I,J,K)
              ENDIF
 500        CONTINUE
            A=XX(2,I)*YY(2,J)
            H=H+A*(SF+ZZ(1,2))
            S=S+A
 510      CONTINUE
 520    CONTINUE
        NP=NP+1
        STBUF(4*NSFT+NP)=H/S
 530  CONTINUE
C     --- 水位2 ---
      NP=0
      JST=1
      DO 535 IST=IIST(3,M),IIST(4,M)
        H=0.0D0
        S=0.0D0
        DO 525 J=MJST(JST),MJST(JST+1)-1
          DO 515 I=MIST(IST),MIST(IST+1)-1
            SF=0.0D0
            DO 505 K=2,NUMK-1
              IF (NF(I,J,K).EQ.-1) THEN
                SF=SF+ZZ(2,K)*1.0D0
              ELSE
ccc                SF=SF+ZZ(2,K)*(1.0D0-GGV(I,J,K)+FF(I,J,K)*GGV(I,J,K))
                SF=SF+ZZ(2,K)*FF(I,J,K)
              ENDIF
 505        CONTINUE
            A=XX(2,I)*YY(2,J)
            H=H+A*(SF+ZZ(1,2))
            S=S+A
 515      CONTINUE
 525    CONTINUE
        NP=NP+1
        STBUF(5*NSFT+NP)=H/S
 535  CONTINUE
C
C@    SEND STBUF,6*NSFT
      N = LB_CADMAS
      IRANK = IB_STOC(M)
      ISIZE = 6*NSFT
      ITAG  = ITAGSC*23+NB_CADMAS*(M-1)+N-1
      CALL MPI_ISEND(STBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $               ITAG,comm_ic_mg,IREQ,IERR)
      CALL MPI_WAIT(IREQ,ISTT,IERR)
C
      ENDDO
      ENDIF

C     --- 北側U,V,W,HU ---
      IF( JNST.EQ.1 ) THEN
C
      DO M=1,NB_STOC
      NICAD1 = IIST(2,M)
      IF( NICAD1.EQ.0 ) CYCLE
C
      J=MJST(NJST)
      NSFT=NKST*NICAD1
      NP=0
      DO 430 KST=1,NKST
        DO 420 IST=IIST(3,M),IIST(4,M)
          U=0.0D0
          V=0.0D0
          W=0.0D0
          H=0.0D0
          S=0.0D0
          Q=0.0D0
          DO 410 K=MKST(KST),MKST(KST+1)-1
            DO 400 I=MIST(IST),MIST(IST+1)-1
              F=(FF(I,J-1,K)+FF(I,J,K))*0.5D0
              A=XX(2,I)*ZZ(2,K)
              U=U+A*F*( UU(I  ,J-1,K)+UU(I  ,J,K)
     &                 +UU(I+1,J-1,K)+UU(I+1,J,K))*0.25D0
              V=V+A*F*VV(I,J,K)
              W=W+A*F*( WW(I,J-1,K  )+WW(I,J,K  )
     &                 +WW(I,J-1,K+1)+WW(I,J,K+1))*0.25D0
C             H=H+A*F*VV(I,J,K)
              H=H-A*FFLXY(I,J,K)/DTWORK
              S=S+A
              Q=Q+A*F
 400        CONTINUE
 410      CONTINUE
          NP=NP+1
          IF (Q.LE.0.0D0) Q=1.0D0
          STBUF(0*NSFT+NP)=U/Q
          STBUF(1*NSFT+NP)=V/Q
          STBUF(2*NSFT+NP)=W/Q
          STBUF(3*NSFT+NP)=H/S
 420    CONTINUE
 430  CONTINUE
C     --- 水位 ---
      NP=0
      JST=NJST-1
      DO 630 IST=IIST(3,M),IIST(4,M)
        H=0.0D0
        S=0.0D0
        DO 620 J=MJST(JST),MJST(JST+1)-1
          DO 610 I=MIST(IST),MIST(IST+1)-1
            SF=0.0D0
            DO 600 K=2,NUMK-1
              IF (NF(I,J,K).EQ.-1) THEN
                SF=SF+ZZ(2,K)*1.0D0
              ELSE
ccc                SF=SF+ZZ(2,K)*(1.0D0-GGV(I,J,K)+FF(I,J,K)*GGV(I,J,K))
                SF=SF+ZZ(2,K)*FF(I,J,K)
              ENDIF
 600        CONTINUE
            A=XX(2,I)*YY(2,J)
            H=H+A*(SF+ZZ(1,2))
            S=S+A
 610      CONTINUE
 620    CONTINUE
        NP=NP+1
        STBUF(4*NSFT+NP)=H/S
 630  CONTINUE
C
C     --- 水位2 ---
      NP=0
      JST=NJST
      DO 635 IST=IIST(3,M),IIST(4,M)
        H=0.0D0
        S=0.0D0
        DO 625 J=MJST(JST),MJST(JST+1)-1
          DO 615 I=MIST(IST),MIST(IST+1)-1
            SF=0.0D0
            DO 605 K=2,NUMK-1
              IF (NF(I,J,K).EQ.-1) THEN
                SF=SF+ZZ(2,K)*1.0D0
              ELSE
ccc                SF=SF+ZZ(2,K)*(1.0D0-GGV(I,J,K)+FF(I,J,K)*GGV(I,J,K))
                SF=SF+ZZ(2,K)*FF(I,J,K)
              ENDIF
 605        CONTINUE
            A=XX(2,I)*YY(2,J)
            H=H+A*(SF+ZZ(1,2))
            S=S+A
 615      CONTINUE
 625    CONTINUE
        NP=NP+1
        STBUF(5*NSFT+NP)=H/S
 635  CONTINUE
C
C@    SEND STBUF,6*NSFT
      N = LB_CADMAS
      IRANK = IB_STOC(M)
      ISIZE = 6*NSFT
      ITAG  = ITAGSC*24+NB_CADMAS*(M-1)+N-1
      CALL MPI_ISEND(STBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $               ITAG,comm_ic_mg,IREQ,IERR)
      CALL MPI_WAIT(IREQ,ISTT,IERR)
C
      ENDDO
      ENDIF
C
C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
