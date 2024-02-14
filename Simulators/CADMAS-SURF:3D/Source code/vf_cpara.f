      SUBROUTINE VF_CPARA()

CD=== 概要 ===========================================================

CDT   VF_CPARA:並列制御データをチェックし設定する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

C==== 実行 ===========================================================

CD    -- 端の値を設定 --
      NUMNPI=NUMNPI+1
      NUMNPJ=NUMNPJ+1
c      write(100+mgrank,*) 'numnpi=',NUMNPI
c      write(100+mgrank,*) 'numnpj=',NUMNPJ
c      write(100+mgrank,*) ''
      IPROCS(0     )=0
      JPROCS(0     )=0
      IPROCS(NUMNPI)=NUMI0-2
      JPROCS(NUMNPJ)=NUMJ0-2

CD    -- 値をシフトしチェック --
      DO 100 I=0,NUMNPI
        IPROCS(I)=IPROCS(I)+1
 100  CONTINUE
      DO 110 J=0,NUMNPJ
        JPROCS(J)=JPROCS(J)+1
 110  CONTINUE
      IF (NUMNPI.GT.1) THEN
        DO 120 I=1,NUMNPI
          IF (IPROCS(I)-IPROCS(I-1).LT.2)
     &                      CALL VF_A2ERR('VF_CPARA','2 > IC2-IC1.')
 120    CONTINUE
      ENDIF
      IF (NUMNPJ.GT.1) THEN
        DO 130 J=1,NUMNPJ
          IF (JPROCS(J)-JPROCS(J-1).LT.2)
     &                      CALL VF_A2ERR('VF_CPARA','2 > JC2-JC1.')
 130    CONTINUE
      ENDIF

c      do i=0,numnpi
c         write(100+mgrank,*) 'iprocs(',i,')=',iprocs(i)
c      enddo
c      do j=0,numnpj
c         write(100+mgrank,*) 'jprocs(',j,')=',jprocs(j)
c      enddo
c      write(100+mgrank,*) ''

CD    -- プロセス数との整合性をチェック --
      IF (NPROCS.NE.NUMNPI*NUMNPJ)
     &                CALL VF_A2ERR('VF_CPARA','NPROCS <> NPI*NPJ.')

CD    -- プロセスの位置 --
      L=0
      DO 210 J=1,NUMNPJ
        DO 200 I=1,NUMNPI
          IF (L.EQ.MYRANK) THEN
            MYRI=I
            MYRJ=J
          ENDIF
          L=L+1
 200    CONTINUE
 210  CONTINUE

CD    -- プロセス単位の格子数等の設定 --
      MYGIS=IPROCS(MYRI-1)+1
      MYGIE=IPROCS(MYRI  )
      MYGJS=JPROCS(MYRJ-1)+1
      MYGJE=JPROCS(MYRJ  )
      NUMI =MYGIE-MYGIS+1
      NUMJ =MYGJE-MYGJS+1
      MYMIS=2
      MYMIE=2
      MYMJS=2
      MYMJE=2
      IF (MYGIS.EQ.2      ) MYMIS=1
      IF (MYGIE.EQ.NUMI0-1) MYMIE=1
      IF (MYGJS.EQ.2      ) MYMJS=1
      IF (MYGJE.EQ.NUMJ0-1) MYMJE=1
      NUMI =NUMI+MYMIS+MYMIE
      NUMJ =NUMJ+MYMJS+MYMJE
      MYGIS=MYGIS-MYMIS
      MYGIE=MYGIE+MYMIE
      MYGJS=MYGJS-MYMJS
      MYGJE=MYGJE+MYMJE
      MYIS =MYMIS+1
      MYIE =NUMI-MYMIE
      MYJS =MYMJS+1
      MYJE =NUMJ-MYMJE

c      write(100+mgrank,*) 'NUMI       :',NUMI
c      write(100+mgrank,*) 'NUMJ       :',NUMJ
c      write(100+mgrank,*) 'MYGIS,MYGIE:',MYGIS,MYGIE
c      write(100+mgrank,*) 'MYGJS,MYGJE:',MYGJS,MYGJE
c      write(100+mgrank,*) 'MYIS ,MYIE :',MYIS,MYIE
c      write(100+mgrank,*) 'MYJS ,MYJE :',MYJS,MYJE
c      write(100+mgrank,*) 'MYMIS,MYMIE:',MYMIS,MYMIE
c      write(100+mgrank,*) 'MYMJS,MYMJE:',MYMJS,MYMJE
c      write(100+mgrank,*) ''

CD    -- バッファ用データの数(1本分)の設定 --
C     NUMBUF=0
C     IF (NUMNPI.GE.2) THEN
C       IF (NUMBUF.LT.NUMJ) NUMBUF=NUMJ
C     ENDIF
C     IF (NUMNPJ.GE.2) THEN
C       IF (NUMBUF.LT.NUMI) NUMBUF=NUMI
C     ENDIF
C     IF (NUMBUF.EQ.0) THEN
C       NUMBUF=1
C     ELSE
C       NUMBUF=NUMBUF*NUMK
C     ENDIF
      IF (NUMI.GT.NUMJ) THEN
        NUMBUF=NUMI*NUMK
      ELSE
        NUMBUF=NUMJ*NUMK
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
