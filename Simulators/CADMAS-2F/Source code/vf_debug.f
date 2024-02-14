      SUBROUTINE VF_DEBUG(CR,NF,FF,GGV,I,J,K)

CD=== 概要 ===========================================================

CDT   VF_DEBUG:デバッグ出力を行う

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ACOMPR.h'
      INCLUDE 'VF_ATIMEI.h'
      INCLUDE 'VF_ATIMER.h'

CD    -- 引数 --
CD    NF(@FOR-3D@)        : I/O : I*4 : セルの状態を示すインデックス
CD    FF(@FOR-3D@)        : I/O  : R*8 : VOF関数F
      CHARACTER*(*) CR
      DIMENSION NF (NUMI,NUMJ,NUMK)
      DIMENSION FF (NUMI,NUMJ,NUMK)
      DIMENSION GGV(NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 並列時のシフト分 --
      IP=MYGIS-1
      JP=MYGJS-1

CD    -- 並列時で担当領域に含まれていなければ何もしない --
      IF (MYGIS.GT.I .OR. I.GT.MYGIE) GOTO 9999
      IF (MYGJS.GT.J .OR. J.GT.MYGJE) GOTO 9999

CD    -- デバッグ出力 --
      IDBGF=2001+MYRANK
      WRITE(IDBGF,'(3A,I5,A,I3,1P,2(A,E12.5))')
     & ' SUBROUTINE(',CR,') NS=',NNOW,'(',ILOOP,') T=',TNOW,
     & ' PLOWER=',PLOWER
      DO JD=J-1,J+1
        WRITE(IDBGF,'(A,I4,A,3(I12,:,'',''))') 'J=',JD,' I=',I-1,I,I+1
        DO KD=K+1,K-1,-1
          WRITE(IDBGF,'(A,I4,2(A,1P,3(E12.4,:,'','')),A,3(I2,:,'',''))')
     &                '  K=',KD,':',(FF (ID,JD,KD),ID=I-1,I+1),
     &                          ':',(GGV(ID,JD,KD),ID=I-1,I+1),
     &                          ':',(NF (ID,JD,KD),ID=I-1,I+1)
        ENDDO
      ENDDO

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
