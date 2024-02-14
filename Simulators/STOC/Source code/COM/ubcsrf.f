      SUBROUTINE UBCSRF(PATM,WX,WY,CD,QQ,QW,RMMB,RMMF)
C======================================================================
C     水面に関するPP,WX,WYの値を設定する
C       WX  : セル中心のX方向風速(m/s)    :RMM(*,*,1)
C       WY  : セル中心のY方向風速(m/s)    :RMM(*,*,2)
C       PATM: セル中心の表面気圧偏差(Pa)  :RMM(*,*,3)<--海面気圧(hPa)
C       CD  : 摩擦係数                    :RMM(*,*,9)
C       QQ  : 表面からの熱流束(W/m2)
C       QW  : 表面からの塩分流束(m/s)
C       QS  : 下向きの短波放射量          :RMM(*,*,4)
C       QB1 : 下向きの長波放射量          :RMM(*,*,5)
C       QE  : 潜熱                        :RMM(*,*,6)
C       QC  : 顕熱(マイナス?)             :RMM(*,*,7)
C       RAIN: 降雨(mm/hour)               :RMM(*,*,8)
C       ALE : 蒸発潜熱
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'FILEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      REAL(8),INTENT(INOUT)::PATM(MX,MY)
      REAL(8),INTENT(INOUT)::WX(MX,MY),WY(MX,MY),CD(MX,MY)
      REAL(8),INTENT(INOUT)::QQ(MX,MY,MZ),QW(MX,MY)
      REAL(8),INTENT(INOUT)::RMMB(MX,MY,9),RMMF(MX,MY,9)
C
      REAL(4)::RMMBS(MX,MY,9),RMMFS(MX,MY,9)
      REAL(4)::TIMFDS,TIMBDS
C
      REAL(8)::ALE=2.45D9
      REAL(8),SAVE::TIMFD,TIMBD,TIMF0
      INTEGER,SAVE::IALLOC=0,I0FLG=0
C
      REAL(8)::DRA,QB1,QC,QE,QS,RAIN,W1,W2
      INTEGER::I,J,K,IRTN
C
C     DATA IRSFLG / 0 /
C     SAVE IRSFLG
C
      if(IALLOC.eq.0)then
        TIME = TIME+0.5D0*DT
      endif
CCC
C
      IF(IALLOC.EQ.0) THEN
C
        CFLNM(IFLNM-3:IFLNM) = '.win'
        write(lp,*) CFLNM(1:IFLNM)
        OPEN(IFLSF,FILE=CFLNM(1:IFLNM),STATUS='OLD',
     $       FORM='UNFORMATTED',ERR=900)
        READ(IFLSF) TIMBDS
        TIMBD=TIMBDS
        IF(IAUTOD.EQ.0)THEN
           READ(IFLSF) (((RMMBS(I,J,K),K=1,9),I=2,MXM),J=2,MYM)
        ELSE
           CALL RDSUB2RBY(RMMBS,IFLSF,0,9,IRTN)
        ENDIF
        IF(TIME.LE.TIMBD) I0FLG=1
        READ(IFLSF) TIMFDS
        TIMFD=TIMFDS
        IF(IAUTOD.EQ.0)THEN
           READ(IFLSF) (((RMMFS(I,J,K),K=1,9),I=2,MXM),J=2,MYM)
        ELSE
           CALL RDSUB2RBY(RMMFS,IFLSF,0,9,IRTN)
        ENDIF
        TIMF0=TIMFD
        IF(TIME.LT.TIMBD) THEN
          CALL ERRMS2('UBCSRF',7220)
          WRITE(LP,*) '### WIND TABLE(TIME<TIMB)### TIME,TIMB='
     $                ,TIME,TIMBD
        END IF
C
        DO 50 K=1,9
        DO 50 J=1,MY
        DO 50 I=1,MX
        RMMB(I,J,K)=RMMBS(I,J,K)
        RMMF(I,J,K)=RMMFS(I,J,K)
  50    CONTINUE
C
      END IF
C
      IF(TIME.GT.TIMBD) I0FLG=0
C
   10 CONTINUE
c     write(*,*)'tanaka ubcsrf 1: time,timbd,timfd=',time,timbd,timfd
      IF(TIME.GT.TIMFD) THEN
        DO 100 K=1,9
        DO 100 J=1,MY
        DO 100 I=1,MX
          RMMB(I,J,K) = RMMF(I,J,K)
  100   CONTINUE
        TIMBD = TIMFD
C
        READ(IFLSF,END=99) TIMFDS
        TIMFD=TIMFDS
        IF(IAUTOD.EQ.0)THEN
           READ(IFLSF,END=99) (((RMMFS(I,J,K),K=1,9),I=2,MXM),J=2,MYM)
        ELSE
           CALL RDSUB2RBY(RMMFS,IFLSF,0,9,IRTN)
           IF(IRTN.EQ.2) GOTO 99
        ENDIF
C
        DO 60 K=1,9
        DO 60 J=1,MY
        DO 60 I=1,MX
          RMMF(I,J,K)=RMMFS(I,J,K)
  60    CONTINUE
C
        IF(TIME.GT.TIMFD) THEN
           GO TO 10
        ELSE
          W2 = (TIME-TIMBD)/(TIMFD-TIMBD)
          W1 = 1.0D0-W2
          DRA = 1.0D-3/3600.D0
          DO 300 J=2,MYM
          DO 350 I=2,MXM
            WX(I,J)   = W1*RMMB(I,J,1)+W2*RMMF(I,J,1)
            WY(I,J)   = W1*RMMB(I,J,2)+W2*RMMF(I,J,2)
            PATM(I,J) = W1*RMMB(I,J,3)+W2*RMMF(I,J,3)
            PATM(I,J) = (PATM(I,J)-1013.25D0)*100.0D0
            QS        = W1*RMMB(I,J,4)+W2*RMMF(I,J,4)
            QB1       = W1*RMMB(I,J,5)+W2*RMMF(I,J,5)
            QE        = W1*RMMB(I,J,6)+W2*RMMF(I,J,6)
            QC        = W1*RMMB(I,J,7)+W2*RMMF(I,J,7)
            RAIN      = W1*RMMB(I,J,8)+W2*RMMF(I,J,8)
            QQ(I,J,1) = QS+QB1+QC-QE                  ! 入熱正(W/m2)
            QW(I,J)   = QE/ALE-RAIN*DRA               ! 上方正(m/s)
            CD(I,J)   = W1*RMMB(I,J,9)+W2*RMMF(I,J,9)
  350     CONTINUE
  300     CONTINUE
C
        ENDIF
        GO TO 20
C
   99   CONTINUE
        TIMFD = 1.0D10
   20   CONTINUE
C
      ELSE
        W2 = (TIME-TIMBD)/(TIMFD-TIMBD)
        W1 = 1.0D0-W2
C
        IF(I0FLG.EQ.1) THEN
          W1=1.0D0
          W2=0.0D0
        END IF
C
          DRA = 1.0D-3/3600.D0
        DO 200 J=2,MYM
        DO 250 I=2,MXM
          WX(I,J)   = W1*RMMB(I,J,1)+W2*RMMF(I,J,1)
          WY(I,J)   = W1*RMMB(I,J,2)+W2*RMMF(I,J,2)
          PATM(I,J) = W1*RMMB(I,J,3)+W2*RMMF(I,J,3)
          PATM(I,J) = (PATM(I,J)-1013.25D0)*100.0D0
          QS        = W1*RMMB(I,J,4)+W2*RMMF(I,J,4)
          QB1       = W1*RMMB(I,J,5)+W2*RMMF(I,J,5)
          QE        = W1*RMMB(I,J,6)+W2*RMMF(I,J,6)
          QC        = W1*RMMB(I,J,7)+W2*RMMF(I,J,7)
          RAIN      = W1*RMMB(I,J,8)+W2*RMMF(I,J,8)
          QQ(I,J,1) = QS+QB1+QC-QE                  ! 入熱正(W/m2)
          QW(I,J)   = QE/ALE-RAIN*DRA               ! 上方正(m/s)
          CD(I,J)   = W1*RMMB(I,J,9)+W2*RMMF(I,J,9)
  250   CONTINUE
  200   CONTINUE
C
C
      END IF
C
C     if(IRSFLG.eq.0)then
      if(IALLOC.eq.0)then
        TIME=TIME-0.5D0*DT
        IALLOC=1
      endif
      RETURN
C
  900 CONTINUE
      CALL ERRMSG('UBCSRF',7221)
      WRITE(LP,*) '### WIND FILE READ ERROR ###'
      CALL ABORT1('')
C
      END
