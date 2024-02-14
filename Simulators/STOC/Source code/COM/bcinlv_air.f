      SUBROUTINE BCINLV_AIR(UUA,VVA,UUBCAIR,VVBCAIR,UUBCAIRB,VVBCAIRB,
     $                      UUBCAIRF,VVBCAIRF,AKBCAIR,EPBCAIR,AKBCAIRB,
     $                      EPBCAIRB,AKBCAIRF,EPBCAIRF,INDUA,INDVA)
C======================================================================
C     流速固定境界の流速の法線方向成分を設定する
C
C     風速データをファイルから読み込む処理を行う
C        <書式：ASCIIフリーフォーマット> 以下を時刻数分繰り返し
C        TIME
C        南側面のU,V (IBCAIRSOU==0のときはスキップ)
C        西側面のU,V (IBCAIRWES==0のときはスキップ)
C        東側面のU,V (IBCAIREAS==0のときはスキップ)
C        北側面のU,V (IBCAIRNOR==0のときはスキップ)
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CP_NESTBC.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'FILEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'AIRI.h'
      INCLUDE 'AIRR.h'
C
      REAL(8),INTENT(OUT)::UUA(MX,MY,MZA),VVA(MX,MY,MZA)
      REAL(8),INTENT(INOUT)::UUBCAIR(NXY,MZA,4),VVBCAIR(NXY,MZA,4)
      REAL(8),INTENT(INOUT)::UUBCAIRB(NXY,MZA,4),VVBCAIRB(NXY,MZA,4)
      REAL(8),INTENT(INOUT)::UUBCAIRF(NXY,MZA,4),VVBCAIRF(NXY,MZA,4)
      REAL(8),INTENT(INOUT)::AKBCAIR(NXY,MZA,4),EPBCAIR(NXY,MZA,4)
      REAL(8),INTENT(INOUT)::AKBCAIRB(NXY,MZA,4),EPBCAIRB(NXY,MZA,4)
      REAL(8),INTENT(INOUT)::AKBCAIRF(NXY,MZA,4),EPBCAIRF(NXY,MZA,4)
      INTEGER,INTENT(IN)::INDUA(MX,MY,MZA),INDVA(MX,MY,MZA)
C
      REAL(8)::C1,C2
      INTEGER::I,J,K
      INTEGER,SAVE::IOPEN=0
C               =0: BEFORE OPEN, =1: WHILE READING, =2:READ END
C
C
C----------------------------------------------------------------
C     ファイル読み込みと時間方向の補間処理
C----------------------------------------------------------------
C ... 最初だけファイルオープンと2時刻分のデータ読込み
      IF( IOPEN.EQ.0.AND.IBCAIRSOU.EQ.1.OR.IBCAIRWES.EQ.1.OR.
     $                   IBCAIREAS.EQ.1.OR.IBCAIRNOR.EQ.1 )THEN
         CFLNM(IFLNM-3:IFLNM) = '.abc'
         write(lp,*) 'OPEN ',CFLNM(1:IFLNM)
         OPEN(IFLABC,FILE=CFLNM(1:IFLNM),FORM='FORMATTED',STATUS='OLD',
     $        ERR=900)
         IOPEN=1
C
         READ(IFLABC,*) TIMEABC2
         IF( IBCAIRSOU.EQ.1 ) THEN
            READ(IFLABC,*) ((UUBCAIRF(I,K,1),I=2,MXM),K=2,MZMA)
            READ(IFLABC,*) ((VVBCAIRF(I,K,1),I=2,MXM),K=2,MZMA)
            IF( LTURBA.EQ.2 ) THEN
            READ(IFLABC,*) ((AKBCAIRF(I,K,1),I=2,MXM),K=2,MZMA)
            READ(IFLABC,*) ((EPBCAIRF(I,K,1),I=2,MXM),K=2,MZMA)
            ENDIF
         ENDIF
         IF( IBCAIRWES.EQ.1 ) THEN
            READ(IFLABC,*) ((UUBCAIRF(J,K,2),J=2,MYM),K=2,MZMA)
            READ(IFLABC,*) ((VVBCAIRF(J,K,2),J=2,MYM),K=2,MZMA)
            IF( LTURBA.EQ.2 ) THEN
            READ(IFLABC,*) ((AKBCAIRF(J,K,2),J=2,MYM),K=2,MZMA)
            READ(IFLABC,*) ((EPBCAIRF(J,K,2),J=2,MYM),K=2,MZMA)
            ENDIF
         ENDIF
         IF( IBCAIREAS.EQ.1 ) THEN
            READ(IFLABC,*) ((UUBCAIRF(J,K,3),J=2,MYM),K=2,MZMA)
            READ(IFLABC,*) ((VVBCAIRF(J,K,3),J=2,MYM),K=2,MZMA)
            IF( LTURBA.EQ.2 ) THEN
            READ(IFLABC,*) ((AKBCAIRF(J,K,3),J=2,MYM),K=2,MZMA)
            READ(IFLABC,*) ((EPBCAIRF(J,K,3),J=2,MYM),K=2,MZMA)
            ENDIF
         ENDIF
         IF( IBCAIRNOR.EQ.1 ) THEN
            READ(IFLABC,*) ((UUBCAIRF(I,K,4),I=2,MXM),K=2,MZMA)
            READ(IFLABC,*) ((VVBCAIRF(I,K,4),I=2,MXM),K=2,MZMA)
            IF( LTURBA.EQ.2 ) THEN
            READ(IFLABC,*) ((AKBCAIRF(I,K,4),I=2,MXM),K=2,MZMA)
            READ(IFLABC,*) ((EPBCAIRF(I,K,4),I=2,MXM),K=2,MZMA)
            ENDIF
         ENDIF
C
         TIMEABC1=TIMEABC2
         UUBCAIRB=UUBCAIRF
         VVBCAIRB=VVBCAIRF
         IF( LTURBA.EQ.2 ) THEN
         AKBCAIRB=AKBCAIRF
         EPBCAIRB=EPBCAIRF
         ENDIF
C
         READ(IFLABC,*,END=10) TIMEABC2
         IF( IBCAIRSOU.EQ.1 ) THEN
            READ(IFLABC,*) ((UUBCAIRF(I,K,1),I=2,MXM),K=2,MZMA)
            READ(IFLABC,*) ((VVBCAIRF(I,K,1),I=2,MXM),K=2,MZMA)
            IF( LTURBA.EQ.2 ) THEN
            READ(IFLABC,*) ((AKBCAIRF(I,K,1),I=2,MXM),K=2,MZMA)
            READ(IFLABC,*) ((EPBCAIRF(I,K,1),I=2,MXM),K=2,MZMA)
            ENDIF
         ENDIF
         IF( IBCAIRWES.EQ.1 ) THEN
            READ(IFLABC,*) ((UUBCAIRF(J,K,2),J=2,MYM),K=2,MZMA)
            READ(IFLABC,*) ((VVBCAIRF(J,K,2),J=2,MYM),K=2,MZMA)
            IF( LTURBA.EQ.2 ) THEN
            READ(IFLABC,*) ((AKBCAIRF(J,K,2),J=2,MYM),K=2,MZMA)
            READ(IFLABC,*) ((EPBCAIRF(J,K,2),J=2,MYM),K=2,MZMA)
            ENDIF
         ENDIF
         IF( IBCAIREAS.EQ.1 ) THEN
            READ(IFLABC,*) ((UUBCAIRF(J,K,3),J=2,MYM),K=2,MZMA)
            READ(IFLABC,*) ((VVBCAIRF(J,K,3),J=2,MYM),K=2,MZMA)
            IF( LTURBA.EQ.2 ) THEN
            READ(IFLABC,*) ((AKBCAIRF(J,K,3),J=2,MYM),K=2,MZMA)
            READ(IFLABC,*) ((EPBCAIRF(J,K,3),J=2,MYM),K=2,MZMA)
            ENDIF
         ENDIF
         IF( IBCAIRNOR.EQ.1 ) THEN
            READ(IFLABC,*) ((UUBCAIRF(I,K,4),I=2,MXM),K=2,MZMA)
            READ(IFLABC,*) ((VVBCAIRF(I,K,4),I=2,MXM),K=2,MZMA)
            IF( LTURBA.EQ.2 ) THEN
            READ(IFLABC,*) ((AKBCAIRF(I,K,4),I=2,MXM),K=2,MZMA)
            READ(IFLABC,*) ((EPBCAIRF(I,K,4),I=2,MXM),K=2,MZMA)
            ENDIF
         ENDIF
C
         GOTO 20
   10    CONTINUE
         IOPEN=2
   20    CONTINUE
      ENDIF
C
C ... 必要であれば次の時刻のデータ読込み
      IF( TIME.GT.TIMEABC2.AND.IOPEN.EQ.1 ) THEN
         TIMEABC1=TIMEABC2
         UUBCAIRB=UUBCAIRF
         VVBCAIRB=VVBCAIRF
         IF( LTURBA.EQ.2 ) THEN
         AKBCAIRB=AKBCAIRF
         EPBCAIRB=EPBCAIRF
         ENDIF
C
         READ(IFLABC,*,END=30) TIMEABC2
         IF( IBCAIRSOU.EQ.1 ) THEN
            READ(IFLABC,*) ((UUBCAIRF(I,K,1),I=2,MXM),K=2,MZMA)
            READ(IFLABC,*) ((VVBCAIRF(I,K,1),I=2,MXM),K=2,MZMA)
            IF( LTURBA.EQ.2 ) THEN
            READ(IFLABC,*) ((AKBCAIRF(I,K,1),I=2,MXM),K=2,MZMA)
            READ(IFLABC,*) ((EPBCAIRF(I,K,1),I=2,MXM),K=2,MZMA)
            ENDIF
         ENDIF
         IF( IBCAIRWES.EQ.1 ) THEN
            READ(IFLABC,*) ((UUBCAIRF(J,K,2),J=2,MYM),K=2,MZMA)
            READ(IFLABC,*) ((VVBCAIRF(J,K,2),J=2,MYM),K=2,MZMA)
            IF( LTURBA.EQ.2 ) THEN
            READ(IFLABC,*) ((AKBCAIRF(J,K,2),J=2,MYM),K=2,MZMA)
            READ(IFLABC,*) ((EPBCAIRF(J,K,2),J=2,MYM),K=2,MZMA)
            ENDIF
         ENDIF
         IF( IBCAIREAS.EQ.1 ) THEN
            READ(IFLABC,*) ((UUBCAIRF(J,K,3),J=2,MYM),K=2,MZMA)
            READ(IFLABC,*) ((VVBCAIRF(J,K,3),J=2,MYM),K=2,MZMA)
            IF( LTURBA.EQ.2 ) THEN
            READ(IFLABC,*) ((AKBCAIRF(J,K,3),J=2,MYM),K=2,MZMA)
            READ(IFLABC,*) ((EPBCAIRF(J,K,3),J=2,MYM),K=2,MZMA)
            ENDIF
         ENDIF
         IF( IBCAIRNOR.EQ.1 ) THEN
            READ(IFLABC,*) ((UUBCAIRF(I,K,4),I=2,MXM),K=2,MZMA)
            READ(IFLABC,*) ((VVBCAIRF(I,K,4),I=2,MXM),K=2,MZMA)
            IF( LTURBA.EQ.2 ) THEN
            READ(IFLABC,*) ((AKBCAIRF(I,K,4),I=2,MXM),K=2,MZMA)
            READ(IFLABC,*) ((EPBCAIRF(I,K,4),I=2,MXM),K=2,MZMA)
            ENDIF
         ENDIF
C
         GOTO 40
   30    CONTINUE
         IOPEN=2
   40    CONTINUE
      ENDIF
C
C ... 時間方向の補間
      C1=0.0D0
      IF( IOPEN.GT.0 ) THEN
         IF( TIME.LE.TIMEABC1 ) THEN
            C1=0.0D0
         ELSEIF( TIME.GE.TIMEABC2 ) THEN
            C1=1.0D0
         ELSE
            C1=(TIME-TIMEABC1)/MAX(TIMEABC2-TIMEABC1,1.D-10)
         ENDIF
      ENDIF
      C2=1.0D0-C1
C
      IF( IBCAIRSOU.EQ.0 ) THEN
         UUBCAIR(2:MXM,2:MZMA,1)=UBCAIRSOU
         VVBCAIR(2:MXM,2:MZMA,1)=VBCAIRSOU
         IF( LTURBA.EQ.2 ) THEN
         AKBCAIR(2:MXM,2:MZMA,1)=AKBCAIRSOU
         EPBCAIR(2:MXM,2:MZMA,1)=EPBCAIRSOU
         ENDIF
      ELSE
         UUBCAIR(2:MXM,2:MZMA,1)
     $      =C1*UUBCAIRB(2:MXM,2:MZMA,1)+C2*UUBCAIRF(2:MXM,2:MZMA,1)
         VVBCAIR(2:MXM,2:MZMA,1)
     $      =C1*VVBCAIRB(2:MXM,2:MZMA,1)+C2*VVBCAIRF(2:MXM,2:MZMA,1)
         IF( LTURBA.EQ.2 ) THEN
         AKBCAIR(2:MXM,2:MZMA,1)
     $      =C1*AKBCAIRB(2:MXM,2:MZMA,1)+C2*AKBCAIRF(2:MXM,2:MZMA,1)
         EPBCAIR(2:MXM,2:MZMA,1)
     $      =C1*EPBCAIRB(2:MXM,2:MZMA,1)+C2*EPBCAIRF(2:MXM,2:MZMA,1)
         ENDIF
      ENDIF
      IF( IBCAIRWES.EQ.0 ) THEN
         UUBCAIR(2:MYM,2:MZMA,2)=UBCAIRWES
         VVBCAIR(2:MYM,2:MZMA,2)=VBCAIRWES
         IF( LTURBA.EQ.2 ) THEN
         AKBCAIR(2:MYM,2:MZMA,2)=AKBCAIRWES
         EPBCAIR(2:MYM,2:MZMA,2)=EPBCAIRWES
         ENDIF
      ELSE
         UUBCAIR(2:MYM,2:MZMA,2)
     $      =C1*UUBCAIRB(2:MYM,2:MZMA,2)+C2*UUBCAIRF(2:MYM,2:MZMA,2)
         VVBCAIR(2:MYM,2:MZMA,2)
     $      =C1*VVBCAIRB(2:MYM,2:MZMA,2)+C2*VVBCAIRF(2:MYM,2:MZMA,2)
         IF( LTURBA.EQ.2 ) THEN
         AKBCAIR(2:MYM,2:MZMA,2)
     $      =C1*AKBCAIRB(2:MYM,2:MZMA,2)+C2*AKBCAIRF(2:MYM,2:MZMA,2)
         EPBCAIR(2:MYM,2:MZMA,2)
     $      =C1*EPBCAIRB(2:MYM,2:MZMA,2)+C2*EPBCAIRF(2:MYM,2:MZMA,2)
         ENDIF
      ENDIF
      IF( IBCAIREAS.EQ.0 ) THEN
         UUBCAIR(2:MYM,2:MZMA,3)=UBCAIREAS
         VVBCAIR(2:MYM,2:MZMA,3)=VBCAIREAS
         IF( LTURBA.EQ.2 ) THEN
         AKBCAIR(2:MYM,2:MZMA,3)=AKBCAIREAS
         EPBCAIR(2:MYM,2:MZMA,3)=EPBCAIREAS
         ENDIF
      ELSE
         UUBCAIR(2:MYM,2:MZMA,3)
     $      =C1*UUBCAIRB(2:MYM,2:MZMA,3)+C2*UUBCAIRF(2:MYM,2:MZMA,3)
         VVBCAIR(2:MYM,2:MZMA,3)
     $      =C1*VVBCAIRB(2:MYM,2:MZMA,3)+C2*VVBCAIRF(2:MYM,2:MZMA,3)
         IF( LTURBA.EQ.2 ) THEN
         AKBCAIR(2:MYM,2:MZMA,3)
     $      =C1*AKBCAIRB(2:MYM,2:MZMA,3)+C2*AKBCAIRF(2:MYM,2:MZMA,3)
         EPBCAIR(2:MYM,2:MZMA,3)
     $      =C1*EPBCAIRB(2:MYM,2:MZMA,3)+C2*EPBCAIRF(2:MYM,2:MZMA,3)
         ENDIF
      ENDIF
      IF( IBCAIRNOR.EQ.0 ) THEN
         UUBCAIR(2:MXM,2:MZMA,4)=UBCAIRNOR
         VVBCAIR(2:MXM,2:MZMA,4)=VBCAIRNOR
         IF( LTURBA.EQ.2 ) THEN
         AKBCAIR(2:MXM,2:MZMA,4)=AKBCAIRNOR
         EPBCAIR(2:MXM,2:MZMA,4)=EPBCAIRNOR
         ENDIF
      ELSE
         UUBCAIR(2:MXM,2:MZMA,4)
     $      =C1*UUBCAIRB(2:MXM,2:MZMA,4)+C2*UUBCAIRF(2:MXM,2:MZMA,4)
         VVBCAIR(2:MXM,2:MZMA,4)
     $      =C1*VVBCAIRB(2:MXM,2:MZMA,4)+C2*VVBCAIRF(2:MXM,2:MZMA,4)
         IF( LTURBA.EQ.2 ) THEN
         AKBCAIR(2:MXM,2:MZMA,4)
     $      =C1*AKBCAIRB(2:MXM,2:MZMA,4)+C2*AKBCAIRF(2:MXM,2:MZMA,4)
         EPBCAIR(2:MXM,2:MZMA,4)
     $      =C1*EPBCAIRB(2:MXM,2:MZMA,4)+C2*EPBCAIRF(2:MXM,2:MZMA,4)
         ENDIF
      ENDIF
C
C
C----------------------------------------------------------------
C     境界面に法線方向流速を設定
C----------------------------------------------------------------
C ... 南側
      IF( IBCAIRSOU.EQ.0.OR.IBCAIRSOU.EQ.1 ) THEN
         J=1
         DO K=2,MZMA
         DO I=2,MXM
            IF( INDVA(I,J,K).EQ.-1 ) THEN
               VVA(I,J,K) = VVBCAIR(I,K,1)
            ELSE
               VVA(I,J,K) = 0.D0
            ENDIF
         ENDDO
         ENDDO
      ENDIF
C
C ... 西側
      IF( IBCAIRWES.EQ.0.OR.IBCAIRWES.EQ.1 ) THEN
         I=1
         DO K=2,MZMA
         DO J=2,MYM
            IF( INDUA(I,J,K).EQ.-1 ) THEN
               UUA(I,J,K) = UUBCAIR(J,K,2)
            ELSE
               UUA(I,J,K) = 0.D0
            ENDIF
         ENDDO
         ENDDO
      ENDIF
C
C ... 東側
      IF( IBCAIREAS.EQ.0.OR.IBCAIREAS.EQ.1 ) THEN
         I=MXM
         DO K=2,MZMA
         DO J=2,MYM
            IF( INDUA(I,J,K).EQ.-1 ) THEN
               UUA(I,J,K) = UUBCAIR(J,K,3)
            ELSE
               UUA(I,J,K) = 0.D0
            ENDIF
         ENDDO
         ENDDO
      ENDIF
C
C ... 北側
      IF( IBCAIRNOR.EQ.0.OR.IBCAIRNOR.EQ.1 ) THEN
         J=MYM
         DO K=2,MZMA
         DO I=2,MXM
            IF( INDVA(I,J,K).EQ.-1 ) THEN
               VVA(I,J,K) = VVBCAIR(I,K,4)
            ELSE
               VVA(I,J,K) = 0.D0
            ENDIF
         ENDDO
         ENDDO
      ENDIF
C
      RETURN
  900 CONTINUE
      CALL ERRMSG('BCINLV_AIR',6820)
      WRITE(LP,*) 'FILE OPEN ERROR AT BCINLV_AIR : ',CFLNM(1:IFLNM)
      CALL ABORT1('')
C
      END
