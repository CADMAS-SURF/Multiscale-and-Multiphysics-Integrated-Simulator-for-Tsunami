      SUBROUTINE UBCSR2(PATM,WX,WY,QQ,QW,TT,ZC,HH,HDEP,RMMB,RMMF,
     $                  INDP,KH,KF,KG)
C======================================================================
C     水面に関するQQ,QW,PATM,WX,WYの値を設定する
C       WX  : セル中心のX方向風速(m/s)    :RMM(*,*,1)
C       WY  : セル中心のY方向風速(m/s)    :RMM(*,*,2)
C       PATM: セル中心の表面気圧偏差(Pa)  :RMM(*,*,3)<--海面気圧(hPa)
C       QQ  : 表面からの熱流束(W/m2)
C       QW  : 表面からの塩分流束(m/s)
C       QS  : 下向きの短波放射量          :RMM(*,*,4)
C       QB1 : 下向きの長波放射量          :RMM(*,*,5)
C       QE  : 潜熱                        :RMM(*,*,6)
C       QC  : 顕熱(マイナス?)             :RMM(*,*,7)
C       RAIN: 降雨(mm/hour)               :RMM(*,*,8)
C======================================================================
      use mod_comm,only: comm_model
      IMPLICIT NONE
C
      INCLUDE 'mpif.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'FILEI.h'
      INCLUDE 'TIMER.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'BOUNDR.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'CONSRV.h'
C
      REAL(8),INTENT(OUT)::PATM(MX,MY)
      REAL(8),INTENT(OUT)::WX(MX,MY),WY(MX,MY)
      REAL(8),INTENT(OUT)::QQ(MX,MY,MZ),QW(MX,MY)
      REAL(8),INTENT(IN)::TT(MX,MY,MZ)
      REAL(8),INTENT(IN)::ZC(8,MZ)
      REAL(8),INTENT(IN)::HH(MX,MY),HDEP(MX,MY)
      REAL(8),INTENT(INOUT)::RMMB(MX,MY,9),RMMF(MX,MY,9)
      INTEGER,INTENT(IN)::INDP(MX,MY,MZ)
      INTEGER,INTENT(IN)::KH(MX,MY),KF(MX,MY),KG(MX,MY)
C
      REAL(8),SAVE::DELTAT,TIMFD,TIMBD
      INTEGER,SAVE::IALLOC=0,IFLAG=1,IEND=0
C
      REAL(8),PARAMETER::STEFG = 5.6704D-8
      REAL(8),PARAMETER::AL1   = 2.453D6
      REAL(8),PARAMETER::ALE   = 2.453D9
      REAL(8),PARAMETER::DRA   = 1.0D-3 / 3600.D0
      REAL(8),PARAMETER::EPSL  = 0.96D0
      REAL(8),PARAMETER::A1    = 7.5D0
      REAL(8),PARAMETER::B1    = 237.3D0                     !checkcheckcheckcheckcheckcheckcheckcheck
      REAL(8),PARAMETER::CAP   = 1005.0D0
C
      REAL(8)::C0,C1
      REAL(8)::RID,RLD,PA,TA,WX1,WY1,EE,RAIN
      REAL(8)::TW,RLU,QL1,QL2,RHOA,WA,W10,CE,ESAT,QS,QA,QE,CC,QC
      REAL(8)::QIU,QID,CHL,H1,H2,DH,RKEXT
      INTEGER::I,J,K,L,IERR,IERR2
C
C
C ... 最初だけファイルオープンと2時刻分のデータ読込み
      IF(IALLOC.EQ.0) THEN
C
         CFLNM(IFLNM-3:IFLNM) = '.wea'
         write(lp,*) CFLNM(1:IFLNM)
         OPEN(IFLSF,FILE=CFLNM(1:IFLNM),STATUS='OLD',
     $        FORM='FORMATTED',ERR=900)
         READ(IFLSF,*) IFLAG,DELTAT
C
         TIMBD=0.0D0
         CALL RDWEA(RMMB,IEND,IFLSF,IFLAG,LP)
C
         TIMFD = TIMBD+DELTAT
         CALL RDWEA(RMMF,IEND,IFLSF,IFLAG,LP)
C
         IALLOC = 1
      ENDIF
C
C
C ... 計算時刻がTIMFDを超えるとデータを更新
      IF( IEND.EQ.0.AND.TIME.GT.TIMFD ) THEN
         TIMBD = TIMFD
         RMMB = RMMF
C
         TIMFD = TIMBD + DELTAT
         CALL RDWEA(RMMF,IEND,IFLSF,IFLAG,LP)
      ENDIF
C
C
C ... データを時間補間
      IF( IEND.EQ.1 ) THEN
         C0 = 1.0D0
         C1 = 0.0D0
      ELSE
         C0 = ( TIMFD - TIME )/DELTAT
         C1 = 1.0D0 - C0
      ENDIF
C
C
      QQ = 0.0D0
      QW = 0.0D0
      QISUM = 0.0D0
      QLSUM = 0.0D0
      QESUM = 0.0D0
      QCSUM = 0.0D0
      QWSUM = 0.0D0
C
      DO J=2,MYM
      DO I=2,MXM
         RID  = C0*RMMB(I,J,1)+C1*RMMF(I,J,1)
         RLD  = C0*RMMB(I,J,2)+C1*RMMF(I,J,2)
         PA   = C0*RMMB(I,J,3)+C1*RMMF(I,J,3)
         TA   = C0*RMMB(I,J,4)+C1*RMMF(I,J,4)
         WX1  = C0*RMMB(I,J,5)+C1*RMMF(I,J,5)
         WY1  = C0*RMMB(I,J,6)+C1*RMMF(I,J,6)
         EE   = C0*RMMB(I,J,7)+C1*RMMF(I,J,7)
         RAIN = C0*RMMB(I,J,8)+C1*RMMF(I,J,8)
C
         PATM(I,J) = (PA-1013.25D0)*100.0D0
         WX(I,J)   = WX1
         WY(I,J)   = WY1
C
         TW        = TT(I,J,KH(I,J))
         RLU       = STEFG*((TW+273.15D0)**4)
         QL1       = EPSL*RLD
         QL2       = EPSL*(-RLU)
C
         RHOA      = 1.293D0*PA/1013.25D0/(1.0D0+0.00367*TA)
         WA        = SQRT(WX1**2+WY1**2)
         W10       = WA*AWIND10
         IF( W10.GT.5.0D0 ) THEN
            CE     = 1.25D-3
         ELSE
            CE     = 1.15D-3
         ENDIF
         ESAT      = 6.1078D0* (10.0D0**(A1*TW/(B1+TW)))      ! (hPa)
         QS        = 0.622D0*(ESAT/PA)/(1.0D0-0.378D0*(ESAT/PA))
         QA        = 0.622D0*(EE  /PA)/(1.0D0-0.378D0*(EE  /PA))
         QE        = RHOA*AL1*CE*(QS-QA)*WA
C
         CC        = CE
         QC        = RHOA*CAP*CC*(TW-TA)*WA
C
         QID       = (1.0D0-ALBEDO)*RID
C
         IF( KF(I,J).EQ.KG(I,J) ) THEN
            K = KF(I,J)
            DH = HH(I,J)-HDEP(I,J)
            IF( DH.LT.1.0D-3 ) THEN
               QL1 = 0.0D0
               QL2 = 0.0D0
               QE  = 0.0D0
               QC  = 0.0D0
               RAIN= 0.0D0
            ENDIF
         ELSE
            K = KF(I,J)-1
         ENDIF
         QW(I,J)   = QE/ALE-RAIN*DRA                 ! 上方正(m/s)
         QQ(I,J,K) = QL1+QL2-QE-QC                   ! 入熱正(W/m2)
         QLSUM = QLSUM + QL1+QL2
         QESUM = QESUM - QE
         QCSUM = QCSUM - QC
C
      ENDDO
      ENDDO
C      
      RETURN
  900 CONTINUE
      CALL ERRMSG('UBCSR2',7210)
      WRITE(LP,*) 'FILE OPEN ERROR AT UBCSR2 : ',CFLNM(1:IFLNM)
      CALL ABORT1('')
C
      END
C
C
      SUBROUTINE RDWEA(RMMS,IEND,IFLSF,IFLAG,LP)
C----------------------------------------------------------------------
C     気象データファイル(*.wea)のデータ部を読み込む
C----------------------------------------------------------------------
      use mod_comm,only: comm_model
      IMPLICIT NONE
C
      INCLUDE 'mpif.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
      REAL(8),INTENT(OUT)::RMMS(MX,MY,9)
      INTEGER,INTENT(OUT)::IEND
      INTEGER,INTENT(IN)::IFLSF,LP,IFLAG
C

      REAL(8):: RMMX(8)
      INTEGER::N,I,J,K,I1,J1,IERR,IERR2,IX,JX,INSIDE
      CHARACTER(1)::CHA
C
C
      IF( IFLAG.EQ.0 ) THEN
         READ(IFLSF,'(A1,I10)',END=200) CHA,N
         READ(IFLSF,*,END=200) (RMMS(1,1,K),K=1,8)
C
         DO K=1,8
         DO J=2,MYM
         DO I=2,MXM
            RMMS(I,J,K) = RMMS(1,1,K)
         ENDDO
         ENDDO
         ENDDO
      ELSE
         READ(IFLSF,'(A1,I10)',END=200) CHA,N
         IF(IAUTOD.EQ.0)THEN
            DO J=2,MYM
            DO I=2,MXM
               READ(IFLSF,*,END=200) I1,J1,(RMMS(I,J,K),K=1,8)
               IF( I.NE.I1.OR.J.NE.J1 ) THEN
                  CALL ERRMSG('UBCSR2',7211)
                  WRITE(LP,*) 'READ ERROR AT XXX.wea'
                  CALL ABORT1('')
               ENDIF
            ENDDO
            ENDDO
         ELSE
            DO J=2,MYG-1
            DO I=2,MXG-1
               READ(IFLSF,*,END=200) I1,J1,(RMMX(K),K=1,8)
               IF( MYPROC.EQ.1.AND.I.NE.I1.OR.J.NE.J1 ) THEN
                  CALL ERRMSG('UBCSR2',7212)
                  WRITE(LP,*) 'READ ERROR AT XXX.wea'
                  CALL ABORT1('')
               ENDIF
               IX=I1
               JX=J1
               CALL MODIJ(I1,IX,J1,JX,0,INSIDE)
               IF(INSIDE.EQ.1) THEN
                  DO K=1,8
                     RMMS(I1,J1,K)=RMMX(K)
                  ENDDO
               ENDIF
            ENDDO
            ENDDO
         ENDIF
      ENDIF
      WRITE(*,*) 'READ WEA DATA. STEP NO.=',N
C
      IEND = 0
      RETURN
C
  200 CONTINUE
      IEND = 1
      RETURN
      END
