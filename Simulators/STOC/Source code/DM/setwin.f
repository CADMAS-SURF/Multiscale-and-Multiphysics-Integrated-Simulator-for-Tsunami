      SUBROUTINE SETWIN(IFLAG)
C----------------------------------------
C     風速場を設定する
C----------------------------------------
      USE M_GRID
      USE M_TIME,ONLY:TIME
      USE M_COM_STOC,ONLY: NP_STOC,IP_STOC,IA_STOC
      USE M_FILEIN,ONLY: WWXAR0,WWYAR0,WWXAR1,WWYAR1
     $   ,TIMWIN0,TIMWIN1,IFLWIN,IEOFWIN,ONLINE,OFFLINE
      USE M_FLUID,ONLY: WXAR,WYAR,WXAR1,WYAR1
      USE M_OUTPUT,ONLY:IFL
      USE M_WIND,ONLY: WIN_FIX,WIND_FILE,WIND_U,WIND_V
C
      IMPLICIT NONE
C
      INTEGER,INTENT(IN)::IFLAG
C
      REAL(8),PARAMETER::EPS=1.0D-5
      REAL(8)::C1,C2
      INTEGER::I,J,K,II,JJ,N,NX,NA,NP,NC,NN,IWK,JWK
      INTEGER::NNN
      CHARACTER(80)::FILENAME
C
C ... REAL(4) Variables for reading ".win" data
      REAL(4),ALLOCATABLE::WINX(:,:),WINY(:,:)
      REAL(4):: TIM1,DUMMY
C
C
      IF( IEOFWIN.EQ.1 ) RETURN
C
C----------------------------------------
C     (A) 初期化処理(IFLAG==0)
C----------------------------------------
      IF( IFLAG.EQ.0 ) THEN
         DO N=1,NP_STOC
            NP=IP_STOC(N)
            NN=NP+1
C
            IF( .NOT. WIN_FIX(NN) ) THEN
               FILENAME='./drift/'//trim(WIND_FILE(NN))
               OPEN(IFLWIN(NN),FILE=trim(FILENAME),STATUS='OLD',
     $              FORM='UNFORMATTED',ERR=900)
            ENDIF
C
            TIMWIN1=-1.0D30
         ENDDO
      ENDIF
C
C
C----------------------------------------
C     (B) 必要なときだけ、ファイル読込み
C----------------------------------------
      IF( TIME.GT.TIMWIN1 ) THEN
  800    CONTINUE
C ...... 一つ古い時刻のデータの入れ換え
         TIMWIN0 = TIMWIN1
         WWXAR0  = WWXAR1
         WWYAR0  = WWYAR1
C
         NX=0
         DO N=1,NP_STOC
            NP=IP_STOC(N)
            NC=IA_STOC(N)
            NN=NP+1
C
            IF( .NOT. WIN_FIX(NN) ) THEN
               READ(IFLWIN(NN),END=90) TIM1
               IF( NX.EQ.1 .AND. ABS(TIM1-TIMWIN1).GT.EPS ) THEN
                  WRITE(*,*) 'TIME IS NOT SYNCRONIZED IN .WIN FILE.'
                  WRITE(*,*) 'TIME1=',TIMWIN1
                  WRITE(*,*) 'TIME2=',TIM1
                  CALL ERRMSG('RDWIND',-1)
               ENDIF
               TIMWIN1 = DBLE(TIM1)
               NX=1
C
               IWK=ISUB(3,N)
               JWK=JSUB(3,N)
               ALLOCATE(WINX(IWK,JWK),WINY(IWK,JWK))
               READ(IFLWIN(NN)) ((WINX(I,J),WINY(I,J),
     $                           (DUMMY,K=1,7),I=1,IWK),J=1,JWK)
C
               DO J=JSUB(1,N),JSUB(2,N)
               DO I=ISUB(1,N),ISUB(2,N)
                  II=I-ISUB(1,N)+1
                  JJ=J-JSUB(1,N)+1
                  NNN = NI(NC)*(J-1) + I
                  WWXAR1(NC,NNN)=DBLE(WINX(II,JJ))
                  WWYAR1(NC,NNN)=DBLE(WINY(II,JJ))
               ENDDO
               ENDDO
C
               DEALLOCATE(WINX,WINY)
C
            ELSE
               DO J=JSUB(1,N),JSUB(2,N)
               DO I=ISUB(1,N),ISUB(2,N)
                  NNN = NI(NC)*(J-1) + I
                  WWXAR1(NC,NNN)=WIND_U
                  WWYAR1(NC,NNN)=WIND_V
               ENDDO
               ENDDO
            ENDIF
         ENDDO
C
         IF(NX.EQ.0) GOTO 90   ! ファイルを用いない場合はEOFと同等とみなす
C
CCCCC         CALL REFERW      ! 子領域の物理量で上書きする？
C
         IF( TIME.GT.TIMWIN1 ) GOTO 800    ! リスタート時の読み飛ばし用
      ENDIF
C
C
C----------------------------------------
C     (C) 以下、毎ステップの補間処理(IFLAG==0,1)
C----------------------------------------
      WXAR=WXAR1
      WYAR=WYAR1
C
      C1 = (TIMWIN1-TIME)/MAX(TIMWIN1-TIMWIN0,1.0D-30)
      C2 = 1.0D0 - C1
C
      DO NA=1,MXAREA
         NNN=0
         DO J=1,NJ(NA)
         DO I=1,NI(NA)
            NNN = NNN+1
            WXAR1(NA,NNN) = C1*WWXAR0(NA,NNN)+C2*WWXAR1(NA,NNN)
            WYAR1(NA,NNN) = C1*WWYAR0(NA,NNN)+C2*WWYAR1(NA,NNN)
         ENDDO
         ENDDO
      ENDDO
C
      RETURN
C
C ... 今後、ファイル読み込みを行わない場合
   90 CONTINUE
      IEOFWIN=1
      DO NA=1,MXAREA
         NNN=0
         DO J=1,NJ(NA)
         DO I=1,NI(NA)
            NNN = NNN+1
            WXAR1(NA,NNN) = WWXAR1(NA,NNN)
            WYAR1(NA,NNN) = WWYAR1(NA,NNN)
            WXAR(NA,NNN)  = WWXAR1(NA,NNN)
            WYAR(NA,NNN)  = WWYAR1(NA,NNN)
         ENDDO
         ENDDO
      ENDDO
      RETURN
C
  900 CONTINUE
      WRITE(*,*) 'ERROR: cannot open ',trim(FILENAME)
      CALL ERRMSG('SETWIN',-2)
      END
