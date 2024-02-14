      SUBROUTINE SOLVER
C----------------------------------------
C     時間積分処理のメインルーチン
C----------------------------------------
      USE M_TIME
      USE M_GRID
      USE M_MODEL,ONLY:SINK_MODEL
      USE M_DRIFT
      USE M_FLUID
      USE M_OUTPUT,ONLY:IFL
      USE M_FILEIN,ONLY:ONLINE
      USE M_WIND,ONLY:WIN_CAL
C
      IMPLICIT NONE
C
      INTEGER::NS1,NE1
C
C
      call ftimer('total','start')

      CALL SETFLW(0)   ! 流体の速度場を設定
      IF( IREST.EQ.0 ) THEN
      HFAR = HFAR1     ! 最初だけ必要
      ENDIF
      IF( WIN_CAL ) THEN
         CALL SETWIN(0) ! 風速場を設定
      ENDIF
C
      CALL SETAREA     ! 漂流物が存在する領域番号を設定
C
      CALL SETLVL      ! 漂流物位置の水位の設定及び地面への乗り上げ判定
C
      CALL SETFENCE(0) ! フェンス状構造物の高さを設定
C
      CALL OUTPUT(0)   ! ファイルのオープンと初期状態の出力
C
      IF( IREST.EQ.0 ) THEN
         NS1 = 1
         NE1 = MAXSTP
      ELSE             ! リスタート時の時間積分ループの開始終了設定
         NS1 = NS+1
         NE1 = MAXSTP+NS1-1
      ENDIF
C
      WRITE(IFL,*) '### START TIME INTEGRATION LOOP '
C
C----------------------------------------
C     時間積分ループ(開始)
C----------------------------------------
      call ftimer('timeloop','start')
      DO NS=NS1,NE1
C
         call ftimer('setdt','start')
         CALL SETDT       ! 時間刻みを設定
         call ftimer('setdt','end')
C
         TIME = TIME + DT
C
         call ftimer('setflw','start')
         CALL SETFLW(1)   ! 流体の速度場を設定
         call ftimer('setflw','end')
C
         IF( WIN_CAL ) THEN
            call ftimer('setwin','start')
            CALL SETWIN(1) ! 風速場を設定
            call ftimer('setwin','end')
         ENDIF
C
         call ftimer('setarea','start')
         CALL SETAREA     ! 漂流物が存在する領域番号を設定
         call ftimer('setarea','end')
C
         call ftimer('setlvl','start')
         CALL SETLVL      ! 漂流物位置の水位の設定及び地面への乗り上げ判定
         call ftimer('setlvl','end')
C
         call ftimer('setfence','start')
         CALL SETFENCE(1) ! フェンス状構造物の浸水深による破壊を判定
         call ftimer('setfence','end')
C
         call ftimer('drift','start')
         CALL DRIFT       ! 漂流物の移動計算
         call ftimer('drift','end')
C
         call ftimer('vertex','start')
         CALL VERTEX      ! 漂流物の頂点座標を計算
         call ftimer('vertex','end')
C
         call ftimer('collision','start')
         CALL COLLISION   ! 漂流物の衝突計算
         call ftimer('collision','end')
C
         IF(SINK_MODEL.EQ.0) THEN
         call ftimer('sink','start')
         CALL SINK        ! 漂流物の沈降計算(H21モデル)
         call ftimer('sink','end')
         ELSE
         call ftimer('sink2','start')
         CALL SINK2       ! 漂流物の沈降計算(H22モデル)
         call ftimer('sink2','end')
         ENDIF
C
         if( online ) then
            call ftimer('block','start')
            CALL BLOCK ! 閉塞格子の判定
            call ftimer('block','end')
         endif
C
         call ftimer('output','start')
         CALL OUTPUT(1)   ! ファイル出力
         call ftimer('output','end')
C
         call ftimer('dshift','start')
         CALL DSHIFT      ! 漂流物の基本変数の入れ換え
         call ftimer('dshift','end')
C
         IF (MOD(NS,100).EQ.0) THEN
            WRITE(IFL,100) NS,TIME,DT
         END IF
  100    FORMAT('STP=',I7,' TIM=',1PE12.5,' DT=',E8.2)
         IF( TIME.GE.TEND ) EXIT  ! 終了判定
C
      ENDDO
      call ftimer('timeloop','end')
C----------------------------------------
C     時間積分ループ(終了)
C----------------------------------------
      WRITE(IFL,*) '### END TIME INTEGRATION LOOP '
C
      CALL OUTPUT(2)   ! ファイルのクローズ
C
      call ftimer('total','end')
      call ftimer('total','summerize')
C
      RETURN
      END
