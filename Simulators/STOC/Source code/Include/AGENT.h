      INTEGER :: NB_SM,IMMTYP,IMAMS,IMAME,IMAMI
      REAL(8):: RMAMS,RMAME,RMAMI,RMAMR
      COMMON /AGENTI/ NB_SM,IMMTYP,IMAMS,IMAME,IMAMI
      COMMON /AGENTR/ RMAMS,RMAME,RMAMI,RMAMR
C
C     NB_SM: マルチエージェントモデルの通信ランク(連成しないとき-1)
C     IMMTYP: マルチエージェントモデル用通信間隔の指定
C             (0のとき通信しない, 1のときステップ指定, 2のとき時刻指定)
C     IMAMS: マルチエージェントモデル用通信の開始ステップ
C     IMAME: マルチエージェントモデル用通信の終了ステップ
C     IMAMI: マルチエージェントモデル用通信のステップ間隔
C     RMAMS: マルチエージェントモデル用通信の開始時刻
C     RMAME: マルチエージェントモデル用通信の終了時刻
C     RMAMI: マルチエージェントモデル用通信の時刻間隔
C     RMAMR: マルチエージェントモデル用通信の次の出力時刻
