      SUBROUTINE INPUT(IFLAG,IRTRN)
C======================================================================
C     解析条件データファイルからデータブロック名を読み込み、
C     ブロック毎の入力データ処理ルーチンを呼び出す
C
C     IFLAG = 0 : 通常の入力処理
C     IFLAG= -1 : data.in読込み時にGRIDブロックの読込みのみを行う
C======================================================================
      IMPLICIT NONE
C     
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
C     
      INTEGER,INTENT(IN)::IFLAG
      INTEGER,INTENT(OUT)::IRTRN
      INTEGER::IE,IEND,IS,N
C
C
      IF( IFLAG.EQ.0 )
     $WRITE(LP,*) '    READ INPUT DATA(START)'
C
      DO 100 N=1,100000
         CALL GET1(IS,IE,IEND)
         IF( IEND.EQ.1 ) GO TO 200
C
         IF( IFLAG.EQ.-1.AND.CLINE(IS:IE).NE.'%GRID' ) GOTO 100
C
         WRITE(LP,*) '       READING BLOCK ',CLINE(IS:IE),' ...'
C
         IF( CLINE(IS:IE) .EQ. '%GRID'     ) THEN
            CALL INGRID(IFLAG,IRTRN)
C
         ELSE IF( CLINE(IS:IE) .EQ. '%OBSTACLE' ) THEN
            CALL INOBST
C
         ELSE IF( CLINE(IS:IE) .EQ. '%TIME'     ) THEN
            CALL INTIME
C
         ELSE IF( CLINE(IS:IE) .EQ. '%MODEL'    ) THEN
            CALL INMODL(IRTRN)
C
         ELSE IF( CLINE(IS:IE) .EQ. '%PROPERTY' ) THEN
            CALL INPROP
C
         ELSE IF( CLINE(IS:IE) .EQ. '%BOUNDARY' ) THEN
            CALL INBOUN
C
         ELSE IF( CLINE(IS:IE) .EQ. '%INITIAL'  ) THEN
            CALL ININIT
C
         ELSE IF( CLINE(IS:IE) .EQ. '%MATRIX'   ) THEN
            CALL INMTRX
C
         ELSE IF( CLINE(IS:IE) .EQ. '%OUTPUT'   ) THEN
            CALL INOUTP
C
         ELSE IF( CLINE(IS:IE) .EQ. '%CASE'     ) THEN
            CALL INCASE
C
         ELSE IF( CLINE(IS:IE) .EQ. '%TYPHOON'     ) THEN
            CALL INTYPH
C
         ELSE IF( CLINE(IS:IE) .EQ. '%SEDIMENT') THEN
            CALL INSEDI
C
         ELSE IF( CLINE(IS:IE) .EQ. '%AIR') THEN
            CALL INAIR(IRTRN)
C
         ELSE
            CALL ERRMSG('INPUT',6400)
            WRITE(LP,*) 'INPUT DATA BLOCK NAME IS INCORRECT'
            WRITE(LP,*) 'LINE=',CLINE
            WRITE(LP,*) 'BLOCK=',CLINE(IS:IE)
            CALL ABORT1('')
         END IF
  100 CONTINUE
  200 CONTINUE
C
      IF( IFLAG.EQ.0 )
     $WRITE(LP,*) '    READ INPUT DATA(END)'
C
      RETURN
      END
