      SUBROUTINE MKIND2(INDP,INDU,INDV,INDW,INDX,INDY,INDZ,
     $                  LLWALL,LLWALP,IFLAG)
C======================================================================
C     壁面境界リスト LLWALL と 板状境界リスト LLWALP を作成する
C
C     (1) INDX〜INDZ に 壁面境界条件番号を埋め込む
C     (2) 壁面境界リスト LLWALL と 板状境界リスト LLWALP を作成する
C======================================================================
      IMPLICIT NONE
C
      INCLUDE 'AREA.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
C
      INTEGER,INTENT(INOUT)::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDX(MX,MY,MZ),INDY(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::INDZ(MX,MY,MZ)
      INTEGER,INTENT(INOUT)::LLWALL(8,MLWALL),LLWALP(8,MLWALP)
C
      INTEGER::I,IDIR,IE,IS,J,JE,JS,K,KE,KS,M,M1,M2,N,IFLAG
C
C
C ... 作業用配列を0で初期化
      CALL ZERCLI(INDX,MXYZ,0)
      CALL ZERCLI(INDY,MXYZ,0)
      CALL ZERCLI(INDZ,MXYZ,0)
C
C
C----------------------------------------------------------------------
C     (1) INDX〜INDZ に 壁面境界条件番号を埋め込む
C----------------------------------------------------------------------
C
      DO 100 N=1,NWALL
         M  = MWALL(1,N)
         IS = IAREA(1,M)
         IE = IAREA(2,M)
         JS = IAREA(3,M)
         JE = IAREA(4,M)
         KS = IAREA(5,M)
         KE = IAREA(6,M)
         IDIR = IAREA(7,M)
C
C ...... 法線方向がX方向の面
         IF( IDIR.EQ.1 ) THEN
            I = IS
            DO 110 K=KS,KE
            DO 110 J=JS,JE
               INDX(I,J,K) = N
               IF( INDU(I,J,K).NE.-2 .AND. INDU(I,J,K).NE.-3
     $             .AND. IFLAG.EQ.0 ) THEN
                  CALL ERRMSG('MKIND2',6980)
                  WRITE(LP,*) 'WALL BOUNDARY CONDITION IS SET ON ',
     $                        'ILLEGAL PLACE'
                  WRITE(LP,*) 'I,J,K=',I,J,K
                  WRITE(LP,*) 'AREA NUM.=',M
                  WRITE(LP,*) 'IS,IE=',IS,IE
                  WRITE(LP,*) 'JS,JE=',JS,JE
                  WRITE(LP,*) 'KS,KE=',KS,KE
                  WRITE(LP,*) 'DIR  =',IDIR,'(=1:X,=2:Y,=3:Z)'
                  CALL ABORT1('')
               END IF
  110       CONTINUE
C
C ...... 法線方向がY方向の面
         ELSE IF( IDIR.EQ.2 ) THEN
            J = JS
            DO 120 K=KS,KE
            DO 120 I=IS,IE
               INDY(I,J,K) = N
               IF( INDV(I,J,K).NE.-2 .AND. INDV(I,J,K).NE.-3
     $             .AND. IFLAG.EQ.0 ) THEN
                  CALL ERRMSG('MKIND2',6981)
                  WRITE(LP,*) 'WALL BOUNDARY CONDITION IS SET ON ',
     $                        'ILLEGAL PLACE'
                  WRITE(LP,*) 'I,J,K=',I,J,K
                  WRITE(LP,*) 'AREA NUM.=',M
                  WRITE(LP,*) 'IS,IE=',IS,IE
                  WRITE(LP,*) 'JS,JE=',JS,JE
                  WRITE(LP,*) 'KS,KE=',KS,KE
                  WRITE(LP,*) 'DIR  =',IDIR,'(=1:X,=2:Y,=3:Z)'
                  CALL ABORT1('')
               END IF
  120       CONTINUE
C
C ...... 法線方向がZ方向の面
         ELSE IF( IDIR.EQ.3 ) THEN
            K = KS
            DO 130 J=JS,JE
            DO 130 I=IS,IE
               INDZ(I,J,K) = N
               IF( INDW(I,J,K).NE.-2 .AND. INDW(I,J,K).NE.-3
     $             .AND. IFLAG.EQ.0 ) THEN
                  CALL ERRMSG('MKIND2',6982)
                  WRITE(LP,*) 'WALL BOUNDARY CONDITION IS SET ON ',
     $                        'ILLEGAL PLACE'
                  WRITE(LP,*) 'I,J,K=',I,J,K
                  WRITE(LP,*) 'AREA NUM.=',M
                  WRITE(LP,*) 'IS,IE=',IS,IE
                  WRITE(LP,*) 'JS,JE=',JS,JE
                  WRITE(LP,*) 'KS,KE=',KS,KE
                  WRITE(LP,*) 'DIR  =',IDIR,'(=1:X,=2:Y,=3:Z)'
                  CALL ABORT1('')
               END IF
  130       CONTINUE
         END IF
  100 CONTINUE
C
C
C----------------------------------------------------------------------
C     (2) 壁面境界リスト LLWALL と 板状境界リスト LLWALP を作成する
C----------------------------------------------------------------------
      M1 = 0
      M2 = 0
C
      DO 200 K=2,MZM
      DO 200 J=2,MYM
      DO 200 I=1,MXM
         IF( INDU(I,J,K).EQ.-2 ) THEN
            M1 = M1 + 1
            N = INDX(I,J,K)
            IF( INDP(I,J,K).EQ.0 ) THEN
               IDIR = 1
            ELSE
               IDIR = 0
            END IF
            LLWALL(1,M1) = I
            LLWALL(2,M1) = J
            LLWALL(3,M1) = K
            LLWALL(4,M1) = IDIR
            LLWALL(5,M1) = N
            IF( N.EQ.0 ) THEN
               LLWALL(6,M1) = MDWALV
               IF( MDWALV.EQ.3 ) LLWALL(6,M1)=0
               LLWALL(7,M1) = MDWALT
               LLWALL(8,M1) = MDWALT
            ELSE
               LLWALL(6,M1) = MWALL(2,N)
               LLWALL(7,M1) = MWALL(3,N)
               LLWALL(8,M1) = MWALL(3,N)
            END IF
            IF( LLWALL(6,M1).EQ.2 ) ILGLWL = 1
C
         ELSE IF( INDU(I,J,K).EQ.-3 ) THEN
            M2 = M2 + 1
            N = INDX(I,J,K)
            LLWALP(1,M2) = I
            LLWALP(2,M2) = J
            LLWALP(3,M2) = K
            LLWALP(4,M2) = 1
            LLWALP(5,M2) = N
            IF( N.EQ.0 ) THEN
               LLWALP(6,M2) = MDWALV
               IF( MDWALV.EQ.3 ) LLWALP(6,M2)=0
               LLWALP(7,M2) = MDWALT
               LLWALP(8,M2) = MDWALT
            ELSE
               LLWALP(6,M2) = MWALL(2,N)
               LLWALP(7,M2) = MWALL(3,N)
               LLWALP(8,M2) = MWALL(3,N)
            END IF
            IF( LLWALP(6,M2).EQ.2 ) ILGLWP = 1
         END IF
  200 CONTINUE
C
      DO 210 K=2,MZM
      DO 210 J=1,MYM
      DO 210 I=2,MXM
         IF( INDV(I,J,K).EQ.-2 ) THEN
            M1 = M1 + 1
            N = INDY(I,J,K)
            IF( INDP(I,J,K).EQ.0 ) THEN
               IDIR = 3
            ELSE
               IDIR = 2
            END IF
            LLWALL(1,M1) = I
            LLWALL(2,M1) = J
            LLWALL(3,M1) = K
            LLWALL(4,M1) = IDIR
            LLWALL(5,M1) = N
            IF( N.EQ.0 ) THEN
               LLWALL(6,M1) = MDWALV
               IF( MDWALV.EQ.3 ) LLWALL(6,M1)=0
               LLWALL(7,M1) = MDWALT
               LLWALL(8,M1) = MDWALT
            ELSE
               LLWALL(6,M1) = MWALL(2,N)
               LLWALL(7,M1) = MWALL(3,N)
               LLWALL(8,M1) = MWALL(3,N)
            END IF
            IF( LLWALL(6,M1).EQ.2 ) ILGLWL = 1
C
         ELSE IF( INDV(I,J,K).EQ.-3 ) THEN
            M2 = M2 + 1
            N = INDY(I,J,K)
            LLWALP(1,M2) = I
            LLWALP(2,M2) = J
            LLWALP(3,M2) = K
            LLWALP(4,M2) = 2
            LLWALP(5,M2) = N
            IF( N.EQ.0 ) THEN
               LLWALP(6,M2) = MDWALV
               IF( MDWALV.EQ.3 ) LLWALP(6,M2)=0
               LLWALP(7,M2) = MDWALT
               LLWALP(8,M2) = MDWALT
            ELSE
               LLWALP(6,M2) = MWALL(2,N)
               LLWALP(7,M2) = MWALL(3,N)
               LLWALP(8,M2) = MWALL(3,N)
            END IF
            IF( LLWALP(6,M2).EQ.2 ) ILGLWP = 1
         END IF
  210 CONTINUE
C
      DO 220 K=1,MZM
      DO 220 J=2,MYM
      DO 220 I=2,MXM
         IF( INDW(I,J,K).EQ.-2 ) THEN
            M1 = M1 + 1
            N = INDZ(I,J,K)
            IF( INDP(I,J,K).EQ.0 ) THEN
               IDIR = 5
            ELSE
               IDIR = 4
            END IF
            LLWALL(1,M1) = I
            LLWALL(2,M1) = J
            LLWALL(3,M1) = K
            LLWALL(4,M1) = IDIR
            LLWALL(5,M1) = N
            IF( N.EQ.0 ) THEN
               LLWALL(6,M1) = MDWALV
               IF( MDWALV.EQ.3 ) LLWALL(6,M1)=1
               LLWALL(7,M1) = MDWALT
               LLWALL(8,M1) = MDWALT
            ELSE
               LLWALL(6,M1) = MWALL(2,N)
               LLWALL(7,M1) = MWALL(3,N)
               LLWALL(8,M1) = MWALL(3,N)
            END IF
            IF( LLWALL(6,M1).EQ.2 ) ILGLWL = 1
C
         ELSE IF( INDW(I,J,K).EQ.-3 ) THEN
            M2 = M2 + 1
            N = INDZ(I,J,K)
            LLWALP(1,M2) = I
            LLWALP(2,M2) = J
            LLWALP(3,M2) = K
            LLWALP(4,M2) = 3
            LLWALP(5,M2) = N
            IF( N.EQ.0 ) THEN
               LLWALP(6,M2) = MDWALV
               IF( MDWALV.EQ.3 ) LLWALP(6,M2)=1
               LLWALP(7,M2) = MDWALT
               LLWALP(8,M2) = MDWALT
            ELSE
               LLWALP(6,M2) = MWALL(2,N)
               LLWALP(7,M2) = MWALL(3,N)
               LLWALP(8,M2) = MWALL(3,N)
            END IF
            IF( LLWALP(6,M2).EQ.2 ) ILGLWP = 1
         END IF
  220 CONTINUE
C
C ... エラーチェック
      IF( MLWALL1.NE.M1 ) THEN
         CALL ERRMSG('MKIND2',6983)
         WRITE(LP,*) 'UNEXPECTED ERROR'
         WRITE(LP,*) 'MLWALL1=',MLWALL1
         WRITE(LP,*) 'M1     =',M1
         CALL ABORT1('')
      END IF
      IF( MLWALP.NE.M2 ) THEN
         CALL ERRMSG('MKIND2',6984)
         WRITE(LP,*) 'UNEXPECTED ERROR'
         WRITE(LP,*) 'MLWALP =',MLWALP
         WRITE(LP,*) 'M2     =',M2
         CALL ABORT1('')
      END IF
C
      RETURN
      END
