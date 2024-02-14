      SUBROUTINE MKIND5(GXBDH,GYBDH,KIBDH,KJBDH,GX0,GY0,INDP,INDU,INDV)
C======================================================================
C     地形変化させないセル境界面の構造物を設定する
C
C     KIBDH,KJBDH：構造物の上端のセルインデックスK(IND[UV]>0の位置)
C                                        (流入出境界は変化させない)
C======================================================================
      use mod_comm,only: comm_model
      IMPLICIT NONE
C
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'FILEI.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'SEDIMENT.h'
      INCLUDE 'mpif.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      REAL(8),INTENT(INOUT)::GXBDH(MX,MY),GYBDH(MX,MY)
      INTEGER,INTENT(INOUT)::KIBDH(MX,MY),KJBDH(MX,MY)
      REAL(8),INTENT(IN)::GX0(MX,MY,MZ),GY0(MX,MY,MZ)
      INTEGER,INTENT(IN)::INDP(MX,MY,MZ)
      INTEGER,INTENT(IN)::INDU(MX,MY,MZ),INDV(MX,MY,MZ)
C
      INTEGER::I,J,K,IX,IFLAG,ICODE,IERR
      INTEGER::IDUM,JDUM,INSIDE1,INSIDE2
C
C
C ... 初期化
      KIBDH(:,:)=0
      KJBDH(:,:)=0
      GXBDH(:,:)=0.0D0
      GYBDH(:,:)=0.0D0
C
      IF( IBEDSTR.EQ.1 ) THEN
         CFLNM(IFLNM-3:IFLNM) = '.ibd'
         write(lp,*) CFLNM
         OPEN(IFLBD,FILE=CFLNM(1:IFLNM),STATUS='OLD',
     $        FORM='FORMATTED',ERR=900)
C
C
C        1行目はスキップ
         READ(IFLBD,*)
         DO
            READ(IFLBD,*,END=250) I,J,IX
            IF(IAUTOD.EQ.0)THEN
               IF( I.LT.1.OR.I.GE.MX.OR.J.LT.1.OR.J.GE.MY ) THEN
                 CALL ERRMSG('MKIND5',7000)
                 WRITE(LP,*) 'ERROR: STRUCTURE-FILE FOR SEDIMENT MODEL:'
                 WRITE(LP,*) '                I,J VALUE IS INVALID'
                 WRITE(LP,*) '                I J FLAG=',I,J,IX
                 CALL ABORT1('')
               ENDIF
C
            ELSE
               IF( MYPROC.EQ.1.AND.
     $            ( I.LT.1.OR.I.GE.MX.OR.J.LT.1.OR.J.GE.MY ) ) THEN
                 CALL ERRMSG('MKIND5',7001)
                 WRITE(LP,*) 'ERROR: STRUCTURE-FILE FOR SEDIMENT MODEL:'
                 WRITE(LP,*) '                I,J VALUE IS INVALID'
                 WRITE(LP,*) '                I J FLAG=',I,J,IX
                 CALL ABORT1('')
               ENDIF
C
               IDUM=I
               JDUM=J
               INSIDE1=0
               INSIDE2=0
               IF(IX.EQ.1.OR.IX.EQ.3)THEN
                 CALL MODIJ(I,IDUM,J,JDUM,2,INSIDE1)
               ENDIF
               IF(IX.EQ.2.OR.IX.EQ.3)THEN
                 CALL MODIJ(I,IDUM,J,JDUM,3,INSIDE2)
               ENDIF
               IF(INSIDE1.EQ.0.AND.INSIDE2.EQ.0) CYCLE
            ENDIF
C     
C     ......... セルの東側に構造物
         IF( IX.EQ.1.OR.IX.EQ.3 ) THEN
            IFLAG=0
            DO K=2,MZM
               IF( INDU(I,J,K).GT.0 ) THEN
                  GXBDH(I,J)=GX0(I,J,K)
                  KIBDH(I,J)=K
                  IFLAG=1
                  EXIT
               ENDIF
            ENDDO
            IF(IFLAG.EQ.0)THEN
               GXBDH(I,J)=0.0D0
               KIBDH(I,J)=MZ
            ENDIF
C     
C     ......... セルの北側に構造物
         ELSEIF( IX.EQ.2.OR.IX.EQ.3 ) THEN
            IFLAG=0
            DO K=2,MZM
               IF( INDV(I,J,K).GT.0 ) THEN
                  GYBDH(I,J)=GY0(I,J,K)
                  KJBDH(I,J)=K
                  IFLAG=1
                  EXIT
               ENDIF
            ENDDO
            IF(IFLAG.EQ.0)THEN
               GYBDH(I,J)=0.0D0
               KJBDH(I,J)=MZ
            ENDIF
C     
         ELSE
         ENDIF
         ENDDO
  250    CONTINUE
C         
         CLOSE(IFLBD)
      ENDIF
C
      RETURN
C
C ... ファイルオープンエラー
  900 CONTINUE
      CALL ERRMSG('MKIND5',7002)
      WRITE(LP,*) 'FILE OPEN ERROR: STRUCTURE FILE FOR SEDIMENT MODEL'
      WRITE(LP,*) 'FILE NUMBER=',IFLBD
      CALL ABORT1('')
      END
