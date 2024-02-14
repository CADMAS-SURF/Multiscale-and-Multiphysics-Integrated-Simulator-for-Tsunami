      SUBROUTINE SETLVL
C----------------------------------------
C     漂流物位置の水位の設定及び地面への乗り上げ判定
C----------------------------------------
      USE M_GRID
      USE M_GEOM
      USE M_DRIFT
      USE M_FLUID
      USE M_TIME
      USE M_MODEL,ONLY:SINK_MODEL
      USE M_OUTPUT,ONLY:IFL
C
      IMPLICIT NONE
      INTEGER,PARAMETER::MXLIST=2000
C
      INTEGER N,M,I,J,IERR,NA
      INTEGER NLIST,LIST(2,MXLIST)
      REAL(8)::HF0,HT0
c
      integer::ii
      data ii /0/
C
C----------------------------------------
C     (1) 重心位置における水位と標高を設定
C----------------------------------------
      DO N=1,ND
         IF( LD(N).EQ.-2 ) CYCLE
C
C
         CALL INTERP_H(HF0,HT0,XD(N),YD(N),INAR(N))
         HZDO(N) = HZD(N)
         HAZO(N) = HAZ(N)
C
         IF( SINK_MODEL.EQ.1 ) THEN           ! H22モデル(水位変化に連動させない)
            if( ii.eq.0.and.irest.eq.0 ) then ! 初期の鉛直方向の位置を釣り合いの位置に設定
               HZD(N) = MAX(HF0-BD0(N),HT0)
               HAD(N) = MAX(HF0-AD0(N),HT0+BD0(N)-AD0(N))
            ELSE
               AD(N) = AD(N)+HF0-HFD(N)
               BD(N) = BD(N)+HF0-HFD(N)
            ENDIF
         ELSE                                 ! H21モデル
            IF( (IFLAG_S(N).EQ.2.OR.IFLAG_S(N).EQ.3) .OR.
     $          (IFLAG_D(N).EQ.2.OR.IFLAG_D(N).EQ.3) ) THEN ! 冠水以降は水位変化に連動させない
               HZD(N) = MAX(HZD(N)-WSD(N)*DT,HT0)
               HAD(N) = MAX(HAD(N)-WSD(N)*DT,HT0+BD0(N)-AD0(N))
               IF( HZD(N)+AZ1(N)+BD0(N)-AD0(N).GT.HF0 )
     $                       HZD(N)=MAX(HF0-(AZ1(N)+BD0(N)-AD0(N)),HT0)
               IF( HAD(N)+AZ1(N).GT.HF0 ) HAD(N)=MAX(HF0-AZ1(N),
     $                                               HT0+BD0(N)-AD0(N))
            ELSE
               HZD(N) = MAX(HF0-BD(N),HT0)
               HAD(N) = MAX(HF0-AD(N),HT0+BD0(N)-AD0(N))
            ENDIF
         ENDIF
         HAZ(N) = MIN(HAD(N)+AZ1(N),HF0)
C
         HFD(N) = HF0
         HTD(N) = HT0
C
         if( ii.eq.0.and.irest.eq.0 ) then
            xd_init(n) = xd(n)
            yd_init(n) = yd(n)
            zd_init(n) = hzd(n) + bd(n)
         endif
         CALL SETKD(HFD(N),KFD(N),INAR(N))   ! HFDの高さを含む流体セルのインデックスKを定める
         CALL SETKD(HAD(N),KAD(N),INAR(N))   ! HADの高さを含む流体セルのインデックスKを定める
         CALL SETKD(HAZ(N),KAZ(N),INAR(N))   ! HAZの高さを含む流体セルのインデックスKを定める
         CALL SETKD(HZD(N),KZD(N),INAR(N))   ! HZDの高さを含む流体セルのインデックスKを定める
C
         IF( LD(N).EQ.0 ) THEN
            IF( HFD(N).LT.HMIN_INI(N).OR.
     $          HFD(N).GT.HMAX_INI(N) ) THEN
               LD(N) = 1
               write(IFL,*) 'SHIP BEGIN TO MOVING DUE TO WATER LEVEL.'
               write(IFL,*) '       ND,LEVEL=',N,HFD(N)
            ENDIF
            IF( TIME.GT.TDST_INI(N) ) THEN
               LD(N) = 1
               write(IFL,*) 'SHIP BEGIN TO MOVING DUE TO TIME.'
               write(IFL,*) '       ND,LEVEL=',N,HFD(N)
            ENDIF
cadd 3line 20130423
            IF( FMAX_INI(N).EQ.-1.0D50) THEN
               LD(N) = 1
            ENDIF
         ENDIF
      ENDDO
C
      ii = 1
C
C----------------------------------------
C     (2) 地面への乗り上げ判定
C----------------------------------------
      DO N=1,ND
         IF( LD(N).EQ.0.OR.LD(N).EQ.-2 ) CYCLE
C
         IF( HZD(N).EQ.HTD(N) ) THEN    ! 省力化のため重心位置での事前チェック
cs 2014/03/25 honda
c            UD1(N) = 0.0D0
c            VD1(N) = 0.0D0
c            OD1(N) = 0.0D0
ce 2014/03/25 honda
            IF( LD(N).NE.-1 ) THEN
c               WRITE(*,*) 'CENTER OF SHIP BOTTOM IS TOUCHED AT GROUND.'
c               WRITE(*,*) '      SHIP NO.=',N
c               WRITE(*,*) '      HEIGHT(WATER,GROUND) =',HFD(N),HTD(N)
            ENDIF
            LD(N) = -1
C<<<<< (START) STOC-BLC VERSION  <<<<<<<
            LBLC(N) = 1
C<<<<<  (END)  STOC-BLC VERSION  <<<<<<<
            CYCLE
         ENDIF
C
C ...... 漂流物を含むセルのリストを作成
C
         CALL MKDLIST(NLIST,LIST,
     $                XCOLLIS(1,N),YCOLLIS(1,N),
     $                XCOLLIS(2,N),YCOLLIS(2,N),0.5D0*BB(N),
     $                MXLIST,IERR,INAR(N))
         IF( IERR.EQ.-2 .AND. INAR(N).EQ.1 ) THEN
            LD(N) = -2
c            WRITE(*,*) '   SHIP NO.=',N
            CYCLE
         ENDIF
         LD(N) = 1
C<<<<< (START) STOC-BLC VERSION  <<<<<<<
         LBLC(N) = 0
C<<<<<  (END)  STOC-BLC VERSION  <<<<<<<
C
C ...... どこか1セルでも漂流物底面より高ければ乗り上げたものとする
         DO M=1,NLIST
            I = LIST(1,M)
            J = LIST(2,M)
            NA = INAR(N)
            IF( HZD(N).LE.HT(NA,I,J) ) THEN
cs 2014/03/25 honda
c               UD1(N) = 0.0D0
c               VD1(N) = 0.0D0
c               OD1(N) = 0.0D0
ce 2014/03/25 honda
               IF( LD(N).NE.-1 ) THEN
c                WRITE(*,*)'A PART OF SHIP BOTTOM IS TOUCHED AT GROUND.'
c                WRITE(*,*)'   SHIP NO.=',N
c                WRITE(*,*)'   HEIGHT(BOTTOM,GROUND) =',HZD(N),HT(NA,I,J)
               ENDIF
               LD(N) = -1
C<<<<< (START) STOC-BLC VERSION  <<<<<<<
               LBLC(N) = 1
C<<<<<  (END)  STOC-BLC VERSION  <<<<<<<
               EXIT
            ENDIF
         ENDDO
      ENDDO
C
      RETURN
      END
