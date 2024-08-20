      SUBROUTINE VF_HSORT(PARAM,ISIZE,NSIZE,IKEY,LP)
C--------------------------------------------------
C     配列をソートする(ヒープソート)
C
C     <input>
C       PARAM: ソート対象の配列
C       ISIZE: ソート対象の配列の第1要素のサイズ
C       NSIZE: ソート対象の配列の第2要素のサイズ
C       IKEY: 配列の第1要素のうち、ソートに用いるキー番号
C       LP : 標準出力ファイル番号
C
C     (output)
C       PARAM: 断層パラメータ
C--------------------------------------------------
      IMPLICIT NONE
C
      REAL(8):: PARAM(ISIZE,NSIZE)
      INTEGER,INTENT(IN):: ISIZE,NSIZE,IKEY,LP
C
      REAL(8),ALLOCATABLE:: PARAM2(:,:),TPTR(:)
      INTEGER,ALLOCATABLE:: IPTR(:)
      REAL(8):: VAL
      INTEGER:: I,J,M,N,ICODE,IERR,ITMP
C
C
      ALLOCATE(PARAM2(ISIZE,NSIZE),TPTR(NSIZE),IPTR(NSIZE),STAT=IERR)
      IF(IERR.NE.0) THEN
         CALL VF_A2ERR('VF_HSORT','CANNOT ALLOCATE PARAM2.')
      ENDIF
      PARAM2(:,:)=PARAM(:,:)
C
      DO N=1,NSIZE
         IPTR(N)=N
         TPTR(N)=PARAM(IKEY,N)
      ENDDO
C
C
CDEBUGC ... バブルソート(単純ソート)
CDEBUG      DO N=1,NSIZE-1
CDEBUG         DO M=NSIZE,N+1,-1
CDEBUG            IF( TPTR(M)<TPTR(M-1) ) THEN
CDEBUGC ............ MとM-1の配列の入れ替え
CDEBUG               I=IPTR(M)
CDEBUG               IPTR(M)=IPTR(M-1)
CDEBUG               IPTR(M-1)=I
CDEBUGC
CDEBUG               VAL=TPTR(M)
CDEBUG               TPTR(M)=TPTR(M-1)
CDEBUG               TPTR(M-1)=VAL
CDEBUG            ENDIF
CDEBUG         ENDDO
CDEBUG      ENDDO
C
C ... ヒープソート(データ数が数万くらいまでは上記でも大差ないが...)
C                  データ数が100万で20秒程度、上記だと10分以上
      DO N=NSIZE/2,1,-1
         VAL=TPTR(N)
         ITMP=IPTR(N)
C
         I=N
         J=I*2
C
         DO
            IF( J>NSIZE ) EXIT
            IF( J<NSIZE ) THEN
               IF( TPTR(J)<TPTR(J+1) ) J=J+1
            ENDIF
            IF( TPTR(J)>VAL ) THEN
               TPTR(I)=TPTR(J)
               IPTR(I)=IPTR(J)
               I=J
               J=I*2
            ELSE
               J=NSIZE+1
            ENDIF
         ENDDO
C
         TPTR(I)=VAL
         IPTR(I)=ITMP
      ENDDO
C
      DO N=NSIZE,2,-1
         VAL=TPTR(N)
         ITMP=IPTR(N)
         TPTR(N)=TPTR(1)
         IPTR(N)=IPTR(1)
C
         I=1
         J=I*2
C
         DO
            IF( J>N-1 ) EXIT
            IF( J<N-1 ) THEN
               IF( TPTR(J)<TPTR(J+1) ) J=J+1
            ENDIF
            IF( TPTR(J)>VAL ) THEN
               TPTR(I)=TPTR(J)
               IPTR(I)=IPTR(J)
               I=J
               J=I*2
            ELSE
               J=N
            ENDIF
         ENDDO
C
         TPTR(I)=VAL
         IPTR(I)=ITMP
      ENDDO
C
C
C ... ソートした順に配列PARAMにデータを格納する
      DO N=1,NSIZE
         DO M=1,ISIZE
            PARAM(M,N)=PARAM2(M,IPTR(N))
         ENDDO
c         write(41,'(<isize-1>f13.5,f8.0)') (param(m,n),m=1,isize)
      ENDDO
C
      DEALLOCATE(PARAM2,TPTR,IPTR)
C
      RETURN
      END
