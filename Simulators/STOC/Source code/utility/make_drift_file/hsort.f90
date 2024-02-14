SUBROUTINE HSORT(PARAM,ISIZE,NSIZE,IKEY)
!--------------------------------------------------
!     配列をソートする(ヒープソート): STOCのhsort.fの書式だけ変更
!
!     <input>
!       PARAM: ソート対象の配列
!       ISIZE: ソート対象の配列の第1要素のサイズ
!       NSIZE: ソート対象の配列の第2要素のサイズ
!       IKEY: 配列の第1要素のうち、ソートに用いるキー番号
!
!     (output)
!       PARAM: パラメータ
!--------------------------------------------------
  IMPLICIT NONE
  character(64),parameter:: subname='hsort'
!
  REAL(8):: PARAM(ISIZE,NSIZE)
  INTEGER,INTENT(IN):: ISIZE,NSIZE,IKEY
!
  REAL(8),ALLOCATABLE:: PARAM2(:,:),TPTR(:)
  INTEGER,ALLOCATABLE:: IPTR(:)
  REAL(8):: VAL
  INTEGER:: I,J,M,N,ICODE,IERR,ITMP
!
!
  ALLOCATE(PARAM2(ISIZE,NSIZE),TPTR(NSIZE),IPTR(NSIZE),STAT=IERR)
  IF(IERR.NE.0) call sub_err(subname,1)
  PARAM2(:,:)=PARAM(:,:)
!
  DO N=1,NSIZE
     IPTR(N)=N
     TPTR(N)=PARAM(IKEY,N)
  ENDDO
!
!
!DEBUGC ... バブルソート(単純ソート)
!DEBUG      DO N=1,NSIZE-1
!DEBUG         DO M=NSIZE,N+1,-1
!DEBUG            IF( TPTR(M)<TPTR(M-1) ) THEN
!DEBUGC ............ MとM-1の配列の入れ替え
!DEBUG               I=IPTR(M)
!DEBUG               IPTR(M)=IPTR(M-1)
!DEBUG               IPTR(M-1)=I
!DEBUGC
!DEBUG               VAL=TPTR(M)
!DEBUG               TPTR(M)=TPTR(M-1)
!DEBUG               TPTR(M-1)=VAL
!DEBUG            ENDIF
!DEBUG         ENDDO
!DEBUG      ENDDO
!
! ... ヒープソート(データ数が数万くらいまでは上記でも大差ないが...)
!                  データ数が100万で20秒程度、上記だと10分以上
  DO N=NSIZE/2,1,-1
     VAL=TPTR(N)
     ITMP=IPTR(N)
!
     I=N
     J=I*2
!
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
!
     TPTR(I)=VAL
     IPTR(I)=ITMP
  ENDDO
!
  DO N=NSIZE,2,-1
     VAL=TPTR(N)
     ITMP=IPTR(N)
     TPTR(N)=TPTR(1)
     IPTR(N)=IPTR(1)
!
     I=1
     J=I*2
!
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
!
     TPTR(I)=VAL
     IPTR(I)=ITMP
  ENDDO
!
!
! ... ソートした順に配列PARAMにデータを格納する
  DO N=1,NSIZE
     DO M=1,ISIZE
        PARAM(M,N)=PARAM2(M,IPTR(N))
     ENDDO
!         write(41,'(<isize-1>f13.5,f8.0)') (param(m,n),m=1,isize)
  ENDDO
!
  DEALLOCATE(PARAM2,TPTR,IPTR)
!
  RETURN
!
!
contains
  subroutine sub_err(subname,code)
!//////////////////////////////////////////////////
! エラー発生に終了処理を行う
!//////////////////////////////////////////////////
    character(*),intent(in):: subname
    integer,intent(in):: code
!
    write(*,*) ''
    write(*,*) 'Stop at subrouine ',trim(subname)
    write(*,*) ' source file name=hsort.f90'
    write(*,*) '       erorr code=',code
    stop
  end subroutine sub_err
!
END SUBROUTINE HSORT
