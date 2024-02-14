      SUBROUTINE READ_STR_FILE(ROUGH)
!//////////////////////////////////////////////////
!     strファイルからマニングの粗度を読み込む
!//////////////////////////////////////////////////
      IMPLICIT NONE
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'CONTROL.h'
      INCLUDE 'DOMAIN.h'
!
      REAL(8),INTENT(OUT):: ROUGH(MX,MY)  ! マニングの粗度
      INTEGER:: NX1,NY1,NZ1
      INTEGER:: I,J
!
!
      ROUGH(:,:)=0.D0
!     
      STRFILE=TRIM(CFLNM)//'.str'
      OPEN(INP,FILE=TRIM(STRFILE),STATUS='OLD',FORM='UNFORMATTED',
     $     ERR=99)
!
      WRITE(LP,*) ''
      WRITE(LP,*) 'READING STR FILE ...'
!
      READ(INP) NX1,NY1,NZ1
!
      IF( NX1.NE.NX.OR.NY1.NE.NY.OR.NZ1.NE.NZ ) THEN
         CALL ERRMSG('READ_STR_FILE',62)
         WRITE(*,*) 'MESH SIZE IS NOT MATCH'
         WRITE(*,*) 'NX,NX1 =',NX,NX1
         WRITE(*,*) 'NY,NY1 =',NY,NY1
         WRITE(*,*) 'NZ,NZ1 =',NZ,NZ1
         CALL ABORT1('')         
      ENDIF
!
      READ(INP) ! INDC
      READ(INP) ! GV
      READ(INP) ! GX
      READ(INP) ! GY
      READ(INP) ! GZ
      READ(INP) ! HDEP
      READ(INP) ! HH
      READ(INP) ((ROUGH(I,J),I=2,MXM),J=2,MYM)
      WRITE(LP,*) '   READ ROUGHNESS DATA'
!
      WRITE(LP,*) 'DONE (READING STR FILE)'
      WRITE(LP,*) ''
      CLOSE(INP)
!
!debug      write(10,'(<nx>f6.3)') ((rough(i,j),i=2,mxm),j=mym,2,-1)
      RETURN
!
   99 CONTINUE
      CALL ERRMSG('READ_STR_FILE',61)
      WRITE(*,*) 'CANNOT OPEN ',TRIM(STRFILE)
      CALL ABORT1('')
      END
