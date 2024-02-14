      SUBROUTINE GF_A4TRAN(VAL,IVAL,IE)

CD=== �T�v ===========================================================

CDT   GF_A4TRAN:���Ԗ��̃f�[�^�̓Ǎ��Əo��

C==== �錾 ===========================================================

C     -- ���^ --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- ���ϐ� --
      INCLUDE 'GF_CONV.h'

CD    -- ���� --
CD    VAL (@@@@) : OUT : R*8 : ������
CD    IVAL(@@@@) : OUT : R*8 : NF��
CD    IE         : OUT : R*8 : !=0:�I��
      DIMENSION VAL (NUMI0,NUMJ0,NUMK0)
      DIMENSION IVAL(NUMI0,NUMJ0,NUMK0)

CD    -- �Ǐ��ϐ� --
      CHARACTER*13 FILENM

C==== ���s ===========================================================

CD    -- �v�Z��� --
      DO 100 N=1,NPROCS
        IFL=IFLIN+N-1
        WRITE(FILENM,'(I5.5)') N-1
        FILENM='data.grp'//FILENM
        READ(IFL,END=1000,ERR=9010) I1,D1
        WRITE(*,*) FILENM,I1,D1
 100  CONTINUE
      WRITE(IFLOU,ERR=9020) I1,D1

CD    -- �Z���̏�Ԃ������C���f�b�N�X --
      DO 110 N=1,NPROCS
        CALL GF_VALI(N,IVAL)
 110  CONTINUE
      WRITE(IFLOU,ERR=9020)
     &           (((IVAL(I,J,K),I=IG1,IG2),J=JG1,JG2),K=KG1,KG2)

CD    -- �������o�� --
      DO 120 N=1,NPROCS
        CALL GF_VALD(N,VAL,1)
 120  CONTINUE
      WRITE(IFLOU,ERR=9020)
     &           (((VAL(I,J,K),I=IG1,IG2+1),J=JG1,JG2),K=KG1,KG2)
      DO 130 N=1,NPROCS
        CALL GF_VALD(N,VAL,2)
 130  CONTINUE
      WRITE(IFLOU,ERR=9020)
     &           (((VAL(I,J,K),I=IG1,IG2),J=JG1,JG2+1),K=KG1,KG2)
      DO 140 N=1,NPROCS
        CALL GF_VALD(N,VAL,3)
 140  CONTINUE
      WRITE(IFLOU,ERR=9020)
     &           (((VAL(I,J,K),I=IG1,IG2),J=JG1,JG2),K=KG1,KG2+1)
      DO 150 N=1,NPROCS
        CALL GF_BCD(N)
 150  CONTINUE
C@    XXXXXXXXXXXXXXXX
      DO 160 N=1,NPROCS
        CALL GF_BCD(N)
 160  CONTINUE
C@    XXXXXXXXXXXXXXXX
      DO 170 N=1,NPROCS
        CALL GF_BCD(N)
 170  CONTINUE
C@    XXXXXXXXXXXXXXXX

CD    -- ���� --
      DO 180 N=1,NPROCS
        CALL GF_VALD(N,VAL,0)
 180  CONTINUE
      WRITE(IFLOU,ERR=9020)
     &           (((VAL(I,J,K),I=IG1,IG2),J=JG1,JG2),K=KG1,KG2)
      DO 190 N=1,NPROCS
        CALL GF_BCD(N)
 190  CONTINUE
C@    XXXXXXXXXXXXXXXX

CD    -- VOF�֐�F --
      DO 210 N=1,NPROCS
        CALL GF_VALD(N,VAL,0)
 210  CONTINUE
      WRITE(IFLOU,ERR=9020)
     &           (((VAL(I,J,K),I=IG1,IG2),J=JG1,JG2),K=KG1,KG2)
      DO 220 N=1,NPROCS
        CALL GF_BCD(N)
 220  CONTINUE
C@    XXXXXXXXXXXXXXXX

CD    -- ������ --
      IF (ISWLK.NE.0) THEN
        DO 230 N=1,NPROCS
          CALL GF_VALD(N,VAL,0)
 230    CONTINUE
        WRITE(IFLOU,ERR=9020)
     &           (((VAL(I,J,K),I=IG1,IG2),J=JG1,JG2),K=KG1,KG2)
        DO 240 N=1,NPROCS
          CALL GF_VALD(N,VAL,0)
 240    CONTINUE
        WRITE(IFLOU,ERR=9020)
     &           (((VAL(I,J,K),I=IG1,IG2),J=JG1,JG2),K=KG1,KG2)
        DO 250 N=1,NPROCS
          CALL GF_BCD(N)
 250    CONTINUE
C@      XXXXXXXXXXXXXXXX
        DO 260 N=1,NPROCS
          CALL GF_BCD(N)
 260    CONTINUE
C@      XXXXXXXXXXXXXXXX
      ENDIF

CD    -- ���x --
      IF (ISWLT.NE.0) THEN
        DO 270 N=1,NPROCS
          CALL GF_VALD(N,VAL,0)
 270    CONTINUE
        WRITE(IFLOU,ERR=9020)
     &           (((VAL(I,J,K),I=IG1,IG2),J=JG1,JG2),K=KG1,KG2)
        DO 280 N=1,NPROCS
          CALL GF_BCD(N)
 280    CONTINUE
C@      XXXXXXXXXXXXXXXX
      ENDIF

CD    -- �Z�x --
      DO 300 LC=1,ISWLS
        DO 290 N=1,NPROCS
          CALL GF_VALD(N,VAL,0)
 290    CONTINUE
        WRITE(IFLOU,ERR=9020)
     &           (((VAL(I,J,K),I=IG1,IG2),J=JG1,JG2),K=KG1,KG2)
 300  CONTINUE
      DO 320 LC=1,ISWLS
        DO 310 N=1,NPROCS
          CALL GF_BCD(N)
 310    CONTINUE
C@      XXXXXXXXXXXXXXXX
 320  CONTINUE

CD    -- �󌄗� --
      IF (ISWLG.NE.0) THEN
        DO 330 N=1,NPROCS
          CALL GF_VALD(N,VAL,0)
 330    CONTINUE
        WRITE(IFLOU,ERR=9020)
     &           (((VAL(I,J,K),I=IG1,IG2),J=JG1,JG2),K=KG1,KG2)
      ENDIF

CD    -- �I������ --
      IE=0
      GOTO 1010
 1000 CONTINUE
      IE=1
 1010 CONTINUE

C     -- ���s���̏I�� --
      GOTO 9999

C==== �G���[���� =====================================================

 9010 CONTINUE
      WRITE(*,*) 'GF_A4TRAN : READ ERROR (',FILENM,').'
      STOP

 9020 CONTINUE
      WRITE(*,*) 'GF_A4TRAN : READ ERROR (data.grp).'
      STOP

C==== �I�� ===========================================================

 9999 CONTINUE
      RETURN
      END
