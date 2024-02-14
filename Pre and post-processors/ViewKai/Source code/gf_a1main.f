      PROGRAM GF_A1MAIN

CD=== �T�v ===========================================================

CDT   GF_A1MAIN:CADMAS-SURF/3D-MP�̕���p�O���t�B�b�N�f�[�^��ϊ�����

C==== �錾 ===========================================================

C     -- ���^ --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- ���ϐ� --
      INCLUDE 'GF_CONV.h'

CD    -- �Ǐ��ϐ� --
CD    XX(NUMI0)  : R*8 : x�����i�q���W
CD    YY(NUMJ0)  : R*8 : y�����i�q���W
CD    ZZ(NUMK0)  : R*8 : z�����i�q���W
CD    VAL (@@@@) : R*8 : ������
CD    IVAL(@@@@) : I*4 : NF��
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: XX ,YY ,ZZ
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: VAL
      INTEGER         , DIMENSION(:,:,:), ALLOCATABLE :: IVAL

C==== ���s ===========================================================

CD    -- �J�n�R�����g�̏o�� --
      WRITE(*,9510)

CD    -- ���������̓Ǎ��Əo�� --
      CALL GF_A2INIT(1)
      DO 100 N=2,NPROCS
        CALL GF_A2INIT(N)
 100  CONTINUE
      WRITE(*,*) 'NPROCS=',NPROCS
      WRITE(*,*) 'NUMI0 =',NUMI0
      WRITE(*,*) 'NUMJ0 =',NUMJ0
      WRITE(*,*) 'NUMK0 =',NUMK0
      DO 110 N=1,NPROCS
        WRITE(*,*) 'LCL   =',MYGIS(N)+MYMIS(N)-1,
     &                       MYGJS(N)+MYMJS(N)-1,
     &                       MYGIE(N)-MYMIE(N)-1,
     &                       MYGJE(N)-MYMJE(N)-1
 110  CONTINUE

C     -- �z��̃A���P�[�g --
      IERR = 0
      ALLOCATE(XX  (NUMI0)            ,STAT=IERR)
      ALLOCATE(YY  (NUMJ0)            ,STAT=IERR)
      ALLOCATE(ZZ  (NUMK0)            ,STAT=IERR)
      ALLOCATE(VAL (NUMI0,NUMJ0,NUMK0),STAT=IERR)
      ALLOCATE(IVAL(NUMI0,NUMJ0,NUMK0),STAT=IERR)
      IF (IERR.NE.0) GOTO 9010

CD    -- �i�q���W���̓Ǎ��Əo�� --
      DO 200 N=1,NPROCS
        CALL GF_A3GRID(N,XX,YY,ZZ,VAL)
 200  CONTINUE

CD    -- ���Ԗ��̃f�[�^�̓Ǎ��Əo�� --
 300  CONTINUE
        CALL GF_A4TRAN(VAL,IVAL,IE)
        IF (IE.EQ.0) GOTO 300
 390  CONTINUE

CD    -- �I���R�����g���o�� --
      WRITE(*,9990)

C     -- ���s���̏I�� --
      GOTO 9999

C==== �G���[���� =====================================================

 9010 CONTINUE
      WRITE(*,*) 'GF_A1MAIN : CAN NOT ALLOC.'
      STOP

C==== �t�H�[�}�b�g�� =================================================

 9510 FORMAT(/' ','##### CADMAS-SURF/3D-MP GF_CONV START. ######' )
 9990 FORMAT(/' ','##### NORMAL END. ###########################'/)

C==== �I�� ===========================================================

 9999 CONTINUE
      STOP
      END
