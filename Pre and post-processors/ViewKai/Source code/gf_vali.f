      SUBROUTINE GF_VALI(N,IVAL)

CD=== �T�v ===========================================================

CDT   GF_VALI:NF���̓Ǎ�/����

C==== �錾 ===========================================================

C     -- ���^ --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- ���ϐ� --
      INCLUDE 'GF_CONV.h'

CD    -- ���� --
CD    N          : IN  : I*4 : �����̃����N+1
CD    IVAL(@@@@) : I/O : R*8 : NF��
      DIMENSION IVAL(NUMI0,NUMJ0,NUMK0)

CD    -- �Ǐ��ϐ� --
      CHARACTER*13 FILENM

C==== ���s ===========================================================

CD    -- ���̓t�@�C���̃t�@�C���� --
      IFL=IFLIN+N-1
      WRITE(FILENM,'(I5.5)') N-1
      FILENM='data.grp'//FILENM

CD    -- �f�[�^�͈̔� --
      I1=MAX(IG1,MYIS(N)+(MYGIS(N)-1)-1)
      J1=MAX(JG1,MYJS(N)+(MYGJS(N)-1)-1)
      K1=KG1
      I2=MIN(IG2,MYIE(N)+(MYGIS(N)-1)-1)
      J2=MIN(JG2,MYJE(N)+(MYGJS(N)-1)-1)
      K2=KG2

CD    -- ������ --
      READ(IFL,ERR=9010) (((IVAL(I,J,K),I=I1,I2),J=J1,J2),K=K1,K2)

C     -- ���s���̏I�� --
      GOTO 9999

C==== �G���[���� =====================================================

 9010 CONTINUE
      WRITE(*,*) 'GF_VALI : READ ERROR (',FILENM,').'
      STOP

C==== �I�� ===========================================================

 9999 CONTINUE
      RETURN
      END
