      SUBROUTINE GF_VALD(N,VAL,ISW)

CD=== �T�v ===========================================================

CDT   GF_VALD:�����ʂ̓Ǎ�/����

C==== �錾 ===========================================================

C     -- ���^ --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- ���ϐ� --
      INCLUDE 'GF_CONV.h'

CD    -- ���� --
CD    N         : IN  : I*4 : �����̃����N+1
CD    VAL(@@@@) : I/O : R*8 : ������
CD    ISW       : IN  : R*8 : ��`�ʒu
      DIMENSION VAL(NUMI0,NUMJ0,NUMK0)

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
      IF     (ISW.EQ.0) THEN
        READ(IFL,ERR=9010) (((VAL(I,J,K),I=I1,I2),J=J1,J2),K=K1,K2)
      ELSEIF (ISW.EQ.1) THEN
        READ(IFL,ERR=9010) (((VAL(I,J,K),I=I1,I2+1),J=J1,J2),K=K1,K2)
      ELSEIF (ISW.EQ.2) THEN
        READ(IFL,ERR=9010) (((VAL(I,J,K),I=I1,I2),J=J1,J2+1),K=K1,K2)
      ELSE
        READ(IFL,ERR=9010) (((VAL(I,J,K),I=I1,I2),J=J1,J2),K=K1,K2+1)
      ENDIF

C     -- ���s���̏I�� --
      GOTO 9999

C==== �G���[���� =====================================================

 9010 CONTINUE
      WRITE(*,*) 'GF_VALD : READ ERROR (',FILENM,').'
      STOP

C==== �I�� ===========================================================

 9999 CONTINUE
      RETURN
      END
