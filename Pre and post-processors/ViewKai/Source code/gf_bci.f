      SUBROUTINE GF_BCI(N)

CD=== �T�v ===========================================================

CDT   GF_BCI:���E�����̓Ǎ�/����

C==== �錾 ===========================================================

C     -- ���^ --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- ���ϐ� --
      INCLUDE 'GF_CONV.h'

CD    -- ���� --
CD    N : IN  : I*4 : �����̃����N+1

CD    -- �Ǐ��ϐ� --
      CHARACTER*13 FILENM

C==== ���s ===========================================================

CD    -- ���̓t�@�C���̃t�@�C���� --
      IFL=IFLIN+N-1
      WRITE(FILENM,'(I5.5)') N-1
      FILENM='data.grp'//FILENM

CD    -- �f�[�^�͈̔� --
      NB=NBX(N)+NBY(N)+NBZ(N)

CD    -- ���E�̃C���f�b�N�X --
      IF (NB.GT.0) THEN
        READ(IFL,ERR=9010) (I1,I=1,NB)
      ENDIF

C     -- ���s���̏I�� --
      GOTO 9999

C==== �G���[���� =====================================================

 9010 CONTINUE
      WRITE(*,*) 'GF_BCI : READ ERROR (',FILENM,').'
      STOP

C==== �I�� ===========================================================

 9999 CONTINUE
      RETURN
      END
