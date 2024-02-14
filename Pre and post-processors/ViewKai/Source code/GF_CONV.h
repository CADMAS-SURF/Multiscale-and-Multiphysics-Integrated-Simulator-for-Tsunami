      PARAMETER (IFLOU=10,IFLIN=21,MPROCS= 900)
      COMMON /GF_CONV/ NPROCS,NUMI0,NUMJ0,NUMK0,
     &                 IG1,JG1,KG1,IG2,JG2,KG2,
     &                 ISWLN,ISWLV,ISWLP,ISWLF,ISWLK,
     &                 ISWLT,ISWLS,ISWLG,ISWL1,ISWL2,
     &                 NBX  (MPROCS),NBY  (MPROCS),NBZ  (MPROCS),
     &                 NUMI (MPROCS),NUMJ (MPROCS),
     &                 MYIS (MPROCS),MYIE (MPROCS),
     &                 MYJS (MPROCS),MYJE (MPROCS),
     &                 MYMIS(MPROCS),MYMIE(MPROCS),
     &                 MYMJS(MPROCS),MYMJE(MPROCS),
     &                 MYGIS(MPROCS),MYGIE(MPROCS),
     &                 MYGJS(MPROCS),MYGJE(MPROCS)

CD=== �T�v ===========================================================

CDT   GF_CONV.h:CADMAS-SURF/3D�̕���p�O���t�B�b�N�f�[�^�ϊ��p

C==== ���e ===========================================================

CD    IFLOU         : PRM : I*4 : �o�̓t�@�C���ԍ�
CD    IFLIN         : PRM : I*4 : ���̓t�@�C���ԍ��̊J�n�ԍ�
CD    MPROCS        : PRM : I*4 : �ő�v���Z�X��
CD @  NPROCS        : CNS : I*4 : �v���Z�X��
CD @  NUMI0         : CNS : I*4 : �S�̂̊i�q��
CD @  NUMJ0         : CNS : I*4 : ..
CD @  NUMK0         : CNS : I*4 : ..
CD @  IG1           : CNS : I*4 : �S�̂̐}���t�@�C���̏o�͔͈�
CD @  JG1           : CNS : I*4 : ..
CD @  KG1           : CNS : I*4 : ..
CD @  IG2           : CNS : I*4 : ..
CD @  JG2           : CNS : I*4 : ..
CD @  KG2           : CNS : I*4 : ..
CD @  ISWLN         : CNS : I*4 : �o�̓t���O
CD @  ISWLV         : CNS : I*4 : ..
CD @  ISWLP         : CNS : I*4 : ..
CD @  ISWLF         : CNS : I*4 : ..
CD @  ISWLK         : CNS : I*4 : ..
CD @  ISWLT         : CNS : I*4 : ..
CD @  ISWLS         : CNS : I*4 : ..
CD @  ISWLG         : CNS : I*4 : ..
CD @  ISWL1         : CNS : I*4 : ..
CD @  ISWL2         : CNS : I*4 : ..
CD @  NBX  (MPROCS) : CNS : I*4 : ���E�l�̏o�͐�
CD @  NBY  (MPROCS) : CNS : I*4 : ..
CD @  NBZ  (MPROCS) : CNS : I*4 : ..
CD @  NUMI (MPROCS) : CNS : I*4 : x�����i�q��
CD @  NUMJ (MPROCS) : CNS : I*4 : y�����i�q��
CD @  MYIS (MPROCS) : CNS : I*4 : x�����Z���ԍ�(�J�n,���z�܂܂�,�Ǐ�)
CD @  MYIE (MPROCS) : CNS : I*4 : x�����Z���ԍ�(�I��,���z�܂܂�,�Ǐ�)
CD @  MYJS (MPROCS) : CNS : I*4 : y�����Z���ԍ�(�J�n,���z�܂܂�,�Ǐ�)
CD @  MYJE (MPROCS) : CNS : I*4 : y�����Z���ԍ�(�I��,���z�܂܂�,�Ǐ�)
CD @  MYMIS(MPROCS) : CNS : I*4 : x�����Z���ԍ�(�J�n,���z�̌���)
CD @  MYMIE(MPROCS) : CNS : I*4 : x�����Z���ԍ�(�I��,���z�̌���)
CD @  MYMJS(MPROCS) : CNS : I*4 : y�����Z���ԍ�(�J�n,���z�̌���)
CD @  MYMJE(MPROCS) : CNS : I*4 : y�����Z���ԍ�(�I��,���z�̌���)
CD @  MYGIS(MPROCS) : CNS : I*4 : x�����Z���ԍ�(�J�n,���z�܂�,���)
CD @  MYGIE(MPROCS) : CNS : I*4 : x�����Z���ԍ�(�I��,���z�܂�,���)
CD @  MYGJS(MPROCS) : CNS : I*4 : y�����Z���ԍ�(�J�n,���z�܂�,���)
CD @  MYGJE(MPROCS) : CNS : I*4 : y�����Z���ԍ�(�I��,���z�܂�,���)
