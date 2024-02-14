#include <stdio.h>
#include <sys/stat.h>

void mk_dir(char *str, int len)
{
	str[len] = 0;

    mkdir(str,0777);

/*	printf("%s\n", str);
	printf("%d\n", len); */
}

void MK_DIR(char *str, int len)
{
	mk_dir(str, len);
}

void mk_dir_(char *str, int len)
{
	mk_dir(str, len);
}