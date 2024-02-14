#include <stdio.h>
#define METIS_NOPTIONS 40

void metis_partition(int *ne, int *nn, int *eptr, int *eind, 
					 int *ncommon, int *nparts, int *epart, int *npart) 
{
	int *vwgt = NULL;
	int *vsize = NULL;
	int options[METIS_NOPTIONS];
	int objval;
	float *tpwgts = NULL;
	int i;

	for (i=0; i<METIS_NOPTIONS; i++)
		options[i] = -1;

	METIS_SetDefaultOptions(options); 

	METIS_PartMeshDual(ne, nn, eptr, eind, vwgt, vsize, ncommon, nparts, tpwgts, 
					   options, &objval, epart, npart);
}

void METIS_PARTITION(int *ne, int *nn, int *eptr, int *eind, 
					 int *ncommon, int *nparts, int *epart, int *npart) 
{
	metis_partition(ne, nn, eptr, eind, ncommon, nparts, epart, npart);
}

void metis_partition_(int *ne, int *nn, int *eptr, int *eind, 
					  int *ncommon, int *nparts, int *epart, int *npart) 
{
	metis_partition(ne, nn, eptr, eind, ncommon, nparts, epart, npart);
}

