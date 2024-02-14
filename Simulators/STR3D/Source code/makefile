export FCOMP=mpiifort
export FOPT=-no-heap-arrays
export CCOMP=mpicc
export COPT=
export FNAME=LOWER_CASE_
export ARC=ar r

METIS_DIR=../lib
BLAS_DIR=../BLAS-3.7.0
SCALAPACK_DIR=../scalapack-2.0.2
MUMPS_DIR=../MUMPS_4.10.0

LIBMETIS=-L$(METIS_DIR) -lmetis

BLAS=blas_l
LIBBLAS=-L$(BLAS_DIR) -lblas

SCALAPACK=scalapack_l
LIBSCALAPACK=-L$(SCALAPACK_DIR) -lscalapack

MUMPS=mumps_l
LIBMUMPS=-L$(MUMPS_DIR)/lib  -ldmumps -lmumps_common -lpord

ALL_L=$(BLAS) $(SCALAPACK) $(MUMPS)
LIBS=$(LIBMUMPS) $(LIBSCALAPACK) $(LIBBLAS) $(LIBMETIS)

all : $(ALL_L) module_o contact_o femap_o geo_o glb_comm_o input_o input_Count_data_o input_Read_data_o \
      input_Set_table_o main_o mpi_comm_o partition_o seq_o src_o util_o
	$(FCOMP) -o str.exe module/*.o contact/*.o femap/*.o geo/*.o glb_comm/*.o input/*.o input/*/*.o \
                 main/*.o mpi_comm/*.o partition/*.o seq/*.o src/*.o util/*.o $(LIBS)

blas_l :
	cd $(BLAS_DIR) ; make double

scalapack_l :
	cd $(SCALAPACK_DIR) ; make lib

mumps_l :
	cd $(MUMPS_DIR) ; make d

module_o :
	cd module ; make all

contact_o :
	cd contact ; make all

femap_o :
	cd femap ; make all

geo_o :
	cd geo ; make all

glb_comm_o :
	cd glb_comm ; make all

input_o :
	cd input ; make all

input_Count_data_o :
	cd input/Count_data ; make all

input_Read_data_o :
	cd input/Read_data ; make all

input_Set_table_o :
	cd input/Set_table ; make all

main_o :
	cd main ; make all

mpi_comm_o :
	cd mpi_comm ; make all

partition_o :
	cd partition ; make all

seq_o :
	cd seq ; make all

src_o :
	cd src ; make all

util_o :
	cd util ; make all

clean : clean_blas clean_scalapack clean_mumps clean_str

clean_blas :
	cd $(BLAS_DIR) ; make clean

clean_scalapack :
	cd $(SCALAPACK_DIR) ; make clean

clean_mumps :
	cd $(MUMPS_DIR) ; make clean

clean_str :
	rm -f */*.o */*/*.o module/*.mod *.exe
