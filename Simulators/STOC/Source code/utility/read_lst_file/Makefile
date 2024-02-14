FC = ifort
COPT = -O1 -fpconstant -zero
LOAD = read_lst_file.out

$(LOAD): read_lst_file.f90
	$(FC) -o $@ read_lst_file.f90 $(COPT)

clean:;
	/bin/rm $(LOAD)
