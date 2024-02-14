FC = ifort
COPT = -O1 -fpconstant -zero
LOAD = read_str_file.out

$(LOAD): read_str_file.f90
	$(FC) -o $@ read_str_file.f90 $(COPT)

clean:;
	/bin/rm $(LOAD)
