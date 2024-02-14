FC = ifort
COPT = -O1 -fpconstant -zero
LOAD = make_str_file.out

$(LOAD): make_str_file.f90
	$(FC) -o $@ make_str_file.f90 $(COPT)

clean:;
	/bin/rm $(LOAD)
