# mod_fault.f90 is copied from %STOC_INSTALL_DIR/COM/mod_fault.90

FC = ifort
COPT = -O1 -fpconstant -zero
LOAD = make_sbt_file.out

$(LOAD): mod_fault.o make_sbt_file.o
	$(FC) -o $@ mod_fault.o make_sbt_file.o $(COPT)

mod_fault.o: mod_fault.f
	$(FC) -o $@ mod_fault.f -c $(COPT)

make_sbt_file.o: make_sbt_file.f90 mod_fault.mod
	$(FC) -o $@ make_sbt_file.f90 -c $(COPT)

clean:;
	/bin/rm make_sbt_file.o mod_fault.mod mod_fault.o $(LOAD)
