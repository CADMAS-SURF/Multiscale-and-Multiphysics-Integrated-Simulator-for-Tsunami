#
# Makefile
#
FC = mpif90
LOAD = ma.out

#optimize
COPT    = -O2 -fallow-argument-mismatch
LOPT =  $(COPT)

SRCS = allocate_cadmas.f90            \
       allocate_potential.f90         \
       errmsg.f90                     \
       errstop.f90                    \
       get_direction.f90              \
       initialize.f90                 \
       main.f90                       \
       make_n_potential.f90           \
       make_potential_mob.f90         \
       make_potential_mob_revise.f90  \
       make_potential_shelter.f90     \
       make_signpost.f90              \
       move_agent.f90                 \
       open_file.f90                  \
       output.f90                     \
       pot_total.f90                  \
       random_normal.f90              \
       read_agent.f90                 \
       read_cadmas.f90                \
       read_condition.f90             \
       read_move_boundary.f90         \
       read_potential.f90             \
       read_shelter.f90               \
       read_signpost.f90              \
       recursive_search_shelter.f90   \
       remesh.f90                     \
       solver.f90                     \
       update_attribute.f90           \
       update_rw.f90                  \
       slope_function.f90

SRCSM= mod_comm.f90    \
       m_agent.f90     \
       m_cadmas.f90    \
       m_output.f90    \
       m_potential.f90 \
       m_timectl.f90   \
       m_flag.f90      \
       m_prob.f90      \
       m_danger.f90

OBJS = $(SRCS:.f90=.o) $(SRCSM:.f90=.o)
MODLS= $(SRCSM:.f90=.mod)

$(LOAD): $(MODLS) $(OBJS)
	$(FC) -o $@ $(OBJS) $(LOPT)

clean:;	rm -f $(MODLS) $(OBJS) $(LOAD) core

.SUFFIXES:.f90 .mod .o

.f90.mod:
	$(FC) -c $(COPT) $<

.f90.o:
	$(FC) -c $(COPT) $<

allocate_cadmas.o: m_potential.mod m_cadmas.mod
allocate_potential.o: m_potential.mod m_flag.mod
errmsg.o: m_timectl.mod
get_direction.o: m_potential.mod m_agent.mod
initialize.o: m_agent.mod m_potential.mod m_cadmas.mod
main.o: mod_comm.mod m_timectl.mod
make_n_potential.o: m_potential.mod m_agent.mod m_cadmas.mod m_prob.mod m_danger.mod
make_potential_mob.o: m_potential.mod m_agent.mod
make_potential_mob_revise.o: m_potential.mod m_agent.mod
make_potential_shelter.o: m_potential.mod m_agent.mod m_flag.mod m_prob.mod m_cadmas.mod
make_signpost.o: m_potential.mod m_agent.mod
move_agent.o: m_timectl.mod m_potential.mod m_agent.mod m_cadmas.mod
output.o: m_timectl.mod m_agent.mod m_potential.mod m_output.mod
pot_total.o: m_potential.mod m_agent.mod
read_agent.o: m_potential.mod m_agent.mod
read_cadmas.o: m_timectl.mod m_cadmas.mod m_potential.mod
read_condition.o: m_timectl.mod m_agent.mod m_potential.mod m_output.mod m_flag.mod m_prob.mod m_danger.mod
read_danger.o: m_potential.mod
read_move_boundary.o: m_potential.mod
read_potential.o: m_potential.mod
read_shelter.o: m_potential.mod
read_signpost.o: m_potential.mod
recursive_search_shelter.o: m_potential.mod m_agent.mod m_cadmas.mod
remesh.o: m_potential.mod m_cadmas.mod
solver.o: m_timectl.mod m_potential.mod m_agent.mod
update_attribute.o: m_agent.mod m_potential.mod m_cadmas.mod
update_rw.o: m_agent.mod

