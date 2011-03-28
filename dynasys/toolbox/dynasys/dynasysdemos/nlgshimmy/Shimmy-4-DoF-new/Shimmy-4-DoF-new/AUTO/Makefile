#
#   Makefile		3/1994		Xianjun WANG
#
PGM    = 7d
FC     = f77
FFLAGS = -O
SRC    = $(PGM).f
EXE    = $(PGM).exe
LIBS   = $(AUTO_DIR)/lib/*.o
RM     = rm -f
#
all: clean $(EXE) run
#
$(EXE): $(SRC:.f=.o)
	$(FC) $(FFLAGS) $(SRC:.f=.o) -o $@ $(LIBS)
#
run: 1
#
1:	$(EXE)
	@echo " "
	@echo "$(PGM) : run 1 : continuation of equilibria"
	@cp r.$(PGM).1 r.$(PGM)
	@"@r" $(PGM)
	@"@sv" $(PGM).1
	@$(RM) fort.*
#
2a:	$(EXE)
	@echo " "
	@echo "$(PGM) : run 2a : Periodic orbits from Hopf PAR(4)"
	@cp r.$(PGM).2.a r.$(PGM)
	@"@r" $(PGM) $(PGM).1
	@"@sv" $(PGM).2.a
	@$(RM) fort.*
#
3a:	$(EXE)
	@echo " "
	@echo "$(PGM) : run 3a : Two-par continuation of Hopf 1-2"
	@cp r.$(PGM).3.a r.$(PGM)
	@"@r" $(PGM) $(PGM).1
	@"@sv" $(PGM).3.a
	@$(RM) fort.*
#
3b:	$(EXE)
	@echo " "
	@echo "$(PGM) : run 3b : Two-par continuation of Hopf 3-2"
	@cp r.$(PGM).3.b r.$(PGM)
	@"@r" $(PGM) $(PGM).1
	@"@sv" $(PGM).3.b
	@$(RM) fort.*
#
4a:	$(EXE)
	@echo " "
	@echo "$(PGM) : run 4a : Two-par continuation of SN 4-5"
	@cp r.$(PGM).4.a r.$(PGM)
	@"@r" $(PGM) $(PGM).2.a
	@"@sv" $(PGM).4.a
	@$(RM) fort.*
#
4b:	$(EXE)
	@echo " "
	@echo "$(PGM) : run 4b : Two-par continuation of SN 4-5"
	@cp r.$(PGM).4.b r.$(PGM)
	@"@r" $(PGM) $(PGM).4.a
	@"@sv" $(PGM).4.b
	@$(RM) fort.*
#
clean:
	@echo "Cleaning $(PGM) ..."
	@cp $(SRC) $(EXE)
	@$(RM) *.exe *.o p.* q.* d.* fort.* *~ core
	@cp r.$(PGM).1 r.$(PGM)
	@echo "Cleaning ... done"
