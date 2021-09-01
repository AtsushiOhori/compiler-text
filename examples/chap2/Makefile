SMLSHARP = smlsharp
SMLFLAGS = -O2
LIBS =
all: Main
Main: TM.smi Eval.smi Main.smi TM.o Eval.o Main.o
	$(SMLSHARP) $(LDFLAGS) -o Main Main.smi $(LIBS)
TM.o: TM.sml TM.smi
	$(SMLSHARP) $(SMLFLAGS) -o TM.o -c TM.sml
Eval.o: Eval.sml TM.smi Eval.smi
	$(SMLSHARP) $(SMLFLAGS) -o Eval.o -c Eval.sml
Main.o: Main.sml TM.smi Eval.smi Main.smi
	$(SMLSHARP) $(SMLFLAGS) -o Main.o -c Main.sml
clean:
	rm -f ./Main ./*.o
