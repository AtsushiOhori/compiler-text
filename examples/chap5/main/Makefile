SMLSHARP = smlsharp
SMLFLAGS = -O2
LIBS =
all: Main
Main: ../parser/Syntax.smi ../parser/CoreML.grm.sig ../parser/CoreML.grm.smi \
 ../parser/CoreML.lex.smi ../parser/Parser.smi Top.smi Main.smi \
 ../parser/Syntax.o ../parser/CoreML.grm.o ../parser/CoreML.lex.o \
 ../parser/Parser.o Top.o Main.o
	$(SMLSHARP) $(LDFLAGS) -o Main Main.smi $(LIBS)
../parser/Syntax.o: ../parser/Syntax.sml ../parser/Syntax.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../parser/Syntax.o -c ../parser/Syntax.sml
../parser/CoreML.grm.sig: ../parser/CoreML.grm
	smlyacc ../parser/CoreML.grm
../parser/CoreML.grm.sml: ../parser/CoreML.grm
	smlyacc ../parser/CoreML.grm
../parser/CoreML.grm.o: ../parser/CoreML.grm.sml ../parser/Syntax.smi \
 ../parser/CoreML.grm.sig ../parser/CoreML.grm.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../parser/CoreML.grm.o -c \
 ../parser/CoreML.grm.sml
../parser/CoreML.lex.sml: ../parser/CoreML.lex
	smllex ../parser/CoreML.lex
../parser/CoreML.lex.o: ../parser/CoreML.lex.sml ../parser/Syntax.smi \
 ../parser/CoreML.grm.sig ../parser/CoreML.grm.smi ../parser/CoreML.lex.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../parser/CoreML.lex.o -c \
 ../parser/CoreML.lex.sml
../parser/Parser.o: ../parser/Parser.sml ../parser/Syntax.smi \
 ../parser/CoreML.grm.sig ../parser/CoreML.grm.smi ../parser/CoreML.lex.smi \
 ../parser/Parser.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../parser/Parser.o -c ../parser/Parser.sml
Top.o: Top.sml ../parser/Syntax.smi ../parser/CoreML.grm.sig \
 ../parser/CoreML.grm.smi ../parser/CoreML.lex.smi ../parser/Parser.smi Top.smi
	$(SMLSHARP) $(SMLFLAGS) -o Top.o -c Top.sml
Main.o: Main.sml ../parser/Syntax.smi ../parser/CoreML.grm.sig \
 ../parser/CoreML.grm.smi ../parser/CoreML.lex.smi ../parser/Parser.smi \
 Top.smi Main.smi
	$(SMLSHARP) $(SMLFLAGS) -o Main.o -c Main.sml
clean:
	rm -f ./*.o ../parser/*.o  ../typeinf/*.o Main
	rm -f ../parser/CoreML.grm.sml ../parser/CoreML.grm.sig ../parser/CoreML.lex.sml 
.SUFFIXES: .grm .lex
