SMLSHARP = smlsharp
SMLFLAGS = -O2
LIBS =
all: Main
Main: ../parser/Syntax.smi ../parser/CoreML.grm.sig ../parser/CoreML.grm.smi \
 ../parser/CoreML.lex.smi ../parser/Parser.smi ../typeinf/Type.smi \
 ../typeinf/TypeUtils.smi ../typeinf/UnifyTy.smi ../typeinf/Typeinf.smi \
 ../eval/Value.smi ../eval/Eval.smi Top.smi Main.smi ../parser/Syntax.o \
 ../parser/CoreML.grm.o ../parser/CoreML.lex.o ../parser/Parser.o \
 ../typeinf/Type.o ../typeinf/TypeUtils.o ../typeinf/UnifyTy.o \
 ../typeinf/Typeinf.o ../eval/Value.o ../eval/Eval.o Top.o Main.o
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
../typeinf/Type.o: ../typeinf/Type.sml ../typeinf/Type.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../typeinf/Type.o -c ../typeinf/Type.sml
../typeinf/TypeUtils.o: ../typeinf/TypeUtils.sml ../typeinf/Type.smi \
 ../typeinf/TypeUtils.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../typeinf/TypeUtils.o -c \
 ../typeinf/TypeUtils.sml
../typeinf/UnifyTy.o: ../typeinf/UnifyTy.sml ../typeinf/Type.smi \
 ../typeinf/TypeUtils.smi ../typeinf/UnifyTy.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../typeinf/UnifyTy.o -c \
 ../typeinf/UnifyTy.sml
../typeinf/Typeinf.o: ../typeinf/Typeinf.sml ../parser/Syntax.smi \
 ../typeinf/Type.smi ../typeinf/TypeUtils.smi ../typeinf/UnifyTy.smi \
 ../typeinf/Typeinf.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../typeinf/Typeinf.o -c \
 ../typeinf/Typeinf.sml
../eval/Value.o: ../eval/Value.sml ../parser/Syntax.smi ../eval/Value.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../eval/Value.o -c ../eval/Value.sml
../eval/Eval.o: ../eval/Eval.sml ../parser/Syntax.smi ../eval/Value.smi \
 ../eval/Eval.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../eval/Eval.o -c ../eval/Eval.sml
Top.o: Top.sml ../parser/Syntax.smi ../parser/CoreML.grm.sig \
 ../parser/CoreML.grm.smi ../parser/CoreML.lex.smi ../parser/Parser.smi \
 ../typeinf/Type.smi ../typeinf/TypeUtils.smi ../typeinf/UnifyTy.smi \
 ../typeinf/Typeinf.smi ../eval/Value.smi ../eval/Eval.smi Top.smi
	$(SMLSHARP) $(SMLFLAGS) -o Top.o -c Top.sml
Main.o: Main.sml ../parser/Syntax.smi ../parser/CoreML.grm.sig \
 ../parser/CoreML.grm.smi ../parser/CoreML.lex.smi ../parser/Parser.smi \
 ../typeinf/Type.smi ../typeinf/TypeUtils.smi ../typeinf/UnifyTy.smi \
 ../typeinf/Typeinf.smi ../eval/Value.smi ../eval/Eval.smi Top.smi Main.smi
	$(SMLSHARP) $(SMLFLAGS) -o Main.o -c Main.sml
clean:
	rm -f ./*.o ../parser/*.o  ../typeinf/*.o ../eval/*.o Main
	rm -f ../parser/CoreML.grm.sml ../parser/CoreML.grm.sig ../parser/CoreML.lex.sml 
.SUFFIXES: .grm .lex
