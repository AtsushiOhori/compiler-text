SMLSHARP = smlsharp
SMLFLAGS = -O2
LIBS =
all: Main
Main: ../lex/Token.smi ../lex/CoreML.lex.smi ../lex/Lexer.smi Top.smi Main.smi \
 ../lex/Token.o ../lex/CoreML.lex.o ../lex/Lexer.o Top.o Main.o
	$(SMLSHARP) $(LDFLAGS) -o Main Main.smi $(LIBS)
../lex/Token.o: ../lex/Token.sml ../lex/Token.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../lex/Token.o -c ../lex/Token.sml
../lex/CoreML.lex.sml:
	smllex ../lex/CoreML.lex
../lex/CoreML.lex.o: ../lex/CoreML.lex.sml ../lex/Token.smi \
 ../lex/CoreML.lex.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../lex/CoreML.lex.o -c ../lex/CoreML.lex.sml
../lex/Lexer.o: ../lex/Lexer.sml ../lex/Token.smi ../lex/CoreML.lex.smi \
 ../lex/Lexer.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../lex/Lexer.o -c ../lex/Lexer.sml
Top.o: Top.sml ../lex/Token.smi ../lex/CoreML.lex.smi ../lex/Lexer.smi Top.smi
	$(SMLSHARP) $(SMLFLAGS) -o Top.o -c Top.sml
Main.o: Main.sml ../lex/Token.smi ../lex/CoreML.lex.smi ../lex/Lexer.smi \
 Top.smi Main.smi
	$(SMLSHARP) $(SMLFLAGS) -o Main.o -c Main.sml
clean:
	rm -f ../lex/Token.o ../lex/CoreML.lex.sml ../lex/CoreML.lex.o ../lex/Lexer.o Top.o Main.o Main

