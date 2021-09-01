SMLSHARP = smlsharp
SMLFLAGS = -O2
LIBS =
all: Main
Main: ../readstring/ReadString.smi Top.smi Main.smi ../readstring/ReadString.o \
 Top.o Main.o
	$(SMLSHARP) $(LDFLAGS) -o Main Main.smi $(LIBS)
../readstring/ReadString.o: ../readstring/ReadString.sml \
 ../readstring/ReadString.smi
	$(SMLSHARP) $(SMLFLAGS) -o ../readstring/ReadString.o -c \
 ../readstring/ReadString.sml
Top.o: Top.sml ../readstring/ReadString.smi Top.smi
	$(SMLSHARP) $(SMLFLAGS) -o Top.o -c Top.sml
Main.o: Main.sml ../readstring/ReadString.smi Top.smi Main.smi
	$(SMLSHARP) $(SMLFLAGS) -o Main.o -c Main.sml
clean:
	rm Main Main.o Top.o ../readstring/ReadString.o
