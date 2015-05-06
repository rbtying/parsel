COMMAND = mkdir -p bin; mkdir -p obj; g++-4.9 -g -std=c++14
LINK_ARGS = -Lcpp/lib -lsndfile
INC_ARGS = -Icpp/include -Icpp

bin/main: cpp/fft4g.c obj/main.o obj/fillers.o obj/Signal.o obj/FSignal.o obj/Interval.o
	$(COMMAND) obj/* -o bin/main $(LINK_ARGS)

obj/main.o: main.cpp cpp/fillers.h cpp/fft4g.c cpp/Chunk.h cpp/utils.h
	$(COMMAND) $(INC_ARGS) -c main.cpp -o obj/main.o

obj/fillers.o: cpp/fillers.cpp cpp/fillers.h cpp/Signal.h
	$(COMMAND) $(INC_ARGS) -c cpp/fillers.cpp -o obj/fillers.o

obj/Signal.o: cpp/Signal.cpp cpp/Signal.h
	$(COMMAND) $(INC_ARGS) -c cpp/Signal.cpp -o obj/Signal.o

obj/FSignal.o: cpp/FSignal.cpp cpp/FSignal.h cpp/Interval.h
	$(COMMAND) $(INC_ARGS) -c cpp/FSignal.cpp -o obj/FSignal.o

obj/Interval.o: cpp/Interval.cpp cpp/Interval.h
	$(COMMAND) $(INC_ARGS) -c cpp/Interval.cpp -o obj/Interval.o

default: bin/ma

clean:
	rm -f obj/*
	rm -f bin/*
