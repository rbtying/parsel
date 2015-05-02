COMMAND = mkdir -p bin; mkdir -p obj; g++-4.9 -std=c++14
LINK_ARGS = -Lcpp/lib -lsndfile
INC_ARGS = -Icpp/include -Icpp

bin/main: cpp/fft4g.c obj/main.o obj/outputs.o obj/Signal.o obj/FSignal.o obj/Interval.o
	$(COMMAND) obj/* -o bin/main $(LINK_ARGS)

obj/main.o: main.cpp cpp/outputs.h cpp/fft4g.c
	$(COMMAND) $(INC_ARGS) -c main.cpp -o obj/main.o

obj/outputs.o: cpp/outputs.cpp cpp/outputs.h cpp/Signal.h
	$(COMMAND) $(INC_ARGS) -c cpp/outputs.cpp -o obj/outputs.o

obj/Signal.o: cpp/Signal.cpp cpp/Signal.h
	$(COMMAND) $(INC_ARGS) -c cpp/Signal.cpp -o obj/Signal.o

obj/FSignal.o: cpp/FSignal.cpp cpp/FSignal.h cpp/Interval.h
	$(COMMAND) $(INC_ARGS) -c cpp/FSignal.cpp -o obj/FSignal.o

obj/Interval.o: cpp/Interval.cpp cpp/Interval.h
	$(COMMAND) $(INC_ARGS) -c cpp/Interval.cpp -o obj/Interval.o

clean:
	rm -f obj/*
	rm -f bin/*
