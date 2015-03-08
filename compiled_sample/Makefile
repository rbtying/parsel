COMMAND = mkdir -p bin; mkdir -p obj; g++-4.9 -std=c++14
LINK_ARGS = -Llib -lsndfile
INC_ARGS = -Iinclude

bin/sample: src/fft4g.c obj/sample.o obj/Signal.o
	$(COMMAND) obj/sample.o obj/Signal.o -o bin/sample $(LINK_ARGS)

obj/sample.o: src/sample.cpp src/Signal.h src/fft4g.c
	$(COMMAND) $(INC_ARGS) -c src/sample.cpp -o obj/sample.o

obj/Signal.o: src/Signal.cpp src/Signal.h
	$(COMMAND) $(INC_ARGS) -c src/Signal.cpp -o obj/Signal.o

clean:
	rm -f obj/*
	rm -f bin/*
