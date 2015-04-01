COMMAND = mkdir -p bin; mkdir -p obj; g++-4.9 -std=c++14
LINK_ARGS = -Llib -lsndfile
INC_ARGS = -Iinclude

bin/test: src/fft4g.c obj/test.o obj/outputs.o obj/Signal.o
	$(COMMAND) obj/* -o bin/test $(LINK_ARGS)

obj/test.o: src/test.cpp src/outputs.h src/fft4g.c
	$(COMMAND) $(INC_ARGS) -c src/test.cpp -o obj/test.o

obj/outputs.o: src/outputs.cpp src/outputs.h src/Signal.h
	$(COMMAND) $(INC_ARGS) -c src/outputs.cpp -o obj/outputs.o

obj/Signal.o: src/Signal.cpp src/Signal.h
	$(COMMAND) $(INC_ARGS) -c src/Signal.cpp -o obj/Signal.o

clean:
	rm -f obj/*
	rm -f bin/*
