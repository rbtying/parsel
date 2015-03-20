COMMAND = mkdir -p bin; mkdir -p obj; g++-4.9 -std=c++14
LINK_ARGS = -Llib -lsndfile
INC_ARGS = -Iinclude

bin/test: src/fft4g.c obj/test.o obj/outputs.o obj/Signal.o obj/Function.o obj/SigFunc.o
	$(COMMAND) obj/* -o bin/test $(LINK_ARGS)

obj/test.o: src/test.cpp src/outputs.h src/fft4g.c
	$(COMMAND) $(INC_ARGS) -c src/test.cpp -o obj/test.o

obj/outputs.o: src/outputs.cpp src/outputs.h src/Signal.h
	$(COMMAND) $(INC_ARGS) -c src/outputs.cpp -o obj/outputs.o

obj/Signal.o: src/Signal.cpp src/Signal.h
	$(COMMAND) $(INC_ARGS) -c src/Signal.cpp -o obj/Signal.o

obj/Function.o: src/Function.cpp src/Function.h
	$(COMMAND) $(INC_ARGS) -c src/Function.cpp -o obj/Function.o

obj/SigFunc.o: src/SigFunc.cpp src/SigFunc.h
	$(COMMAND) $(INC_ARGS) -c src/SigFunc.cpp -o obj/SigFunc.o

clean:
	rm -f obj/*
	rm -f bin/*
