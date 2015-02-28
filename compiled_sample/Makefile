COMMAND = g++-4.9 -std=c++14

bin/sample: fft4g.c obj/sample.o obj/Signal.o obj/wavfile.o
	$(COMMAND) obj/sample.o obj/Signal.o obj/wavfile.o -o bin/sample

obj/sample.o: sample.cpp Signal.h fft4g.c wavfile.h
	$(COMMAND) -c sample.cpp -o obj/sample.o

obj/Signal.o: Signal.cpp Signal.h
	$(COMMAND) -c Signal.cpp -o obj/Signal.o

obj/wavfile.o: wavfile.c wavfile.h
	$(COMMAND) -c wavfile.c -o obj/wavfile.o
