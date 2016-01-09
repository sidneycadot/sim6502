
TARGET = atari

.PHONY : clean run

runtest : runtest.o test.o
	cl65 --target $(TARGET) -O runtest.o test.o

runtest.o : runtest.c test.h
	cl65 --target $(TARGET) -O runtest.c -c -o runtest.o

test.o : test.s
	cl65 --target $(TARGET) test.s -c -o test.o

run : runtest
	sim65 runtest

clean :
	$(RM) runtest runtest.o test.o
