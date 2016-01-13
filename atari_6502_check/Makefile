
.PHONY : clean run

atari_6502_check : atari_executable.cfg atari_6502_check.s
	cl65 -t none -C atari_executable.cfg atari_6502_check.s

run : atari_6502_check
	atari800 -H1 $(PWD) -hreadwrite -showspeed -xl -nobasic -xlxe_rom atarixl-2.rom -run atari_6502_check

clean :
	$(RM) atari_6502_check atari_6502_check.o
