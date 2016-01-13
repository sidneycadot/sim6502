
.PHONY : clean run default

default : atari_6502_check_NoWrite atari_6502_check_Write

atari_6502_check_NoWrite : atari_executable.cfg atari_6502_check.s
	cl65 --asm-define WFF=0 -t none -C atari_executable.cfg atari_6502_check.s -o atari_6502_check_NoWrite

atari_6502_check_Write : atari_executable.cfg atari_6502_check.s
	cl65 --asm-define WFF=1 -t none -C atari_executable.cfg atari_6502_check.s -o atari_6502_check_Write

run : atari_6502_check_Write
	atari800 -H1 $(PWD) -hreadwrite -showspeed -xl -nobasic -xlxe_rom atarixl-2.rom -run atari_6502_check_Write

clean :
	$(RM) atari_6502_check_Write atari_6502_check_NoWrite atari_6502_check.o
