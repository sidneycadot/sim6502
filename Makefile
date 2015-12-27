
.PHONY : clean run

run :
	ghdl -a Sim6502_Package.vhdl
	ghdl -a Sim6502.vhdl
	ghdl -a Sim6502_TestBench.vhdl
	ghdl -e Sim6502_TestBench
	ghdl -r Sim6502_TestBench

clean :
	$(RM) *~ sim6502_testbench Sim6502_TestBench.o e~sim6502_testbench.o work-obj93.cf
