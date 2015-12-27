
library ieee;

use ieee.std_logic_1164.all;
use work.Sim6502_Package.all;

entity Sim6502_TestBench is
begin
end entity Sim6502_TestBench;

architecture arch of Sim6502_TestBench is

signal CLK : std_logic;

signal MEM_ADDRESS         : std_logic_vector(15 downto 0);
signal MEM_DATA_MEM_TO_CPU : std_logic_vector(7 downto 0);
signal MEM_DATA_CPU_TO_MEM : std_logic_vector(7 downto 0);
signal MEM_TRANSFER_TYPE   : MemoryTransferType;

begin

    process (CLK) is
    begin
        if rising_edge(CLK) then
            report "rising";
        end if;
    end process;

    processor: entity work.Sim6502
        port map(
            CLK               => CLK,
            MEM_ADDRESS       => MEM_ADDRESS,
            MEM_DATA_IN       => MEM_DATA_MEM_TO_CPU,
            MEM_DATA_OUT      => MEM_DATA_CPU_TO_MEM,
            MEM_TRANSFER_TYPE => MEM_TRANSFER_TYPE
        );

    process is
    begin
        wait for 10 ns;
        for i in 1 to 10 loop
            CLK <= '0';
            wait for 10 ns;
            CLK <= '1';
            wait for 10 ns;
        end loop;
        CLK <= '0';
        wait;
    end process;

end architecture arch;
