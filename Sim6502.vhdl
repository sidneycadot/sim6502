
library ieee;

use ieee.std_logic_1164.all;

use work.Sim6502_Package.all;

entity Sim6502 is
    port (
        CLK               : in  std_logic;
        MEM_ADDRESS       : out std_logic_vector(15 downto 0);
        MEM_DATA_IN       : in  std_logic_vector(7 downto 0);
        MEM_DATA_OUT      : out std_logic_vector(7 downto 0);
        MEM_TRANSFER_TYPE : out MemoryTransferType
    );
end entity Sim6502;

architecture arch of Sim6502 is

type Instruction is (
    JMP_INDIRECT
);

signal PC : unsigned(15 downto 0);

type Sim6502_state_record is
    record
        -- externally observable state
        ADDRESS       : std_logic_vector(15 downto 0);
        DATA_OUT      : std_logic_vector( 7 downto 0);
        TRANSFER_TYPE : MemoryTransferType;
        -- internal state
        REG_PC : std_logic_vector(15 downto 0);
        REG_IP : std_logic_vector( 7 downto 0);
        REG_A  : std_logic_vector( 7 downto 0);
        REG_X  : std_logic_vector( 7 downto 0);
        REG_Y  : std_logic_vector( 7 downto 0);
        REG_S  : std_logic_vector( 7 downto 0);
        REG_P  : std_logic_vector( 7 downto 0);
    end record Sim6502_state_record;

constant initial_state : Sim6502_state_record (
        ADDRESS       <= x"fffe",
        DATA_OUT      <= x"-",
        TRANSFER_TYPE <= READ,
        REG_PC        <= x"0000",
        REG_IP        <= x"0000",
        REG_A         <= x"00",
        REG_X         <= x"00",
        REG_Y         <= x"00",
        REG_S         <= x"00",
        REG_P         <= x"00"
    );
signal current_state : Sim6502_state_record := initial_state;

begin

    MEM_ADDRESS  <= current_state.ADDRESS;
    MEM_DATA_OUT <= current_state.DATA_OUT;
    MEM_DATA_OUT <= current_state.TRANSFER_TYPE;

    process (CLK) is
    variable state : Sim6502_state_record;
    begin
        if rising_edge(CLK) then
            state := current_state;
            current_state <= state;
        end if;
    end process;

end architecture arch;
