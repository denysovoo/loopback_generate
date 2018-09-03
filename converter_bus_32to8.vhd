 library ieee;
 use ieee.std_logic_1164.all;
 use ieee.numeric_std.all;

 entity converter_bus_32to8 is 
	port (
CLK      : in  std_logic; -- system clock
RST      : in  std_logic; -- high active synchronous reset
rdy_data32bit : in std_logic;
data32bit : in std_logic_vector(31 downto 0);
data_out  : out std_logic_vector(7 downto 0); 
rdy_data8bit : out std_logic;
end_convert_32to8 : out std_logic
);
 end entity converter_bus_32to8;

 architecture RTL of converter_bus_32to8 is
 
signal reg_strob_rdy, reg_shift_strob_rdy : std_logic;
signal reg_start_pause  : std_logic;
signal reg_work_pause : std_logic;
signal reg_cnt_pause : std_logic_vector(1 downto 0);
signal reg_data32bit : std_logic_vector(31 downto 0);
signal reg_data_out : std_logic_vector(7 downto 0);
signal rdy_shift_data8bit : std_logic;

begin

reg_start_pause <= rdy_data32bit;
--rdy_data8bit <= reg_work_pause;
--end_convert_32to8 <= reg_strob_rdy;
data_out <= reg_data_out;

process(all)
begin
 if(RST = '1')  then
   reg_data32bit   <= (others => '0');
 elsif(rising_edge(CLK)) then
	if reg_start_pause = '1' then
		reg_data32bit <= data32bit; 
	end if;
 end if;
end process;

	
process(all)
begin
 if(RST = '1')  then
   reg_strob_rdy   <= '0';
   reg_work_pause <= '0';
   reg_cnt_pause   <= (others => '0');
  -- bin8_cnt_pause  <= (others => '0');
 elsif(rising_edge(CLK)) then
	if reg_start_pause = '1' then
		reg_work_pause <= '1';
		--bin8_cnt_pause <= pause_value;
	elsif reg_strob_rdy  = '1' then
		reg_work_pause <= '0';
	end if;
	
	if reg_work_pause = '1' then
		if reg_cnt_pause = "11" then
			reg_cnt_pause <= (others => '0');
		else
			reg_cnt_pause <= std_logic_vector (unsigned(reg_cnt_pause) + 1);
		end if;
	end if;
	
	if reg_cnt_pause = "11" then 
		reg_strob_rdy   <= '1';
	else
		reg_strob_rdy   <= '0';	
	end if;
 end if;
end process;


 process (all)
 begin
 if(RST = '1')  then
	reg_data_out <= (others=>'0');

 elsif(rising_edge(CLK)) then 
	if  reg_cnt_pause = "01" then
		reg_data_out <= reg_data32bit(7 downto 0);
	elsif  reg_cnt_pause = "00" then
		reg_data_out <= reg_data32bit(15 downto 8);
	elsif  reg_cnt_pause = "11" then
		reg_data_out <= reg_data32bit(23 downto 16);
	elsif  reg_cnt_pause = "10" then
		reg_data_out <= reg_data32bit(31 downto 24);
	end if;	
 end if;
 end process;
 
 
  process (all)
 begin
 if(RST = '1')  then
	rdy_shift_data8bit <= '0';

 elsif(rising_edge(CLK)) then 
 rdy_shift_data8bit <= reg_work_pause;
 rdy_data8bit <= rdy_shift_data8bit;
 
 reg_shift_strob_rdy <= reg_strob_rdy;
 end_convert_32to8 <= reg_shift_strob_rdy;
 
 end if;
 end process; 

end RTL; 
 
   			