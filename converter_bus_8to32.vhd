 library ieee;
 use ieee.std_logic_1164.all;
 use ieee.numeric_std.all;

 entity converter_bus_8to32 is 
	port (
		 clk : in std_logic;
		 RST : in std_logic;
		 load : in std_logic; 
		 rst_addr_32bit  : in std_logic;
		 addr_data_8bit : in std_logic_vector (1 downto 0);
		 addr_32bit	: out std_logic_vector (7 downto 0);	 
		 data_in : in std_logic_vector (7 downto 0);
		 data_mem : out std_logic_vector (31 downto 0);
		 rw  : out std_logic 
		);
 end entity converter_bus_8to32;
 
 architecture RTL of converter_bus_8to32 is
 
 signal reg_addr_data  : std_logic_vector(1 downto 0); 
 signal reg_data_32bit, reg_data_mem : std_logic_vector(31 downto 0); 
 signal reg_strob_wr, reg_shift_strob_wr  : std_logic;
 signal reg_addr_32bit	:  std_logic_vector (7 downto 0);
  
  begin
  
  
	 process (all)
 begin
 if(RST = '1')  then
	reg_addr_data <= (others=>'0');

 elsif(rising_edge(CLK)) then 
   reg_addr_data <= addr_data_8bit;
	
 end if;
 end process;	
	
 
 process (all)
 begin
 if(RST = '1')  then
	reg_data_32bit <= (others=>'0');

 elsif(rising_edge(CLK)) then 
	if load = '1' then
		if  reg_addr_data = "00" then
			reg_data_32bit(7 downto 0) <= data_in;
		elsif  reg_addr_data = "01" then
			reg_data_32bit(15 downto 8) <= data_in;
		elsif  reg_addr_data = "10" then
			reg_data_32bit(23 downto 16) <= data_in;
		elsif  reg_addr_data = "11" then
			reg_data_32bit(31 downto 24) <= data_in;
		end if;	
	end if;
 end if;
 end process; 
 
 process (all)
 begin
 if(RST = '1')  then
	reg_strob_wr <= '0';
	reg_addr_32bit <= (others=>'1');

 elsif(rising_edge(CLK)) then 
	if rst_addr_32bit = '1' then
		reg_addr_32bit <= (others=>'0');
	else	
		if  reg_addr_data(1 downto 0) = "11" then
			reg_strob_wr <= '1';
			reg_addr_32bit <= std_logic_vector (unsigned(reg_addr_32bit) + 1);	
		else
			reg_strob_wr <= '0';
		end if;
	end if;		
 end if;
 end process;

 process (all)
 begin
 if(RST = '1')  then
	reg_shift_strob_wr <= '0';

 elsif(rising_edge(CLK)) then 
	reg_shift_strob_wr <= reg_strob_wr;		
 end if;
 end process;
 
 process (all)
 begin
 if(RST = '1')  then
 
  elsif(rising_edge(CLK)) then 
	--if reg_shift_strob_wr = '1' then
	if reg_strob_wr = '1' then
		reg_data_mem <= reg_data_32bit;
		--addr_32bit <= reg_addr_32bit;
	end if;	
 end if;
 end process;  
 
 data_mem <=reg_data_mem;
 rw <= reg_shift_strob_wr; --reg_strob_wr
 addr_32bit <= reg_addr_32bit;
  
 end RTL; 