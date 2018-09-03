library ieee;
use ieee.std_logic_1164.all;
use IEEE.NUMERIC_STD.ALL;

entity generator_data is
generic (
    DATA    : integer := 32
);
port (
	Clk : in  STD_LOGIC;
	rst : in  STD_LOGIC;
	en  : in  STD_LOGIC;
	timer_data : in  STD_LOGIC_vector(((DATA/2)-1) downto 0);
	wr : out  STD_LOGIC;
	data_out : out  STD_LOGIC_vector((DATA-1) downto 0)
);
end generator_data;

 architecture rtl of generator_data is

 signal reg_timer :  std_logic_vector(((DATA/2)-1) downto 0);	
 signal reg_count :  std_logic_vector((DATA-1) downto 0);
 signal reg_wr : std_logic;
 begin	

--timer:
 process (all) 
	begin 
    if rst = '1'  then
        reg_timer 	<= (others => '0');
		  reg_wr		<= '0';
    elsif (rising_edge(clk)) then
		if en = '1'  then				-- is timer enabled?
			if (reg_timer = (timer_data)) then	-- check if timer reached final count
				reg_wr      <=	'1';		
				reg_timer 	<= (others => '0');				
			else
				reg_timer <= std_logic_vector (unsigned(reg_timer) + 1);	
				reg_wr    <=	'0';
			end if;
		
		end if;
	end if;		
end process;

 process (all) 
	begin 
    if rst = '1'  then
        reg_count 	<= (others => '0');
    elsif (rising_edge(clk)) then
		--if ((en = '1') and (reg_wr = '1'))  then	
		if reg_wr = '1' then
			reg_count <= std_logic_vector (unsigned(reg_count) + 1);
		end if;
	end if;		
end process;

data_out <= reg_count;
wr <= reg_wr;

end rtl;

