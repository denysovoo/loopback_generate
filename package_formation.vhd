library ieee;
use ieee.std_logic_1164.all;
library work;    

package Const_type_package_formation is
type headline_array is array (0 to 7) of std_logic_vector (7 downto 0) ;
type serial_number_packet_array is array (0 to 3) of std_logic_vector (7 downto 0) ;
type amount_data_transmitted_array is array (0 to 1) of std_logic_vector (7 downto 0) ;
type data_mem_array is array (0 to 3) of std_logic_vector (7 downto 0) ;
--constant num_conf_data : std_logic_vector(4 downto 0) := "01010";
--constant num_ram_data : std_logic_vector(7 downto 0) := "10000000";
--constant num_data_read_from_mem : std_logic_vector(7 downto 0) := "00100011";
end package;



 library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.Const_type_package_formation.all;

entity package_formation is 
	port (
		 clk : in std_logic;
		 RST : in std_logic;
		 start : in std_logic;
		headline  : in std_logic_vector(63 downto 0);
		amount_data_transmitted : in std_logic_vector(15 downto 0);
		out_data : out std_logic_vector(7 downto 0);
		
		addr_mem : out std_logic_vector(15 downto 0);
		data_from_mem  : in std_logic_vector(31 downto 0);
		wr_fifo : out std_logic;
				
		ready  : out std_logic
);
end entity package_formation;


architecture rtl of package_formation is

 -- Architecture Declarations
 TYPE state_type IS (idle, transfer_headline, transfer_serial_number_packet, transfer_amount_data_transmitted_array, read_data_memory, transfer_data_to_fifo, ready_transfer_packet);
 -- Declare current and next state signals
 SIGNAL current_state, nxt_state : state_type ;

signal reg_serial_number_packet  : std_logic_vector(32 downto 0);
signal reg_amount_data_transmitted, reg_cnt_data : std_logic_vector(15 downto 0);
signal reg_cnt_transfer_headline : std_logic_vector(3 downto 0);
signal reg_cnt_serial_number_packet : std_logic_vector(2 downto 0);
signal reg_cnt_amount_data_transmitted : std_logic_vector(1 downto 0);

signal reg_headline_array : headline_array;
signal reg_serial_number_packet_array : serial_number_packet_array;
signal reg_amount_data_transmitted_array : amount_data_transmitted_array;
signal reg_data_mem_array : data_mem_array;

signal reg_out_data :  std_logic_vector(7 downto 0);
signal reg_rdy_outdata :  std_logic;
signal cnt_byte : std_logic_vector(2 downto 0);
signal reg_wr_fifo  :  std_logic;


begin

out_data <= reg_out_data;
addr_mem <= reg_cnt_data;
wr_fifo <= reg_wr_fifo or reg_rdy_outdata;

 process (all)
 begin
--if(RST = '1')  then
--reg_headline_array <= (others=> (others=>'0'));
--elsif(rising_edge(CLK)) then 
	for i in 1 to 8 loop
			reg_headline_array(i-1) <= headline(((8*i)-1) downto 8*(i-1));
	end loop;		
--end if;
end process;

 process (all)
 begin
-- if(RST = '1')  then
-- reg_serial_number_packet_array <= (others=> (others=>'0'));
-- elsif(rising_edge(CLK)) then 
	for i in 1 to 4 loop
			reg_serial_number_packet_array(i-1) <= reg_serial_number_packet(((8*i)-1) downto 8*(i-1));
	end loop;		
--end if;
end process;


 process (all)
 begin
-- if(RST = '1')  then
	-- reg_amount_data_transmitted_array <= (others=> (others=>'0'));
-- elsif(rising_edge(CLK)) then 
	reg_amount_data_transmitted_array(0) <= reg_amount_data_transmitted(7 downto 0);
	reg_amount_data_transmitted_array(1) <= reg_amount_data_transmitted(15 downto 8);
--end if;
end process;

 process (all)
 begin
-- if(RST = '1')  then
--	reg_data_mem_array <= (others=> (others=>'0'));
-- elsif(rising_edge(CLK)) then 
	for i in 1 to 4 loop
			reg_data_mem_array(i-1) <= data_from_mem(((8*i)-1) downto 8*(i-1));
	end loop;		
--end if;
end process;




--------------------------------------------------------------------------
 clocked_proc : PROCESS ( all )
 --------------------------------------------------------------------------
 BEGIN
 IF (RST = '1') THEN
 current_state <= idle;
 -- Default Reset Values
		
		reg_serial_number_packet <= (others => '0');
		reg_amount_data_transmitted <= (others => '0');
		reg_cnt_transfer_headline <= (others => '0');
		reg_out_data <= (others => '0');
		reg_cnt_serial_number_packet <= (others => '0');
		reg_cnt_data <= (others => '0'); 
		reg_cnt_amount_data_transmitted <= (others => '0');
		reg_rdy_outdata <= '0';
		ready <= '0';
		reg_wr_fifo <= '0';
		reg_cnt_data <= (others => '0');
		cnt_byte <= (others => '0'); 		
		
 ELSIF (CLK'EVENT AND CLK = '1') THEN
 current_state <= nxt_state;

 -- Combined Actions
 CASE current_state IS
 WHEN idle => 
 
	if start = '1' then
		reg_serial_number_packet	<= std_logic_vector (unsigned(reg_serial_number_packet) + 1);
		reg_amount_data_transmitted <= amount_data_transmitted;
		reg_cnt_transfer_headline <= (others => '0');
		reg_out_data <= (others => '0');
		reg_cnt_serial_number_packet <= (others => '0');
		reg_cnt_amount_data_transmitted <= (others => '0');
		reg_cnt_data <= (others => '0'); 
		reg_rdy_outdata <= '0';
		ready <= '0';
		reg_wr_fifo <= '0';
		cnt_byte <= (others => '0'); 
	end if;	
		
 WHEN transfer_headline => 
	reg_cnt_transfer_headline <= std_logic_vector (unsigned(reg_cnt_transfer_headline) + 1);
	reg_out_data <= reg_headline_array(to_integer(unsigned(reg_cnt_transfer_headline)));
	reg_rdy_outdata <= '1';
	
 WHEN transfer_serial_number_packet => 
	reg_cnt_serial_number_packet <= std_logic_vector (unsigned(reg_cnt_serial_number_packet) + 1);
	reg_out_data <= reg_serial_number_packet_array(to_integer(unsigned(reg_cnt_serial_number_packet)));
	reg_rdy_outdata <= '1';
	
 WHEN transfer_amount_data_transmitted_array => 
	reg_cnt_amount_data_transmitted <= std_logic_vector (unsigned(reg_cnt_amount_data_transmitted) + 1);
	reg_out_data <= reg_amount_data_transmitted_array(to_integer(unsigned(reg_cnt_amount_data_transmitted)));
	reg_rdy_outdata <= '1';
--	if reg_cnt_amount_data_transmitted = "01" then
--		reg_cnt_data <= std_logic_vector (unsigned(reg_cnt_data) + 1);
--		reg_out_data <= reg_data_mem_array(to_integer(unsigned(cnt_byte)));
--		cnt_byte <= std_logic_vector (unsigned(cnt_byte) + 1);
--	end if;
	
 WHEN read_data_memory => 
--	reg_out_data <= reg_data_mem_array(to_integer(unsigned(cnt_byte)));
--	cnt_byte <= std_logic_vector (unsigned(cnt_byte) + 1);
	
 --when temp_state =>
--	reg_out_data <= reg_data_mem_array(to_integer(unsigned(cnt_byte)));
--	cnt_byte <= std_logic_vector (unsigned(cnt_byte) + 1);
	
 WHEN transfer_data_to_fifo =>	
	if cnt_byte = "011" then
		cnt_byte <= (others => '0');
		reg_out_data <= reg_data_mem_array(to_integer(unsigned(cnt_byte)));		
	else	
		reg_out_data <= reg_data_mem_array(to_integer(unsigned(cnt_byte)));
		cnt_byte <= std_logic_vector (unsigned(cnt_byte) + 1);
	end if;
	
	if cnt_byte = "000" then
		reg_cnt_data <= std_logic_vector (unsigned(reg_cnt_data) + 1);
	end if;
	
 WHEN ready_transfer_packet	=>
	reg_rdy_outdata <= '0';
	ready <= '1';
	
WHEN OTHERS => NULL;
END CASE;
END IF;

END PROCESS clocked_proc;
	
--------------------------------------------------------------------------
 nextstate_proc : PROCESS ( all )
 --------------------------------------------------------------------------
 BEGIN
 CASE current_state IS
 WHEN idle => 
 IF (start = '1') THEN
	nxt_state <= transfer_headline;
 ELSE
	nxt_state <= idle;
 END IF;

 WHEN transfer_headline =>
	if reg_cnt_transfer_headline = "0111" then
		nxt_state  <= transfer_serial_number_packet;
	else
		nxt_state  <= transfer_headline;
	end if;		
 
 WHEN transfer_serial_number_packet =>
	if reg_cnt_serial_number_packet = "011" then
		nxt_state  <= transfer_amount_data_transmitted_array;
	else
		nxt_state  <= transfer_serial_number_packet;
	end if;	

 WHEN transfer_amount_data_transmitted_array =>
	if reg_cnt_amount_data_transmitted = "01" then
		nxt_state  <= read_data_memory;
	else
		nxt_state  <= transfer_amount_data_transmitted_array;
	end if;		
	
 WHEN read_data_memory => 	
	nxt_state <= transfer_data_to_fifo;
 
-- when temp_state =>
--	nxt_state  <= transfer_data_to_fifo;

 WHEN transfer_data_to_fifo =>	
	if reg_cnt_data = reg_amount_data_transmitted then
		nxt_state  <= ready_transfer_packet;
	else
		nxt_state  <= transfer_data_to_fifo;
	end if;		
	
WHEN ready_transfer_packet =>
	if ready = '1' then
		nxt_state  <= idle;
	else
		nxt_state  <= ready_transfer_packet;
	end if;	
	
 WHEN OTHERS => nxt_state <= idle;
 END CASE;

 END PROCESS nextstate_proc;

end rtl; 