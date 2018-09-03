-- Control_rockig_glass

library ieee;
use ieee.std_logic_1164.all;
library work;    

package Const_type is
type my_array is array (0 to 10) of std_logic_vector (7 downto 0) ; 
constant num_conf_data : std_logic_vector(4 downto 0) := "01010";
constant num_ram_data : std_logic_vector(9 downto 0) := (others => '1');--"10000000";
constant num_data_read_from_mem : std_logic_vector(7 downto 0) := "00100011";
end package;


library ieee;
use ieee.std_logic_1164.all;
--use ieee.std_logic_arith.all;
--use ieee.std_logic_unsigned.all;
use ieee.numeric_std.all;
use work.Const_type.all;

entity Control_rockig_glass is 
	port (
		-- Avalon bus signals
		 clk : in std_logic;
		 --reset_n : in std_logic;
		 --rdreq : out std_logic;
	    --wrreq : out std_logic;
		 s : out std_logic_vector (5 downto 0);
		 --test_outdata : out std_logic_vector (31 downto 0);
		 --test_indata : out std_logic_vector (7 downto 0);
		 --writedata : in std_logic_vector (7 downto 0);
		 start : out std_logic;
		-- FT2232 Bus Signals
		 usb_clock : in std_logic;
		 usb_data : inout std_logic_vector(7 downto 0);
		 usb_rd_n : out std_logic;
		 usb_wr_n : out std_logic;
		 usb_oe_n : out std_logic;
		 usb_rxf_n : in std_logic;
		 usb_txe_n : in std_logic
		);
end entity Control_rockig_glass;


architecture rtl of Control_rockig_glass is

	
	signal rx_fifo_wrclk : std_logic;
	signal rx_fifo_rdreq : std_logic;
	signal rx_fifo_rdclk : std_logic;
	signal rx_fifo_wrreq : std_logic;
	signal rx_fifo_data : std_logic_vector(7 downto 0);
	signal rx_fifo_rdempty : std_logic;
	signal rx_fifo_wrfull : std_logic;
	signal rx_fifo_q : std_logic_vector(7 downto 0);
	signal rx_fifo_rdusedw : std_logic_vector(11 downto 0);
	
	signal tx_fifo_wrclk : std_logic;
	signal tx_fifo_rdreq : std_logic;
	signal tx_fifo_rdclk : std_logic;
	signal tx_fifo_wrreq : std_logic;
	signal tx_fifo_data : std_logic_vector(7 downto 0);
	signal tx_fifo_rdempty : std_logic;
	signal tx_fifo_wrfull : std_logic;
	signal tx_fifo_q : std_logic_vector(7 downto 0);
	signal tx_fifo_wrusedw : std_logic_vector(11 downto 0);

	signal ft2232_wait : integer range 0 to 1 := 0;
	signal ft2232_bus_oe_mode : integer range 0 to 3 := 0;
	signal ft2232_tx_fifo_read : std_logic;
	signal ft2232_rx_fifo_write : std_logic;
	signal ft2232_tx_please : std_logic;
	signal ft2232_rx_please : std_logic;
	
	signal rx_fifo_valid : std_logic;
	
	
		-- Architecture Declarations
    TYPE state_type IS (full_RX_FIFO_st, read_FIFO_st, read_FIFO_st0, start_work_moduls_st, end_work_moduls_st, pause);
    -- Declare current and next state signals
    SIGNAL current_state, nxt_state : state_type ;	
	
	signal reg_start, reg_load_config :  std_logic;
	signal reg_addr_config	:  std_logic_vector(4 downto 0);
	signal reg_addr_data :  std_logic_vector(9 downto 0);
	signal reg_load_data :  std_logic;
	signal number_values  :  std_logic_vector(15 downto 0);
	
	signal r_rise_rdempty                  : std_logic;
	signal r_fall_rdempty                  : std_logic;
	signal p_input_rdempty       : std_logic_vector(0 to 2); -- input pipe
	
	signal r_rise_rdfull                  : std_logic;
	signal r_fall_rdfull                  : std_logic;
	signal p_input_rdfull       : std_logic_vector(0 to 2); -- input pipe
	
	signal r_rise_ready_buff                  : std_logic;
	signal r_fall_ready_buff                  : std_logic;
	signal p_input_ready_buff       : std_logic_vector(0 to 2); -- input pipe
	
	signal load_others, reg_start_load_others :  std_logic;
	signal RST, reg_aclr_rx_fifo :  std_logic;
	signal reg_junk :   STD_LOGIC_VECTOR (7 downto 0);
	signal reg_wr :  std_logic;
	signal reg_addr_32bit :   STD_LOGIC_VECTOR (7 downto 0);
	signal reg_data_mem :   STD_LOGIC_VECTOR (31 downto 0);
	
	--signal cnt_read_pause_mem  :  std_logic_vector(3 downto 0);
	--signal reg_wr_mem  :  std_logic;
	--signal cnt_addr_mem  :  std_logic_vector(7 downto 0);
	--signal reg_b_dout :  std_logic_vector(31 downto 0);
	
	--signal reg_rdy_data32bit : std_logic;
	--signal reg_data32bit : std_logic_vector(31 downto 0);
	--signal reg_outdata_converter_bus_32to8  : std_logic_vector(7 downto 0); 
	--signal reg_rdy_data8bit : std_logic;
	--signal reg_end_convert_32to8 : std_logic;
	--signal reg_work_pause, reg_strob_rdy : std_logic;
	--signal tx_fifo_wrreq_xxx  : std_logic;
	--signal cnt : std_logic_vector(1 downto 0);
	--signal reg_start_read_mem, en_cnt, en_write_mem : std_logic;
	
	signal reg_addr_mem : STD_LOGIC_VECTOR(15 DOWNTO 0);
	
	signal config_array: my_array;
	signal reg_out_data_package_formation : STD_LOGIC_VECTOR(7 DOWNTO 0);
	signal reg_wr_fifo : std_logic;
	SIGNAL reg_data_from_mem : STD_LOGIC_VECTOR(31 DOWNTO 0);
	signal reg_ready_buff : std_logic;
	
	signal reg_wr_test_generator, reg_mux_wr : std_logic;
	signal reg_data_out_test_generator, reg_mux_data : STD_LOGIC_VECTOR(31 DOWNTO 0);
	

	COMPONENT dcfifo
	GENERIC (
		intended_device_family	: STRING;
		lpm_numwords			: NATURAL;
		lpm_showahead			: STRING;
		lpm_type				: STRING;
		lpm_width				: NATURAL;
		lpm_widthu				: NATURAL;
		overflow_checking		: STRING;
		rdsync_delaypipe		: NATURAL;
		underflow_checking		: STRING;
		use_eab					: STRING;
		wrsync_delaypipe		: NATURAL
	);
	PORT (
			wrclk	: IN STD_LOGIC ;
			rdempty	: OUT STD_LOGIC ;
			rdreq	: IN STD_LOGIC ;
			wrfull	: OUT STD_LOGIC ;
			rdclk	: IN STD_LOGIC ;
			q		: OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
			wrreq	: IN STD_LOGIC ;
			data	: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
			rdusedw	: OUT STD_LOGIC_VECTOR (11 DOWNTO 0);
			wrusedw	: OUT STD_LOGIC_VECTOR (11 DOWNTO 0)
	);
	END COMPONENT;
	
COMPONENT double_buffer 
generic (
    DATA    : integer := 32;
    ADDR    : integer := 5
);
port (

	Clk : in  STD_LOGIC;
	rst : in  STD_LOGIC;
	input_data : in STD_LOGIC_VECTOR(31 downto 0);
	wr : in  STD_LOGIC;
	ready_buff :	out std_logic;
	
    -- Port B
    b_clk   : in  std_logic;
    --b_wr    : in  std_logic;
    b_addr  : in  std_logic_vector(ADDR-1 downto 0);
    --b_din   : in  std_logic_vector(DATA-1 downto 0);
    b_dout  : out std_logic_vector(DATA-1 downto 0)
);
 END COMPONENT;
	
	  COMPONENT converter_bus_8to32  
	port (
		 clk : in std_logic;
		 RST : in std_logic;
		 load : in std_logic;
		 rst_addr_32bit  : in std_logic;
		 addr_32bit	: out std_logic_vector (7 downto 0);
		 addr_data_8bit : in std_logic_vector (1 downto 0);		 
		 data_in : in std_logic_vector (7 downto 0);
		 data_mem : out std_logic_vector (31 downto 0);
		 rw  : out std_logic 
		);
 END COMPONENT;
 
  -- COMPONENT converter_bus_32to8  
	-- port (
		-- CLK      : in  std_logic; 
		-- RST      : in  std_logic; 
		-- rdy_data32bit : in std_logic;
		-- data32bit : in std_logic_vector(31 downto 0);
		-- data_out  : out std_logic_vector(7 downto 0); 
		-- rdy_data8bit : out std_logic;
		-- end_convert_32to8 : out std_logic
		-- );
	-- END COMPONENT;
	
	COMPONENT package_formation
	PORT (
	addr_mem : OUT STD_LOGIC_VECTOR(15 DOWNTO 0);
	amount_data_transmitted : IN STD_LOGIC_VECTOR(15 DOWNTO 0);
	clk : IN STD_LOGIC;
	data_from_mem : IN STD_LOGIC_VECTOR(31 DOWNTO 0);
	headline : IN STD_LOGIC_VECTOR(63 DOWNTO 0);
	out_data : OUT STD_LOGIC_VECTOR(7 DOWNTO 0);
	ready : OUT STD_LOGIC;
	RST : IN STD_LOGIC;
	start : IN STD_LOGIC;
	wr_fifo : OUT STD_LOGIC
	);	
	END COMPONENT;
	
	COMPONENT generator_data 
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
END COMPONENT;
 

begin


	rx_dcfifo : dcfifo
		GENERIC MAP (
		intended_device_family => "Cyclone II",
		lpm_numwords => 4095,
		lpm_showahead => "ON",
		lpm_type => "dcfifo",
		lpm_width => 8,
		lpm_widthu => 12,
		overflow_checking => "ON",
		rdsync_delaypipe => 4,
		underflow_checking => "ON",
		use_eab => "ON",
		wrsync_delaypipe => 4
	)
	PORT MAP (
		wrclk => rx_fifo_wrclk, -- USB2232 side
		rdreq => rx_fifo_rdreq,
		rdclk => rx_fifo_rdclk, -- Avalon Bus side
		wrreq => rx_fifo_wrreq,
		data => rx_fifo_data,
		rdempty => rx_fifo_rdempty,
		wrfull => rx_fifo_wrfull,
		q => rx_fifo_q,
		rdusedw => rx_fifo_rdusedw
	);
	
	tx_dcfifo : dcfifo
	GENERIC MAP (
		intended_device_family => "Cyclone II",
		lpm_numwords => 4095,
		lpm_showahead => "ON",
		lpm_type => "dcfifo",
		lpm_width => 8,
		lpm_widthu => 12,
		overflow_checking => "ON",
		rdsync_delaypipe => 4,
		underflow_checking => "ON",
		use_eab => "ON",
		wrsync_delaypipe => 4
	)
	PORT MAP (
		wrclk => tx_fifo_wrclk,  -- Avalon Bus side
		rdreq => tx_fifo_rdreq,
		rdclk => tx_fifo_rdclk,  -- USB2232 side
		wrreq => tx_fifo_wrreq,
		data => tx_fifo_data,
		rdempty => tx_fifo_rdempty,
		wrfull => tx_fifo_wrfull,
		q => tx_fifo_q,
		wrusedw => tx_fifo_wrusedw
	);
	
	

	-- USB2232 side
	rx_fifo_wrclk <= usb_clock;
	tx_fifo_rdclk <= usb_clock;
		
	ft2232_tx_please <= '1' when usb_txe_n = '0' and tx_fifo_rdempty = '0' and ft2232_wait = 1 else '0';
	ft2232_rx_please <= '1' when usb_rxf_n = '0' and rx_fifo_wrfull = '0' else '0';
	
	ft2232_tx_fifo_read <= '1' when ft2232_tx_please = '1' else '0';
	ft2232_rx_fifo_write <= '1' when ft2232_bus_oe_mode > 1 and ft2232_rx_please = '1' and ft2232_tx_please = '0' else '0';

	tx_fifo_rdreq <= ft2232_tx_fifo_read;
	rx_fifo_wrreq <= ft2232_rx_fifo_write;
	
	usb_rd_n <= '0' when ft2232_rx_fifo_write = '1' else '1';
	usb_wr_n <= '0' when ft2232_tx_fifo_read = '1' else '1';
	usb_oe_n <= '0' when ft2232_bus_oe_mode > 0 else '1';
	usb_data <= tx_fifo_q when ft2232_bus_oe_mode = 0 else (others => 'Z');
	rx_fifo_data <= usb_data when ft2232_bus_oe_mode > 0 and usb_rxf_n = '0';
	
	
	
	
	-- Handle FIFOs to USB2232 in synchronous mode
	process (usb_clock)
	begin
	
		if usb_clock'event and usb_clock = '1' then

			-- Bias TX over RX
			if (ft2232_tx_please = '1' or ft2232_rx_please = '0') then

				ft2232_bus_oe_mode <= 0;
				
				if (usb_txe_n = '0' and tx_fifo_rdempty = '0') then
					ft2232_wait <= ft2232_wait + 1;
				else
					ft2232_wait <= 0;
				end if;
				
			elsif (ft2232_rx_please = '1') then
		
				ft2232_wait <= 0;
				
				-- Handle bus turn-around. Negate OE (and for atleast 1 clock)
				if (ft2232_bus_oe_mode < 3) then		
					ft2232_bus_oe_mode <= ft2232_bus_oe_mode + 1;
				end if;

			end if;

		end if;		
	
	end process;
	
	rx_fifo_rdclk <= clk;
	tx_fifo_wrclk <= clk;
	RST <= '0';
	start <= reg_start;
	

	rx_fifo_rdreq <= reg_load_config or reg_load_data or load_others;
		
	--- loopback---------------------------------------	
	-- rx_fifo_rdreq <= not (rx_fifo_rdempty);
	-- tx_fifo_data <= rx_fifo_q;
	-- tx_fifo_wrreq <= rx_fifo_valid;	
		
	-- process (clk)
	-- begin	
	-- if clk'event and clk = '1' then	
	       -- rx_fifo_valid <= rx_fifo_rdreq;  
	-- end if;	
	-- end process;
	--- loopback---------------------------------------
	
--------------------------------------------------------------------------
 clocked_proc : PROCESS ( all )
 --------------------------------------------------------------------------
 BEGIN
 IF (RST = '1') THEN
	current_state <= full_RX_FIFO_st;
 -- Default Reset Values
	reg_start <= '0';
	reg_addr_config <= (others => '0');
	reg_addr_data <= (others => '0');

	--load_config <= '0';
	reg_start <= '0';
	reg_load_config <= '0';
	reg_load_data <= '0';
	--rx_fifo_rdreq <= '0';
	reg_aclr_rx_fifo <= '0';
	--load_others	<= '0';
	reg_start_load_others <= '0';
		
 ELSIF (CLK'EVENT AND CLK = '1') THEN
 current_state <= nxt_state;

 -- Combined Actions
 CASE current_state IS
 
 WHEN full_RX_FIFO_st => 
 --reg_start_load_others <= '0';
	IF (r_rise_rdfull = '1' ) THEN
		reg_start <= '0';
		reg_addr_config <= (others => '0');
		reg_addr_data <= (others => '0');
		reg_load_config <= '0';
		--rx_fifo_rdreq <= '0';
		reg_load_data <= '0';
	else	
	reg_load_config <= '0';	
	END IF;
	
 WHEN read_FIFO_st =>
	reg_load_config <= '1';
	reg_addr_config <= std_logic_vector (unsigned(reg_addr_config) + 1);  -- reg_addr_config + "00001";
	
  WHEN read_FIFO_st0 =>
	reg_load_config <= '0';
	reg_load_data <= '1';
	reg_addr_data <= std_logic_vector (unsigned(reg_addr_data) + 1);   --reg_addr_data + "00000001";		
	
 WHEN start_work_moduls_st => 
	reg_start <= '1';
	reg_load_data <= '0';
	reg_start_load_others <= '1';
	--reg_aclr_rx_fifo <= '1';
	
 WHEN end_work_moduls_st =>
	reg_start <= '0';
	reg_start_load_others <= '0';
	--reg_aclr_rx_fifo <= '0';

WHEN OTHERS => NULL;
END CASE;
END IF;

END PROCESS clocked_proc;
	
--------------------------------------------------------------------------
 nextstate_proc : PROCESS ( all )
 --------------------------------------------------------------------------
 BEGIN
 CASE current_state IS
 WHEN full_RX_FIFO_st =>  
 IF (r_rise_rdfull = '1') THEN
	nxt_state <= read_FIFO_st; --
 ELSE
	nxt_state <= full_RX_FIFO_st;
 END IF;

 WHEN read_FIFO_st =>
	if reg_addr_config = num_conf_data then  
		nxt_state  <= read_FIFO_st0;
	else
		nxt_state  <= read_FIFO_st;
	end if;
	
 WHEN read_FIFO_st0 =>
	--if reg_addr_data = number_values  then  0000000000001000
	if reg_addr_data = num_ram_data  then  --10010110
		nxt_state  <= start_work_moduls_st;
	else
		nxt_state  <= read_FIFO_st0;
	end if;
	
 WHEN start_work_moduls_st => 
	nxt_state <= end_work_moduls_st; --s0; 

 WHEN end_work_moduls_st => 
	if r_rise_rdempty = '1' then
		nxt_state  <= full_RX_FIFO_st;
	else
		nxt_state  <= end_work_moduls_st;
	end if;

-- WHEN pause =>	
--	nxt_state  <= full_RX_FIFO_st;
	
 WHEN OTHERS => nxt_state <= full_RX_FIFO_st;
 END CASE;

 END PROCESS nextstate_proc;		
	
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
 process (all)
 begin
if(RST = '1')  then
config_array <= (others=> (others=>'0'));
elsif(rising_edge(CLK)) then 
	if  reg_load_config = '1' then
		for i in 0 to (config_array'LENGTH - 1) loop
			if reg_addr_config = std_logic_vector(to_unsigned(i, reg_addr_config'LENGTH)) then
				config_array(i) <= rx_fifo_q;
			end if;	
		end loop;		
	end if;
end if;
end process;


p_edge_detector_rdempty : process(all)
begin
  IF (RST = '1') THEN
    r_rise_rdempty       <= '0';
    r_fall_rdempty       <= '0';
    p_input_rdempty      <= (others=>'0');
  elsif(rising_edge(clk)) then
    r_rise_rdempty       <= not p_input_rdempty(2) and p_input_rdempty(1);
    r_fall_rdempty       <= not p_input_rdempty(1) and p_input_rdempty(2);
    p_input_rdempty      <= rx_fifo_rdempty&p_input_rdempty(0 to p_input_rdempty'length-2);
  end if;
end process p_edge_detector_rdempty;


p_edge_detector_rdfull : process(all)
begin
  IF (RST = '1') THEN
    r_rise_rdfull       <= '0';
    r_fall_rdfull       <= '0';
    p_input_rdfull      <= (others=>'0');
  elsif(rising_edge(clk)) then
    r_rise_rdfull       <= not p_input_rdfull(2) and p_input_rdfull(1);
    r_fall_rdfull       <= not p_input_rdfull(1) and p_input_rdfull(2);
    p_input_rdfull      <= rx_fifo_wrfull&p_input_rdfull(0 to p_input_rdfull'length-2);
  end if;
end process p_edge_detector_rdfull;


p_edge_ready_buff : process(all)
begin
  IF (RST = '1') THEN
    r_rise_ready_buff       <= '0';
    r_fall_ready_buff       <= '0';
    p_input_ready_buff      <= (others=>'0');
  elsif(rising_edge(clk)) then
    r_rise_ready_buff       <= not p_input_ready_buff(2) and p_input_ready_buff(1);
    r_fall_ready_buff       <= not p_input_ready_buff(1) and p_input_ready_buff(2);
    p_input_ready_buff      <= reg_ready_buff&p_input_ready_buff(0 to p_input_ready_buff'length-2);
  end if;
end process p_edge_ready_buff;


 process (all)
begin
if(RST = '1')  then
  load_others <= '0';
elsif(rising_edge(CLK)) then
	if reg_start_load_others = '1' then   
		load_others <= '1';
	elsif r_rise_rdempty = '1' then 
		load_others <= '0';
	end if;	
end if;
end process; 
	
	
	double_buffer_inst  : double_buffer
generic map (
    DATA    => 32,
    ADDR    => 8
)
port map (

	Clk     	=> CLK,
	rst			=> RST,
	input_data	=> reg_mux_data,
	wr 			=> reg_mux_wr,
	ready_buff  => reg_ready_buff,
	
    -- Port B
    b_clk   	=> CLK,
    b_addr  	=> reg_addr_mem(7 downto 0),--(others => '0'),
    b_dout   	=> reg_data_from_mem --reg_b_dout
);

 reg_mux_wr <= reg_wr when (config_array(0)(0) = '1') else reg_wr_test_generator;
 reg_mux_data <= reg_data_mem when (config_array(0)(0) = '1') else reg_data_out_test_generator;


	
		 converter_bus_8to32_inst :  converter_bus_8to32 
	port map (
		 clk => CLK, --
		 RST => RST, --
		 load => reg_load_data, --
		 addr_32bit => reg_addr_32bit,
		 rst_addr_32bit => reg_start,
		 addr_data_8bit => reg_addr_data(1 downto 0),	--	 
		 data_in => rx_fifo_q, --
		 data_mem => reg_data_mem,  --32bits
		 rw  => reg_wr 
		);
		
		
		
		 -- converter_bus_32to8_inst :  converter_bus_32to8
	-- port map (
		-- CLK => CLK, --
		-- RST => RST, --
		-- rdy_data32bit => reg_rdy_data32bit,
		-- data32bit => reg_data32bit,
		-- data_out  => reg_outdata_converter_bus_32to8,
		-- rdy_data8bit => reg_rdy_data8bit,
		-- end_convert_32to8 => reg_end_convert_32to8
		-- );
		
	package_formation_inst : package_formation
	PORT MAP (
-- list connections between master ports and signals
	addr_mem => reg_addr_mem,
	amount_data_transmitted => x"00FF",
	clk => clk,
	data_from_mem => reg_data_from_mem,
	headline =>  x"1122334455667788",
	out_data => reg_out_data_package_formation,
	ready => open, --ready,
	RST => RST,
	start => r_rise_ready_buff, --reg_ready_buff,
	wr_fifo => reg_wr_fifo
	);
		

tx_fifo_wrreq <= reg_wr_fifo;
tx_fifo_data <= reg_out_data_package_formation;



	generator_data_inst : generator_data 
 generic map (
    DATA   => 32
			)
 port map (
	Clk 	=> clk,
	rst 	=> RST,
	en  	=> '1',--config_array(0)(1),
	timer_data => "0000000000000010",
	wr => reg_wr_test_generator,
	data_out => reg_data_out_test_generator
		);

-- for test config_array


process (clk,rst)
begin
if (rst = '1') then
s <=  (others => '0');
   elsif (CLK'event and CLK ='1') then     
for i in 0 to ((config_array'LENGTH - 1)/2) loop    
	 if ( config_array(i) < config_array(i + ((config_array'LENGTH - 1)/2)) ) then 
       s(i) <= '1';
    else 
       s(i) <= '0';
    end if;
end loop;
end if; 
end process;

			 
end rtl;