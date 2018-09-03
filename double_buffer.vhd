library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use IEEE.NUMERIC_STD.ALL;

entity double_buffer is
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
end double_buffer;

architecture rtl of double_buffer is
signal r_rise                  : std_logic :='0';
signal r_fall                  : std_logic :='0';
signal p_input                 : std_logic_vector(0 to 2):= (others => '0'); -- input pipe

signal r_rise1                  : std_logic :='0';
signal r_fall1                  : std_logic :='0';
signal p_input1                 : std_logic_vector(0 to 2):= (others => '0'); -- input pipe

--signal reg_position, reg_position_shift :  STD_LOGIC_VECTOR (31 downto 0);
signal reg_count  :   std_logic_vector(ADDR-1 downto 0);
--signal timer_reg : unsigned (DATA-1 downto 0);
signal reg_wr :  STD_LOGIC;
--signal comp_position :  STD_LOGIC;
--signal reg_cnt_ena_QuadratureDecoder :  STD_LOGIC;
signal reg_buff_full, reg_buff_empty, sel_buff, reg_empty, reg_sel_out_buff :  STD_LOGIC;
signal reg_wr_buffA, reg_wr_buffB :  STD_LOGIC;
signal reg_mux_dout, reg_doutA, reg_doutB : std_logic_vector(DATA-1 downto 0);
signal reg_empty_buffA, reg_empty_buffB  :  STD_LOGIC;
signal reg_b_addr, reg_b_addrA, reg_b_addrB : std_logic_vector(ADDR-1 downto 0);

COMPONENT bram_tdp
generic (
    DATA    : integer := 32;
    ADDR    : integer := 12
);
port (
    -- Port A
    a_clk   : in  std_logic;
    a_wr    : in  std_logic;
    a_addr  : in  std_logic_vector(ADDR-1 downto 0);
    a_din   : in  std_logic_vector(DATA-1 downto 0);
    a_dout  : out std_logic_vector(DATA-1 downto 0);

    -- Port B
    b_clk   : in  std_logic;
    b_wr    : in  std_logic;
    b_addr  : in  std_logic_vector(ADDR-1 downto 0);
    b_din   : in  std_logic_vector(DATA-1 downto 0);
    b_dout  : out std_logic_vector(DATA-1 downto 0)
);
END COMPONENT;


begin

reg_wr <= wr;

		 
bram_tdpA_inst : bram_tdp
generic map (
    DATA     => DATA,--32,
    ADDR     => ADDR
	)
port map(
    -- Port A
    a_clk   => Clk,
    a_wr    => reg_wr_buffA, --reg_wr and comp_position,
    a_addr  => reg_count,
    a_din   => input_data,
    a_dout  => open,

    -- Port B
    b_clk   => b_clk,
    b_wr    => '0', 
    b_addr  => reg_b_addrA,
    b_din   => (others => '0'),
    b_dout  => reg_doutA
);

bram_tdpB_inst : bram_tdp
generic map (
    DATA     => DATA,--32,
    ADDR     => ADDR
	)
port map(
    -- Port A
    a_clk   => Clk,
    a_wr    => reg_wr_buffB, --reg_wr and comp_position,
    a_addr  => reg_count,
    a_din   => input_data,
    a_dout  => open,

    -- Port B
    b_clk   => b_clk,
    b_wr    => '0', 
    b_addr  => reg_b_addrB,
    b_din   => (others => '0'),
    b_dout  => reg_doutB
);

p_edge_detector0 : process(all)
begin
  if (rst = '1') then
    r_rise       <= '0';
    r_fall       <= '0';
    p_input      <= (others=>'0');
  elsif(rising_edge(Clk)) then
    r_rise       <= not p_input(2) and p_input(1);
    r_fall       <= not p_input(1) and p_input(2);
    p_input      <= reg_buff_full&p_input(0 to p_input'length-2);
  end if;
end process p_edge_detector0;

p_edge_detector1 : process(all)
begin
  if (rst = '1') then
    r_rise1       <= '0';
    r_fall1       <= '0';
    p_input1      <= (others=>'0');
  elsif(rising_edge(Clk)) then
    r_rise1       <= not p_input1(2) and p_input1(1);
    r_fall1       <= not p_input(1) and p_input1(2);
    p_input1      <= reg_buff_empty&p_input1(0 to p_input1'length-2);
  end if;
end process p_edge_detector1;


process(all)
begin
	if (rst = '1') then
		reg_count  <= (others=>'0');
	elsif(rising_edge(Clk)) then
		if(reg_wr = '1') then
			reg_count <= reg_count + 1;
		end if;
	end if;
end process;

reg_buff_full <= '1' WHEN reg_count = std_logic_vector( to_unsigned(natural((2**ADDR)-2), reg_count'length )) ELSE '0';
reg_buff_empty <= '1' WHEN reg_b_addr = std_logic_vector( to_unsigned(natural((2**ADDR)-2), reg_b_addr'length )) ELSE '0';

---------------------direction double buffer -------------------------

b_dout <= reg_mux_dout;

process(all)
begin
	if (rst = '1') then
		sel_buff  <= '1';
		reg_empty <= '0';
		ready_buff <= '0';
	elsif(rising_edge(Clk)) then
		reg_b_addr <= b_addr;
		if(r_rise = '1') then
			sel_buff <= not(sel_buff);
			ready_buff <= '1';
		elsif (r_rise1 = '1')  then	
			reg_empty <= not(reg_empty);
			ready_buff <= '0';
		end if;
	end if;
end process;

process(all)
begin
	if (rst = '1') then
		reg_wr_buffA <= '0';
		reg_wr_buffB <= '0';
	elsif(rising_edge(Clk)) then
		if(sel_buff = '1' and reg_empty = '0') then
			reg_wr_buffA <= reg_wr;  
			reg_wr_buffB <= '0';
		elsif (sel_buff = '0' and reg_empty = '1') then
			reg_wr_buffB <= reg_wr; 
			reg_wr_buffA <= '0';
		
		elsif(sel_buff = '1' and reg_empty = '1') then
			reg_wr_buffA <= reg_wr;  
			reg_wr_buffB <= '0';
		elsif (sel_buff = '0' and reg_empty = '0') then
			reg_wr_buffB <= reg_wr; 
			reg_wr_buffA <= '0';
		

		end if;
	end if;
end process;

--direction mux output buff data

process(all)
begin
	if (rst = '1') then
		reg_mux_dout  <= (others=>'0');
	elsif(rising_edge(Clk)) then
	
		if(sel_buff = '0') then
			reg_mux_dout <= reg_doutA;
			reg_b_addrA <= b_addr;
			reg_b_addrB <=(others=>'0');
		else
			reg_b_addrB <= b_addr;
			reg_b_addrA <=(others=>'0');
			reg_mux_dout <= reg_doutB;
		end if;

--		if reg_empty_buffA <= '0' then
--			reg_mux_dout <= reg_doutA;
--		elsif reg_empty_buffB <= '0' then
--			reg_mux_dout <= reg_doutB;
--		end if;	

	end if;
end process;



end rtl;