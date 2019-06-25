----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11.04.2019 12:34:53
-- Design Name: 
-- Module Name: HMCAD1511_v2_00 - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_unsigned.ALL;

library work;
use work.lvds_deserializer;
use work.high_speed_clock_to_serdes;
--use work.adc1_ila;
--use work.adc1_icon;
--use work.adc1_vio;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
library UNISIM;
use UNISIM.VComponents.all;

entity HMCAD1511_v2_00 is
    generic (
      C_IDELAY_VALUE        : integer := 16;
      C_IODELAY_FIXED       : boolean := FALSE
    );
    Port (
      LCLKp                 : in std_logic;
      LCLKn                 : in std_logic;
      DxXAp                 : in std_logic_vector(3 downto 0);
      DxXAn                 : in std_logic_vector(3 downto 0);
      DxXBp                 : in std_logic_vector(3 downto 0);
      DxXBn                 : in std_logic_vector(3 downto 0);
      CAL_DUAL_PATTERN      : in std_logic_vector(15 downto 0);
      CAL                   : in std_logic;
      CAL_DONE              : out std_logic;
      CLK                   : in std_logic;
      ARESET                : in std_logic;

      M_STRM_VALID          : out std_logic;
      M_STRM_DATA           : out std_logic_vector(63 downto 0);
      DIVCLK_OUT            : out std_logic
    );
end HMCAD1511_v2_00;

architecture Behavioral of HMCAD1511_v2_00 is
    type   adc_data is array(3 downto 0) of std_logic_vector(7 downto 0);
    signal adc_data_a_8bit                  : adc_data;
    signal adc_data_b_8bit                  : adc_data;
    signal lclk_ibufg_out                   : std_logic;
    signal IOCLK0_0                         : std_logic;
    signal IOCLK1_0                         : std_logic;
    signal serdesstrobe_0                   : std_logic;
    signal IOCLK0_1                         : std_logic;
    signal IOCLK1_1                         : std_logic;
    signal serdesstrobe_1                   : std_logic;
    signal div_clk_bufg_0                   : std_logic;
    signal div_clk_bufg                   : std_logic;
    signal pll_clkfbout                     : std_logic;
    signal pll_clkfbout_bufg                : std_logic;
    signal pll_clkfbin                      : std_logic;
    signal pll_locked                       : std_logic;
    signal pll_clkout0_125MHz               : std_logic;
    signal lvds_deserializers_busy_vec      : std_logic_vector(7 downto 0);
    signal lvds_deserializers_busy          : std_logic;
    signal lvds_deserializers_busy_d        : std_logic;
    signal lvds_deserializers_busy_edge     : std_logic;
    signal lvds_deserializers_busy_fall     : std_logic;
    signal cal_in                           : std_logic;
    signal cal_in_d1                        : std_logic;
    signal cal_in_d2                        : std_logic;
    signal cal_in_d3                        : std_logic;
    signal cal_in_sync                      : std_logic;
    signal data_out_valid                   : std_logic;
    signal data_out_valid_d1                : std_logic;
    signal data_out_valid_d2                : std_logic;
    signal data_out_valid_d3                : std_logic;
    signal cal_done_sync                    : std_logic;
    signal iodelay_clk                      : std_logic;
    signal iodelay_inc                      : std_logic;
    signal iodelay_ce                       : std_logic;
    signal iodelay_cal                      : std_logic;
    signal iodelay_rst                      : std_logic;
    signal iodelay_calib                    : std_logic;
    signal adc1_ila_control                 : std_logic_vector(35 downto 0);
    signal adc1_vio_control                 : std_logic_vector(35 downto 0);
    signal vio_calib_vector                 : std_logic_vector(3 downto 0);
    signal vio_calib_vector_d               : std_logic_vector(3 downto 0);
    signal data_out                         : std_logic_vector(63 downto 0);
    

begin
DIVCLK_OUT <= div_clk_bufg;
M_STRM_VALID <= data_out_valid;
CAL_DONE <= cal_done_sync;

--cal_capture_proc :
--   process(CLK)
--   begin
--     if ARESET = '1' then
--      cal_in <= '0';
--      data_out_valid_d1 <= '0';
--      data_out_valid_d2 <= '0';
--      data_out_valid_d3 <= '0';
--     elsif rising_edge(clk) then
--       if (CAL = '1') then
--         cal_in <= '1';
--       elsif data_out_valid_d2 = '1' then
--         cal_in <= '0';
--       end if;
--       data_out_valid_d1 <= data_out_valid;
--       data_out_valid_d2 <= data_out_valid_d1;
--       data_out_valid_d3 <= data_out_valid_d2;
--       cal_done_sync <= (not data_out_valid_d3) and data_out_valid_d2;
--     end if;
--   end process;
--
--sync_cal_in_proc :
--   process(div_clk_bufg)
--   begin
--     if ARESET = '1' then
--      cal_in_d1 <= '0';
--      cal_in_d2 <= '0';
--      cal_in_d3 <= '0';
--     elsif rising_edge(div_clk_bufg) then
--      cal_in_d1 <= cal_in;
--      cal_in_d2 <= cal_in_d1;
--      cal_in_d3 <= cal_in_d2;
--      cal_in_sync <= (not cal_in_d3) and cal_in_d2;
--     end if;
--   end process;


lvds_deserializers_busy <=  lvds_deserializers_busy_vec(7) or lvds_deserializers_busy_vec(6) or 
                            lvds_deserializers_busy_vec(5) or lvds_deserializers_busy_vec(4) or 
                            lvds_deserializers_busy_vec(3) or lvds_deserializers_busy_vec(2) or 
                            lvds_deserializers_busy_vec(1) or lvds_deserializers_busy_vec(0);
valid_process :
   process(div_clk_bufg, ARESET)
   begin
     if (ARESET = '1') then
       data_out_valid <= '0';
       lvds_deserializers_busy_edge <= '0';
       lvds_deserializers_busy_fall <= '0';
     elsif rising_edge(div_clk_bufg) then
       lvds_deserializers_busy_d <= lvds_deserializers_busy;
       lvds_deserializers_busy_edge <= not lvds_deserializers_busy_d and lvds_deserializers_busy;
       lvds_deserializers_busy_fall <= not lvds_deserializers_busy and lvds_deserializers_busy_d;
         if lvds_deserializers_busy_edge = '1' then
           data_out_valid <= '0';
         elsif (lvds_deserializers_busy_fall = '1') then
           data_out_valid <= '1';
         end if;
       end if;
   end process;

IBUFGDS_LCLK_inst : IBUFGDS
   generic map (
      IBUF_LOW_PWR => TRUE, -- Low power (TRUE) vs. performance (FALSE) setting for referenced I/O standards
      IOSTANDARD => "DEFAULT")
   port map (
      O => lclk_ibufg_out,  -- Clock buffer output
      I => LCLKp,  -- Diff_p clock buffer input
      IB => LCLKn -- Diff_n clock buffer input
   );

high_speed_clock_to_serdes_inst : entity high_speed_clock_to_serdes
    Port map( 
        in_clk_from_bufg    => lclk_ibufg_out,
        div_clk_bufg        => div_clk_bufg,
        serdesclk0          => IOCLK0_0,
        serdesclk1          => IOCLK1_0,
        serdesstrobe        => serdesstrobe_0
    );

adc_deserializer_gen : for i in 0 to 3 generate
lvds_deserializer_a_inst: entity lvds_deserializer
    generic map(
      C_IDELAY_VALUE        => C_IDELAY_VALUE,
      C_IODELAY_FIXED       => C_IODELAY_FIXED
    )
    Port map( 
      data_in_p             => DxXAp(i),
      data_in_n             => DxXAn(i),
      ioclk0          		=> IOCLK0_0,
      ioclk1          		=> IOCLK1_0,
      clkdiv         		=> div_clk_bufg,
      serdesstrobe   		=> serdesstrobe_0,
      
      iodelay_clk           => iodelay_clk,
      iodelay_inc           => iodelay_inc,
      iodelay_ce            => iodelay_ce ,
      iodelay_cal           => iodelay_cal,
      iodelay_rst           => iodelay_rst,
      
      cal_dual_pattern      => cal_dual_pattern,
      data_8bit_out         => adc_data_a_8bit(i),
	  start_calib           => CAL,
      calib_busy            => lvds_deserializers_busy_vec(2*i),
	  rst					=> ARESET
    );

lvds_deserializer_b_inst: entity lvds_deserializer
    generic map(
      C_IDELAY_VALUE        => C_IDELAY_VALUE,
      C_IODELAY_FIXED       => C_IODELAY_FIXED
    )
    Port map( 
      data_in_p             => DxXBp(i),
      data_in_n             => DxXBn(i),
      ioclk0         		=> IOCLK0_0,
      ioclk1         		=> IOCLK1_0,
      clkdiv         		=> div_clk_bufg,
      serdesstrobe   		=> serdesstrobe_0,
      
      iodelay_clk           => iodelay_clk,
      iodelay_inc           => iodelay_inc,
      iodelay_ce            => iodelay_ce ,
      iodelay_cal           => iodelay_cal,
      iodelay_rst           => iodelay_rst,
      
      cal_dual_pattern      => cal_dual_pattern,
      data_8bit_out         => adc_data_b_8bit(i),
      start_calib           => CAL,
      calib_busy            => lvds_deserializers_busy_vec(2*i + 1),
      rst                   => ARESET
    );
end generate;

    M_STRM_DATA <= data_out;
    data_out    <= adc_data_b_8bit(3) & adc_data_a_8bit(3) & 
                   adc_data_b_8bit(2) & adc_data_a_8bit(2) & 
                   adc_data_b_8bit(1) & adc_data_a_8bit(1) & 
                   adc_data_b_8bit(0) & adc_data_a_8bit(0);

--adc1_ila_inst : ENTITY adc1_ila
--  port map(
--    CONTROL     => adc1_ila_control,
--    CLK         => div_clk_bufg,
--    DATA        => data_out & data_out_valid & iodelay_inc & iodelay_rst & iodelay_ce & iodelay_cal,
--    TRIG0       => iodelay_inc & iodelay_rst & iodelay_ce & iodelay_cal
--    );
--
--adc1_icon_inst : ENTITY adc1_icon
--  port map(
--    CONTROL0    => adc1_ila_control,
--    CONTROL1    => adc1_vio_control
--    );
--
--adc1_vio_inst : ENTITY adc1_vio
--  port map (
--    CONTROL     => adc1_vio_control,
--    CLK         => div_clk_bufg,
--    SYNC_OUT    => vio_calib_vector
--    );
--
--sync_proc : process(div_clk_bufg)
--begin
--  if rising_edge(div_clk_bufg) then
--    vio_calib_vector_d(3 downto 0) <= vio_calib_vector(3 downto 0);
--    iodelay_inc <= vio_calib_vector_d(3);
--    iodelay_rst <= vio_calib_vector_d(2);
--    iodelay_ce  <= (not vio_calib_vector_d(1)) and vio_calib_vector(1);
--    iodelay_cal <= (not vio_calib_vector_d(0)) and vio_calib_vector(0);
--  end if;
--end process;



iodelay_clk <= div_clk_bufg;

end Behavioral;
