----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 30.05.2019 09:43:32
-- Design Name: 
-- Module Name: ADC1511_Dual_1GHzX2_Top - Behavioral
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
use work.HMCAD1511_v2_00;
use work.clock_generator;
use work.spi_adc_250x4_master;
use work.fifo_sream;
use work.trigger_capture;
use work.data_capture_module;
use work.QuadSPI_adc_250x4_module;
--use work.ila;
--use work.icon;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
library UNISIM;
use UNISIM.VComponents.all;

entity ADC1511_Dual_1GHzX2_Top is
    Port ( 
        adc1_lck_p              : in std_logic;
        adc1_lck_n              : in std_logic;
        adc1_dx_a_p             : in std_logic_vector(3 downto 0);
        adc1_dx_a_n             : in std_logic_vector(3 downto 0);
        adc1_dx_b_p             : in std_logic_vector(3 downto 0);
        adc1_dx_b_n             : in std_logic_vector(3 downto 0);
    
        adc2_lck_p              : in std_logic;
        adc2_lck_n              : in std_logic;
        adc2_dx_a_p             : in std_logic_vector(3 downto 0);
        adc2_dx_a_n             : in std_logic_vector(3 downto 0);
        adc2_dx_b_p             : in std_logic_vector(3 downto 0);
        adc2_dx_b_n             : in std_logic_vector(3 downto 0);
        
        adc1_data_valid         : out std_logic;
        adc2_data_valid         : out std_logic;
        adc_calib_done          : out std_logic;
        main_pll_lock           : out std_logic;

        pulse_out               : out std_logic;

        in_clk_50MHz            : in std_logic;

        spifi_cs                : in std_logic;
        spifi_sck               : in std_logic;
        spifi_miso              : inout std_logic;
        spifi_mosi              : inout std_logic;
        spifi_sio2              : inout std_logic;
        spifi_sio3              : inout std_logic;
        
        fpga_sck                : in std_logic;
        fpga_cs                 : in std_logic;
        fpga_miso               : out std_logic;
        fpga_mosi               : in std_logic
    );
end ADC1511_Dual_1GHzX2_Top;

architecture Behavioral of ADC1511_Dual_1GHzX2_Top is
    signal adc1_clk_div8        : std_logic;
    signal adc1_fifo_rstn        : std_logic;
    signal adc1_fifo_m_stream_valid : std_logic;
    signal adc1_receiver_valid  : std_logic;
    signal adc1_data_out        : std_logic_vector(63 downto 0);
    signal adc2_clk_div8        : std_logic;
    signal adc2_fifo_rstn        : std_logic;
    signal adc2_fifo_m_stream_valid : std_logic;
    signal adc2_receiver_valid  : std_logic;
    signal adc2_data_out        : std_logic_vector(63 downto 0);
    
    signal MISO_I               : std_logic;
    signal MISO_O               : std_logic;
    signal MISO_T               : std_logic;
    signal MOSI_I               : std_logic;
    signal MOSI_O               : std_logic;
    signal MOSI_T               : std_logic;
    signal m_fcb_aresetn        : std_logic;
    signal m_fcb_addr           : std_logic_vector(8 - 1 downto 0);
    signal m_fcb_wrdata         : std_logic_vector(16 - 1 downto 0);
    signal m_fcb_wrreq          : std_logic;
    signal m_fcb_wrack          : std_logic;
    signal m_fcb_rddata         : std_logic_vector(16 - 1 downto 0);
    signal m_fcb_rdreq          : std_logic;
    signal m_fcb_rdack          : std_logic;
    
    signal trig_set_up_reg      : std_logic_vector(15 downto 0):= x"7f00";
    signal trig_window_width_reg: std_logic_vector(15 downto 0):= x"0200";
    signal trig_position_reg    : std_logic_vector(15 downto 0):= x"0800";
    signal control_reg          : std_logic_vector(15 downto 0):= (others => '0');
    signal calib_pattern_reg    : std_logic_vector(15 downto 0):= x"55AA";
    signal wr_req_vec           : std_logic_vector(5 downto 0);
    signal reg_address_int      : integer;
    signal adc_calib            : std_logic:= '0';
    signal control_reg_d        : std_logic_vector(15 downto 0);
    signal low_adc_buff_len     : std_logic_vector(15 downto 0);
    
    signal adc1_fifo_m_stream_data   : std_logic_vector(63 downto 0);
    signal adc2_fifo_m_stream_data   : std_logic_vector(63 downto 0);

    signal adc1_ila_control     : std_logic_vector(35 downto 0);
    signal control_1            : std_logic_vector(35 downto 0);
    signal control_2            : std_logic_vector(35 downto 0);
    signal cal                  : std_logic;
    signal infrst_rst_out       : std_logic;
    signal vio_calib_vector     : std_logic_vector(3 downto 0);
    signal vio_calib_vector_d   : std_logic_vector(3 downto 0);
    signal clk_125MHz           : std_logic;
    signal rst                  : std_logic;
    signal ext_trig             : std_logic:= '0';
    signal adc1_trigger_start   : std_logic;
    signal adc2_trigger_start   : std_logic;
    signal adc1_trigger_set_up  : std_logic;
    signal adc1_trigger_set_vec : std_logic_vector(1 downto 0);
    signal adc2_trigger_set_up  : std_logic;
    signal adc2_trigger_set_vec : std_logic_vector(1 downto 0);
    signal adc1_capture_module_rst : std_logic;
    signal adc2_capture_module_rst : std_logic;
    signal trigger_start        : std_logic;
    signal adc2_activ           : std_logic;
    signal spifi_cs_d           : std_logic_vector(2 downto 0);
    signal spifi_cs_up          : std_logic;
    signal adc1_m_strm_data     : std_logic_vector(63 downto 0);
    signal adc1_m_strm_valid    : std_logic;
    signal adc1_m_strm_ready    : std_logic;
    signal adc2_m_strm_data     : std_logic_vector(63 downto 0);
    signal adc2_m_strm_valid    : std_logic;
    signal adc2_m_strm_ready    : std_logic;
    signal adc_receiver_rst     : std_logic:= '0';
    signal pulse_start          : std_logic:= '0';
    signal pulse                : std_logic;
    signal pulse_counter        : std_logic_vector(3 downto 0);

begin

cs_up_proc :
    process(clk_125MHz)
    begin
      if rising_edge(clk_125MHz) then
        spifi_cs_d(0) <= spifi_cs;
        spifi_cs_d(2 downto 1) <= spifi_cs_d(1 downto 0);
      end if;
    end process;

spifi_cs_up <= spifi_cs_d(1) and (not spifi_cs_d(2));

rst <= infrst_rst_out or control_reg(1);

Clock_gen_inst : entity clock_generator
    Port map( 
      clk_in            => in_clk_50MHz,
      rst_in            => '0',
      pll_lock          => main_pll_lock,
      clk_out_125MHz    => clk_125MHz,
      rst_out           => infrst_rst_out
    );

adc_calib_done <= adc1_receiver_valid and adc2_receiver_valid;

adc1_data_receiver : entity HMCAD1511_v2_00
    generic map(
      C_IDELAY_VALUE      => 15,
      C_IODELAY_FIXED     => true
    )
    Port map(
      LCLKp             => adc1_lck_p,
      LCLKn             => adc1_lck_n,
      DxXAp             => adc1_dx_a_p,
      DxXAn             => adc1_dx_a_n,
      DxXBp             => adc1_dx_b_p,
      DxXBn             => adc1_dx_b_n,
      
      CAL_DUAL_PATTERN  => calib_pattern_reg,
      CAL               => adc_calib,
      ARESET            => adc_receiver_rst,
      CAL_DONE          => open,
      CLK               => clk_125MHz,
      DIVCLK_OUT        => adc1_clk_div8,
      M_STRM_VALID      => adc1_receiver_valid,
      M_STRM_DATA       => adc1_data_out
    );

--adc1_fifo_rstn <= not adc_receiver_rst;
--adc2_fifo_rstn <= not adc_receiver_rst;

adc1_stream_fifo_inst : ENTITY fifo_sream
  PORT map(
      m_aclk            => clk_125MHz,
      s_aclk            => adc1_clk_div8,
      s_aresetn         => adc1_receiver_valid,
      s_axis_tvalid     => adc1_receiver_valid,
      s_axis_tready     => open,
      s_axis_tdata      => adc1_data_out,
      m_axis_tvalid     => adc1_fifo_m_stream_valid,
      m_axis_tready     => '1',
      m_axis_tdata      => adc1_fifo_m_stream_data
  );

adc2_data_receiver : entity HMCAD1511_v2_00
    generic map(
      C_IDELAY_VALUE      => 15,
      C_IODELAY_FIXED     => true
    )
    Port map(
      LCLKp             => adc2_lck_p,
      LCLKn             => adc2_lck_n,
      DxXAp             => adc2_dx_a_p,
      DxXAn             => adc2_dx_a_n,
      DxXBp             => adc2_dx_b_p,
      DxXBn             => adc2_dx_b_n,

      CAL_DUAL_PATTERN  => calib_pattern_reg,
      CAL               => adc_calib,
      ARESET            => adc_receiver_rst,
      CAL_DONE          => open,
      CLK               => clk_125MHz,
      DIVCLK_OUT        => adc2_clk_div8,
      M_STRM_VALID      => adc2_receiver_valid,
      M_STRM_DATA       => adc2_data_out
    );

adc2_stream_fifo_inst : ENTITY fifo_sream
  PORT map(
      m_aclk            => clk_125MHz,
      s_aclk            => adc2_clk_div8,
      s_aresetn         => adc2_receiver_valid,
      s_axis_tvalid     => adc2_receiver_valid,
      s_axis_tready     => open,
      s_axis_tdata      => adc2_data_out,
      m_axis_tvalid     => adc2_fifo_m_stream_valid,
      m_axis_tready     => '1',
      m_axis_tdata      => adc2_fifo_m_stream_data
  );
  
adc1_trigger_capture_inst : entity trigger_capture
    generic map(
      c_data_width    => 64
    )
    Port map( 
      clk               => clk_125MHz,
      rst               => rst,
      capture_mode      => trig_set_up_reg(1 downto 0),
      capture_level     => trig_set_up_reg(15 downto 8),
      trigger_set_up    => adc1_trigger_set_up,
      

      data              => adc1_fifo_m_stream_data,     -- входные значения данных от АЦП
      ext_trig          => ext_trig,                    -- внешний триггер
      
      trigger_start     => adc1_trigger_start           -- выходной сигнал управляет модулем захвата данных
    );
    
adc2_trigger_capture_inst : entity trigger_capture
    generic map(
      c_data_width    => 64
    )
    Port map( 
      clk               => clk_125MHz,
      rst               => rst,
      capture_mode      => trig_set_up_reg(1 downto 0),
      capture_level     => trig_set_up_reg(15 downto 8),
      trigger_set_up    => adc2_trigger_set_up,
      

      data              => adc2_fifo_m_stream_data,          -- входные значения данных от АЦП
      ext_trig          => ext_trig,                    -- внешний триггер
      
      trigger_start     => adc2_trigger_start           -- выходной сигнал управляет модулем захвата данных
    );
    
trigger_set_up_process :
  process(clk_125MHz)
  begin
    if rising_edge(clk_125MHz) then
      if (control_reg(4) = '1') then
        case trig_set_up_reg(3 downto 2) is
          when b"00" => 
            adc1_trigger_set_vec(0) <= '1';
            adc2_trigger_set_vec(0) <= '1';
          when b"10" => 
            adc2_trigger_set_vec(0) <= '1';
          when b"01" => 
            adc1_trigger_set_vec(0) <= '1';
          when others =>
            null;
        end case;
      else
        adc1_trigger_set_vec(1) <= adc1_trigger_set_vec(0);
        adc2_trigger_set_vec(1) <= adc2_trigger_set_vec(0);
        adc1_trigger_set_vec(0) <= '0';
        adc2_trigger_set_vec(0) <= '0';
      end if;
    end if;
  end process;

--adc1_trigger_set_up <= control_reg(4) when trig_set_up_reg(3) = '0' else '0';
--adc2_trigger_set_up <= control_reg(4) when trig_set_up_reg(2) = '0' else '0';

adc1_trigger_set_up <= (not adc1_trigger_set_vec(1)) and adc1_trigger_set_vec(0);
adc2_trigger_set_up <= (not adc2_trigger_set_vec(1)) and adc2_trigger_set_vec(0);

adc1_capture_module_rst <= rst or (spifi_cs_up and (not adc2_activ));

trigger_start <= adc1_trigger_start or adc2_trigger_start;

adc1_stream_data_capture_inst    : entity data_capture_module
    generic map (
      c_max_window_size_width   => 16,
      c_strm_data_width         => 64,
      c_trig_delay              => 0
    )
    Port map(
      clk                   => clk_125MHz,
      rst                   => adc1_capture_module_rst,
      trigger_start         => trigger_start,
      window_size           => trig_window_width_reg,
      trig_position         => trig_position_reg,

      s_strm_data           => adc1_fifo_m_stream_data,
      s_strm_valid          => adc1_fifo_m_stream_valid,
      s_strm_ready          => open,

      m_strm_data           => adc1_m_strm_data,
      m_strm_valid          => adc1_m_strm_valid,
      m_strm_ready          => adc1_m_strm_ready 
    );

adc2_capture_module_rst <= rst or (spifi_cs_up and adc2_activ);

adc2_stream_data_capture_inst    : entity data_capture_module
    generic map (
      c_max_window_size_width   => 16,
      c_strm_data_width         => 64,
      c_trig_delay              => 0
    )
    Port map(
      clk                   => clk_125MHz,
      rst                   => adc2_capture_module_rst,
      trigger_start         => trigger_start,
      window_size           => trig_window_width_reg,
      trig_position         => trig_position_reg,

      s_strm_data           => adc2_fifo_m_stream_data,
      s_strm_valid          => adc2_fifo_m_stream_valid,
      s_strm_ready          => open,

      m_strm_data           => adc2_m_strm_data,
      m_strm_valid          => adc2_m_strm_valid,
      m_strm_ready          => adc2_m_strm_ready 
    );

QuadSPI_adc_250x4_module_inst : entity QuadSPI_adc_250x4_module
    Port map(
      spifi_cs                  => spifi_cs  ,
      spifi_sck                 => spifi_sck ,
      spifi_miso                => spifi_miso,
      spifi_mosi                => spifi_mosi,
      spifi_sio2                => spifi_sio2,
      spifi_sio3                => spifi_sio3,
      
      clk                       => clk_125MHz,
      rst                       => rst,
      
      adc1_s_strm_data          => adc1_m_strm_data,
      adc1_s_strm_valid         => adc1_m_strm_valid,
      adc1_s_strm_ready         => adc1_m_strm_ready,
      adc1_valid                => adc1_data_valid,
      
      adc2_s_strm_data          => adc2_m_strm_data,
      adc2_s_strm_valid         => adc2_m_strm_valid,
      adc2_s_strm_ready         => adc2_m_strm_ready,
      adc2_valid                => adc2_data_valid,

      adc2_activ                => adc2_activ
    );

spi_fcb_master_inst : entity spi_adc_250x4_master
    generic map(
      C_CPHA            => 1,
      C_CPOL            => 1,
      C_LSB_FIRST       => 0
    )
    Port map( 
      SCK               => fpga_sck,
      CS                => fpga_cs,

      MISO_I            => MISO_I,
      MISO_O            => MISO_O,
      MISO_T            => MISO_T,
      MOSI_I            => MOSI_I,
      MOSI_O            => MOSI_O,
      MOSI_T            => MOSI_T,

      m_fcb_clk         => clk_125MHz,
      m_fcb_areset      => infrst_rst_out,
      m_fcb_addr        => m_fcb_addr   ,
      m_fcb_wrdata      => m_fcb_wrdata ,
      m_fcb_wrreq       => m_fcb_wrreq  ,
      m_fcb_wrack       => m_fcb_wrack  ,
      m_fcb_rddata      => m_fcb_rddata ,
      m_fcb_rdreq       => m_fcb_rdreq  ,
      m_fcb_rdack       => m_fcb_rdack  
    );

OBUFT_inst : OBUFT
   generic map (
      DRIVE => 12,
      IOSTANDARD => "DEFAULT",
      SLEW => "SLOW")
   port map (
      O => fpga_miso,     -- Buffer output (connect directly to top-level port)
      I => MISO_O,     -- Buffer input
      T => MISO_T      -- 3-state enable input 
   );

MOSI_I <= fpga_mosi;

-------------------------------------------------
-- управляющие регистры 
-------------------------------------------------
-- процесс записи/чтения регистров управления
reg_address_int <= conv_integer(m_fcb_addr(6 downto 0));

m_fcb_wr_process :
    process(clk_125MHz)
    begin
      if rising_edge(clk_125MHz) then
        if (infrst_rst_out = '1') then
          wr_req_vec <= (others => '0');
          control_reg(15 downto 0) <= (others => '0');
          trig_set_up_reg(15 downto 8) <= x"7f";
          trig_set_up_reg(3 downto 0) <= (others => '0');
          low_adc_buff_len <= x"2004";
          trig_window_width_reg <= x"0200";
          calib_pattern_reg <= x"55AA";
          adc_calib <= '0';
        elsif (m_fcb_wrreq = '1') then
          m_fcb_wrack <= '1';
          case reg_address_int is
            when 0 => 
              wr_req_vec(0) <= '1';
              trig_set_up_reg(15 downto 2) <= m_fcb_wrdata(15 downto 2);
            when 1 => 
              wr_req_vec(1) <= '1';
              trig_window_width_reg <= m_fcb_wrdata;
            when 2 =>
              wr_req_vec(2) <= '1';
              trig_position_reg <= m_fcb_wrdata;
            when 3 =>
              wr_req_vec(3) <= '1';
              trig_set_up_reg(1 downto 0) <= m_fcb_wrdata(3 downto 2);
              control_reg(1 downto 0) <= m_fcb_wrdata(1 downto 0);
              control_reg(7 downto 4) <= m_fcb_wrdata(7 downto 4);
            when 4 =>
              wr_req_vec(4) <= '1';
              calib_pattern_reg <= m_fcb_wrdata;
            when 5 =>
              wr_req_vec(5) <= '1';
              low_adc_buff_len <= m_fcb_wrdata;
            when others =>
          end case;
        else 
          m_fcb_wrack               <= '0';
          wr_req_vec                <= (others => '0');
          control_reg(15 downto 6)  <= (others => '0');
          control_reg(5)            <= control_reg(5);
          control_reg(4 downto 0)   <= (others => '0');
          control_reg_d(7)          <= control_reg(7);
          pulse_start               <= (not control_reg_d(7)) and control_reg(7);
          adc_calib                 <= control_reg(0);
        end if;
      end if;
    end process;
    
adc_receiver_rst    <= control_reg(5);

OBUF_inst : OBUF
   generic map (
      DRIVE => 24,
      IOSTANDARD => "LVCMOS33",
      SLEW => "FAST")
   port map (
      O => pulse_out,     -- Buffer output (connect directly to top-level port)
      I => pulse      -- Buffer input 
   );

pulse_counter_proc :
    process(clk_125MHz)
    begin
      if rising_edge(clk_125MHz) then
        if control_reg(7) = '1' then
          pulse_counter <= (others => '1');
        else
          pulse_counter(3 downto 1) <= pulse_counter(2 downto 0);
          pulse_counter(0) <= '0';
        end if;
        pulse <= pulse_counter(3);
      end if;
    end process;

m_fcb_rd_process :
    process(clk_125MHz)
    begin
      if rising_edge(clk_125MHz) then
        if (infrst_rst_out = '1') then
        elsif (m_fcb_rdreq = '1') then
          m_fcb_rdack <= '1';
          case reg_address_int is
            when 0 => 
              m_fcb_rddata(15 downto 2) <= trig_set_up_reg(15 downto 2);
            when 1 => 
              m_fcb_rddata <= trig_window_width_reg;
            when 2 =>
              m_fcb_rddata <= trig_position_reg;
            when 3 =>
              m_fcb_rddata(1 downto 0) <= control_reg(1 downto 0);
              m_fcb_rddata(3 downto 2) <= trig_set_up_reg(1 downto 0);
              m_fcb_rddata(15 downto 4)<= control_reg(15 downto 4);
            when 4 =>
              m_fcb_rddata <= calib_pattern_reg;
            when 5 => 
              m_fcb_rddata <= low_adc_buff_len;
            when 6 =>
              m_fcb_rddata(0) <= adc_receiver_rst;
              m_fcb_rddata(15 downto 1) <= (others => '0');
            when others =>
          end case;
        else 
          m_fcb_rdack <= '0';
        end if;
      end if;
    end process;

--ila1_inst : ENTITY ila
--  port map(
--    CONTROL => control_1,
--    CLK     => clk_125MHz,
--    DATA    => adc1_m_strm_data & adc1_m_strm_ready & adc1_m_strm_valid & trigger_start,
--    TRIG0   => adc1_m_strm_ready & adc1_m_strm_valid & trigger_start
--    );
--
--ila2_inst : ENTITY ila
--  port map(
--    CONTROL => control_2,
--    CLK     => clk_125MHz,
--    DATA    => adc2_m_strm_data & adc2_m_strm_ready & adc2_m_strm_valid & trigger_start,
--    TRIG0   => adc2_m_strm_ready & adc2_m_strm_valid & trigger_start
--    );
--
--icon_inst : entity icon
--  port map (
--    CONTROL0    => control_1,
--    CONTROL1    => control_2
--    );

end Behavioral;
