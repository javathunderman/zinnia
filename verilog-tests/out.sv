module comb_mem_d1 #(
    parameter WIDTH = 32,
    parameter SIZE = 16,
    parameter IDX_SIZE = 4
) (
   input wire                logic [IDX_SIZE-1:0] addr0,
   input wire                logic [ WIDTH-1:0] write_data,
   input wire                logic write_en,
   input wire                logic clk,
   input wire                logic reset,
   output logic [ WIDTH-1:0] read_data,
   output logic              done
);

  logic [WIDTH-1:0] mem[SIZE-1:0];

  /* verilator lint_off WIDTH */
  assign read_data = mem[addr0];

  always_ff @(posedge clk) begin
    if (reset)
      done <= '0;
    else if (write_en)
      done <= '1;
    else
      done <= '0;
  end

  always_ff @(posedge clk) begin
    if (!reset && write_en)
      mem[addr0] <= write_data;
  end

  // Check for out of bounds access
  `ifdef VERILATOR
    always_comb begin
      if (addr0 >= SIZE)
        $error(
          "comb_mem_d1: Out of bounds access\n",
          "addr0: %0d\n", addr0,
          "SIZE: %0d", SIZE
        );
    end
  `endif
endmodule

module comb_mem_d2 #(
    parameter WIDTH = 32,
    parameter D0_SIZE = 16,
    parameter D1_SIZE = 16,
    parameter D0_IDX_SIZE = 4,
    parameter D1_IDX_SIZE = 4
) (
   input wire                logic [D0_IDX_SIZE-1:0] addr0,
   input wire                logic [D1_IDX_SIZE-1:0] addr1,
   input wire                logic [ WIDTH-1:0] write_data,
   input wire                logic write_en,
   input wire                logic clk,
   input wire                logic reset,
   output logic [ WIDTH-1:0] read_data,
   output logic              done
);

  /* verilator lint_off WIDTH */
  logic [WIDTH-1:0] mem[D0_SIZE-1:0][D1_SIZE-1:0];

  assign read_data = mem[addr0][addr1];

  always_ff @(posedge clk) begin
    if (reset)
      done <= '0;
    else if (write_en)
      done <= '1;
    else
      done <= '0;
  end

  always_ff @(posedge clk) begin
    if (!reset && write_en)
      mem[addr0][addr1] <= write_data;
  end

  // Check for out of bounds access
  `ifdef VERILATOR
    always_comb begin
      if (addr0 >= D0_SIZE)
        $error(
          "comb_mem_d2: Out of bounds access\n",
          "addr0: %0d\n", addr0,
          "D0_SIZE: %0d", D0_SIZE
        );
      if (addr1 >= D1_SIZE)
        $error(
          "comb_mem_d2: Out of bounds access\n",
          "addr1: %0d\n", addr1,
          "D1_SIZE: %0d", D1_SIZE
        );
    end
  `endif
endmodule

module comb_mem_d3 #(
    parameter WIDTH = 32,
    parameter D0_SIZE = 16,
    parameter D1_SIZE = 16,
    parameter D2_SIZE = 16,
    parameter D0_IDX_SIZE = 4,
    parameter D1_IDX_SIZE = 4,
    parameter D2_IDX_SIZE = 4
) (
   input wire                logic [D0_IDX_SIZE-1:0] addr0,
   input wire                logic [D1_IDX_SIZE-1:0] addr1,
   input wire                logic [D2_IDX_SIZE-1:0] addr2,
   input wire                logic [ WIDTH-1:0] write_data,
   input wire                logic write_en,
   input wire                logic clk,
   input wire                logic reset,
   output logic [ WIDTH-1:0] read_data,
   output logic              done
);

  /* verilator lint_off WIDTH */
  logic [WIDTH-1:0] mem[D0_SIZE-1:0][D1_SIZE-1:0][D2_SIZE-1:0];

  assign read_data = mem[addr0][addr1][addr2];

  always_ff @(posedge clk) begin
    if (reset)
      done <= '0;
    else if (write_en)
      done <= '1;
    else
      done <= '0;
  end

  always_ff @(posedge clk) begin
    if (!reset && write_en)
      mem[addr0][addr1][addr2] <= write_data;
  end

  // Check for out of bounds access
  `ifdef VERILATOR
    always_comb begin
      if (addr0 >= D0_SIZE)
        $error(
          "comb_mem_d3: Out of bounds access\n",
          "addr0: %0d\n", addr0,
          "D0_SIZE: %0d", D0_SIZE
        );
      if (addr1 >= D1_SIZE)
        $error(
          "comb_mem_d3: Out of bounds access\n",
          "addr1: %0d\n", addr1,
          "D1_SIZE: %0d", D1_SIZE
        );
      if (addr2 >= D2_SIZE)
        $error(
          "comb_mem_d3: Out of bounds access\n",
          "addr2: %0d\n", addr2,
          "D2_SIZE: %0d", D2_SIZE
        );
    end
  `endif
endmodule

module comb_mem_d4 #(
    parameter WIDTH = 32,
    parameter D0_SIZE = 16,
    parameter D1_SIZE = 16,
    parameter D2_SIZE = 16,
    parameter D3_SIZE = 16,
    parameter D0_IDX_SIZE = 4,
    parameter D1_IDX_SIZE = 4,
    parameter D2_IDX_SIZE = 4,
    parameter D3_IDX_SIZE = 4
) (
   input wire                logic [D0_IDX_SIZE-1:0] addr0,
   input wire                logic [D1_IDX_SIZE-1:0] addr1,
   input wire                logic [D2_IDX_SIZE-1:0] addr2,
   input wire                logic [D3_IDX_SIZE-1:0] addr3,
   input wire                logic [ WIDTH-1:0] write_data,
   input wire                logic write_en,
   input wire                logic clk,
   input wire                logic reset,
   output logic [ WIDTH-1:0] read_data,
   output logic              done
);

  /* verilator lint_off WIDTH */
  logic [WIDTH-1:0] mem[D0_SIZE-1:0][D1_SIZE-1:0][D2_SIZE-1:0][D3_SIZE-1:0];

  assign read_data = mem[addr0][addr1][addr2][addr3];

  always_ff @(posedge clk) begin
    if (reset)
      done <= '0;
    else if (write_en)
      done <= '1;
    else
      done <= '0;
  end

  always_ff @(posedge clk) begin
    if (!reset && write_en)
      mem[addr0][addr1][addr2][addr3] <= write_data;
  end

  // Check for out of bounds access
  `ifdef VERILATOR
    always_comb begin
      if (addr0 >= D0_SIZE)
        $error(
          "comb_mem_d4: Out of bounds access\n",
          "addr0: %0d\n", addr0,
          "D0_SIZE: %0d", D0_SIZE
        );
      if (addr1 >= D1_SIZE)
        $error(
          "comb_mem_d4: Out of bounds access\n",
          "addr1: %0d\n", addr1,
          "D1_SIZE: %0d", D1_SIZE
        );
      if (addr2 >= D2_SIZE)
        $error(
          "comb_mem_d4: Out of bounds access\n",
          "addr2: %0d\n", addr2,
          "D2_SIZE: %0d", D2_SIZE
        );
      if (addr3 >= D3_SIZE)
        $error(
          "comb_mem_d4: Out of bounds access\n",
          "addr3: %0d\n", addr3,
          "D3_SIZE: %0d", D3_SIZE
        );
    end
  `endif
endmodule

/**
 * Core primitives for Calyx.
 * Implements core primitives used by the compiler.
 *
 * Conventions:
 * - All parameter names must be SNAKE_CASE and all caps.
 * - Port names must be snake_case, no caps.
 */

module std_slice #(
    parameter IN_WIDTH  = 32,
    parameter OUT_WIDTH = 32
) (
   input wire                   logic [ IN_WIDTH-1:0] in,
   output logic [OUT_WIDTH-1:0] out
);
  assign out = in[OUT_WIDTH-1:0];

  `ifdef VERILATOR
    always_comb begin
      if (IN_WIDTH < OUT_WIDTH)
        $error(
          "std_slice: Input width less than output width\n",
          "IN_WIDTH: %0d", IN_WIDTH,
          "OUT_WIDTH: %0d", OUT_WIDTH
        );
    end
  `endif
endmodule

module std_pad #(
    parameter IN_WIDTH  = 32,
    parameter OUT_WIDTH = 32
) (
   input wire logic [IN_WIDTH-1:0]  in,
   output logic     [OUT_WIDTH-1:0] out
);
  localparam EXTEND = OUT_WIDTH - IN_WIDTH;
  assign out = { {EXTEND {1'b0}}, in};

  `ifdef VERILATOR
    always_comb begin
      if (IN_WIDTH > OUT_WIDTH)
        $error(
          "std_pad: Output width less than input width\n",
          "IN_WIDTH: %0d", IN_WIDTH,
          "OUT_WIDTH: %0d", OUT_WIDTH
        );
    end
  `endif
endmodule

module std_cat #(
  parameter LEFT_WIDTH  = 32,
  parameter RIGHT_WIDTH = 32,
  parameter OUT_WIDTH = 64
) (
  input wire logic [LEFT_WIDTH-1:0] left,
  input wire logic [RIGHT_WIDTH-1:0] right,
  output logic [OUT_WIDTH-1:0] out
);
  assign out = {left, right};

  `ifdef VERILATOR
    always_comb begin
      if (LEFT_WIDTH + RIGHT_WIDTH != OUT_WIDTH)
        $error(
          "std_cat: Output width must equal sum of input widths\n",
          "LEFT_WIDTH: %0d", LEFT_WIDTH,
          "RIGHT_WIDTH: %0d", RIGHT_WIDTH,
          "OUT_WIDTH: %0d", OUT_WIDTH
        );
    end
  `endif
endmodule

module std_not #(
    parameter WIDTH = 32
) (
   input wire               logic [WIDTH-1:0] in,
   output logic [WIDTH-1:0] out
);
  assign out = ~in;
endmodule

module std_and #(
    parameter WIDTH = 32
) (
   input wire               logic [WIDTH-1:0] left,
   input wire               logic [WIDTH-1:0] right,
   output logic [WIDTH-1:0] out
);
  assign out = left & right;
endmodule

module std_or #(
    parameter WIDTH = 32
) (
   input wire               logic [WIDTH-1:0] left,
   input wire               logic [WIDTH-1:0] right,
   output logic [WIDTH-1:0] out
);
  assign out = left | right;
endmodule

module std_xor #(
    parameter WIDTH = 32
) (
   input wire               logic [WIDTH-1:0] left,
   input wire               logic [WIDTH-1:0] right,
   output logic [WIDTH-1:0] out
);
  assign out = left ^ right;
endmodule

module std_sub #(
    parameter WIDTH = 32
) (
   input wire               logic [WIDTH-1:0] left,
   input wire               logic [WIDTH-1:0] right,
   output logic [WIDTH-1:0] out
);
  assign out = left - right;
endmodule

module std_gt #(
    parameter WIDTH = 32
) (
   input wire   logic [WIDTH-1:0] left,
   input wire   logic [WIDTH-1:0] right,
   output logic out
);
  assign out = left > right;
endmodule

module std_lt #(
    parameter WIDTH = 32
) (
   input wire   logic [WIDTH-1:0] left,
   input wire   logic [WIDTH-1:0] right,
   output logic out
);
  assign out = left < right;
endmodule

module std_eq #(
    parameter WIDTH = 32
) (
   input wire   logic [WIDTH-1:0] left,
   input wire   logic [WIDTH-1:0] right,
   output logic out
);
  assign out = left == right;
endmodule

module std_neq #(
    parameter WIDTH = 32
) (
   input wire   logic [WIDTH-1:0] left,
   input wire   logic [WIDTH-1:0] right,
   output logic out
);
  assign out = left != right;
endmodule

module std_ge #(
    parameter WIDTH = 32
) (
    input wire   logic [WIDTH-1:0] left,
    input wire   logic [WIDTH-1:0] right,
    output logic out
);
  assign out = left >= right;
endmodule

module std_le #(
    parameter WIDTH = 32
) (
   input wire   logic [WIDTH-1:0] left,
   input wire   logic [WIDTH-1:0] right,
   output logic out
);
  assign out = left <= right;
endmodule

module std_lsh #(
    parameter WIDTH = 32
) (
   input wire               logic [WIDTH-1:0] left,
   input wire               logic [WIDTH-1:0] right,
   output logic [WIDTH-1:0] out
);
  assign out = left << right;
endmodule

module std_rsh #(
    parameter WIDTH = 32
) (
   input wire               logic [WIDTH-1:0] left,
   input wire               logic [WIDTH-1:0] right,
   output logic [WIDTH-1:0] out
);
  assign out = left >> right;
endmodule

/// this primitive is intended to be used
/// for lowering purposes (not in source programs)
module std_mux #(
    parameter WIDTH = 32
) (
   input wire               logic cond,
   input wire               logic [WIDTH-1:0] tru,
   input wire               logic [WIDTH-1:0] fal,
   output logic [WIDTH-1:0] out
);
  assign out = cond ? tru : fal;
endmodule

module std_bit_slice #(
    parameter IN_WIDTH = 32,
    parameter START_IDX = 0,
    parameter END_IDX = 31,
    parameter OUT_WIDTH = 32
)(
   input wire logic [IN_WIDTH-1:0] in,
   output logic [OUT_WIDTH-1:0] out
);
    assign out = in[END_IDX:START_IDX];

  `ifdef VERILATOR
    always_comb begin
      if (START_IDX < 0 || END_IDX > IN_WIDTH-1)
        $error(
          "std_bit_slice: Slice range out of bounds\n",
          "IN_WIDTH: %0d", IN_WIDTH,
          "START_IDX: %0d", START_IDX,
          "END_IDX: %0d", END_IDX,
        );
    end
  `endif

endmodule

module undef #(
    parameter WIDTH = 32
) (
   output logic [WIDTH-1:0] out
);
assign out = 'x;
endmodule

module std_const #(
    parameter WIDTH = 32,
    parameter VALUE = 32
) (
   output logic [WIDTH-1:0] out
);
assign out = VALUE;
endmodule

module std_wire #(
    parameter WIDTH = 32
) (
   input wire logic [WIDTH-1:0] in,
   output logic [WIDTH-1:0] out
);
assign out = in;
endmodule

module std_add #(
    parameter WIDTH = 32
) (
   input wire logic [WIDTH-1:0] left,
   input wire logic [WIDTH-1:0] right,
   output logic [WIDTH-1:0] out
);
assign out = left + right;
endmodule

module std_reg #(
    parameter WIDTH = 32
) (
   input wire logic [WIDTH-1:0] in,
   input wire logic write_en,
   input wire logic clk,
   input wire logic reset,
   output logic [WIDTH-1:0] out,
   output logic done
);
always_ff @(posedge clk) begin
    if (reset) begin
       out <= 0;
       done <= 0;
    end else if (write_en) begin
      out <= in;
      done <= 1'd1;
    end else done <= 1'd0;
  end
endmodule

module main(
  input logic go,
  output logic done,
  input logic clk,
  input logic reset
);
// COMPONENT START: main
string DATA;
int CODE;
initial begin
    CODE = $value$plusargs("DATA=%s", DATA);
    $display("DATA (path to meminit files): %s", DATA);
    $readmemh({DATA, "/mem.dat"}, mem.mem);
end
final begin
    $writememh({DATA, "/mem.out"}, mem.mem);
end
logic mem_addr0;
logic [3:0] mem_write_data;
logic mem_write_en;
logic mem_clk;
logic mem_reset;
logic [3:0] mem_read_data;
logic mem_done;
logic [3:0] val_in;
logic val_write_en;
logic val_clk;
logic val_reset;
logic [3:0] val_out;
logic val_done;
logic [3:0] add_left;
logic [3:0] add_right;
logic [3:0] add_out;
logic [3:0] counter_in;
logic counter_write_en;
logic counter_clk;
logic counter_reset;
logic [3:0] counter_out;
logic counter_done;
logic [3:0] add2_left;
logic [3:0] add2_right;
logic [3:0] add2_out;
logic [3:0] lt_left;
logic [3:0] lt_right;
logic lt_out;
logic comb_reg_in;
logic comb_reg_write_en;
logic comb_reg_clk;
logic comb_reg_reset;
logic comb_reg_out;
logic comb_reg_done;
logic [2:0] fsm_in;
logic fsm_write_en;
logic fsm_clk;
logic fsm_reset;
logic [2:0] fsm_out;
logic fsm_done;
logic ud_out;
logic [2:0] adder_left;
logic [2:0] adder_right;
logic [2:0] adder_out;
logic ud0_out;
logic [2:0] adder0_left;
logic [2:0] adder0_right;
logic [2:0] adder0_out;
logic signal_reg_in;
logic signal_reg_write_en;
logic signal_reg_clk;
logic signal_reg_reset;
logic signal_reg_out;
logic signal_reg_done;
logic [1:0] fsm0_in;
logic fsm0_write_en;
logic fsm0_clk;
logic fsm0_reset;
logic [1:0] fsm0_out;
logic fsm0_done;
logic invoke0_go_in;
logic invoke0_go_out;
logic invoke0_done_in;
logic invoke0_done_out;
logic early_reset_cond0_go_in;
logic early_reset_cond0_go_out;
logic early_reset_cond0_done_in;
logic early_reset_cond0_done_out;
logic early_reset_static_seq_go_in;
logic early_reset_static_seq_go_out;
logic early_reset_static_seq_done_in;
logic early_reset_static_seq_done_out;
logic wrapper_early_reset_cond0_go_in;
logic wrapper_early_reset_cond0_go_out;
logic wrapper_early_reset_cond0_done_in;
logic wrapper_early_reset_cond0_done_out;
logic while_wrapper_early_reset_static_seq_go_in;
logic while_wrapper_early_reset_static_seq_go_out;
logic while_wrapper_early_reset_static_seq_done_in;
logic while_wrapper_early_reset_static_seq_done_out;
logic tdcc_go_in;
logic tdcc_go_out;
logic tdcc_done_in;
logic tdcc_done_out;
comb_mem_d1 # (
    .IDX_SIZE(1),
    .SIZE(1),
    .WIDTH(4)
) mem (
    .addr0(mem_addr0),
    .clk(mem_clk),
    .done(mem_done),
    .read_data(mem_read_data),
    .reset(mem_reset),
    .write_data(mem_write_data),
    .write_en(mem_write_en)
);
std_reg # (
    .WIDTH(4)
) val (
    .clk(val_clk),
    .done(val_done),
    .in(val_in),
    .out(val_out),
    .reset(val_reset),
    .write_en(val_write_en)
);
std_add # (
    .WIDTH(4)
) add (
    .left(add_left),
    .out(add_out),
    .right(add_right)
);
std_reg # (
    .WIDTH(4)
) counter (
    .clk(counter_clk),
    .done(counter_done),
    .in(counter_in),
    .out(counter_out),
    .reset(counter_reset),
    .write_en(counter_write_en)
);
std_add # (
    .WIDTH(4)
) add2 (
    .left(add2_left),
    .out(add2_out),
    .right(add2_right)
);
std_lt # (
    .WIDTH(4)
) lt (
    .left(lt_left),
    .out(lt_out),
    .right(lt_right)
);
std_reg # (
    .WIDTH(1)
) comb_reg (
    .clk(comb_reg_clk),
    .done(comb_reg_done),
    .in(comb_reg_in),
    .out(comb_reg_out),
    .reset(comb_reg_reset),
    .write_en(comb_reg_write_en)
);
std_reg # (
    .WIDTH(3)
) fsm (
    .clk(fsm_clk),
    .done(fsm_done),
    .in(fsm_in),
    .out(fsm_out),
    .reset(fsm_reset),
    .write_en(fsm_write_en)
);
undef # (
    .WIDTH(1)
) ud (
    .out(ud_out)
);
std_add # (
    .WIDTH(3)
) adder (
    .left(adder_left),
    .out(adder_out),
    .right(adder_right)
);
undef # (
    .WIDTH(1)
) ud0 (
    .out(ud0_out)
);
std_add # (
    .WIDTH(3)
) adder0 (
    .left(adder0_left),
    .out(adder0_out),
    .right(adder0_right)
);
std_reg # (
    .WIDTH(1)
) signal_reg (
    .clk(signal_reg_clk),
    .done(signal_reg_done),
    .in(signal_reg_in),
    .out(signal_reg_out),
    .reset(signal_reg_reset),
    .write_en(signal_reg_write_en)
);
std_reg # (
    .WIDTH(2)
) fsm0 (
    .clk(fsm0_clk),
    .done(fsm0_done),
    .in(fsm0_in),
    .out(fsm0_out),
    .reset(fsm0_reset),
    .write_en(fsm0_write_en)
);
std_wire # (
    .WIDTH(1)
) invoke0_go (
    .in(invoke0_go_in),
    .out(invoke0_go_out)
);
std_wire # (
    .WIDTH(1)
) invoke0_done (
    .in(invoke0_done_in),
    .out(invoke0_done_out)
);
std_wire # (
    .WIDTH(1)
) early_reset_cond0_go (
    .in(early_reset_cond0_go_in),
    .out(early_reset_cond0_go_out)
);
std_wire # (
    .WIDTH(1)
) early_reset_cond0_done (
    .in(early_reset_cond0_done_in),
    .out(early_reset_cond0_done_out)
);
std_wire # (
    .WIDTH(1)
) early_reset_static_seq_go (
    .in(early_reset_static_seq_go_in),
    .out(early_reset_static_seq_go_out)
);
std_wire # (
    .WIDTH(1)
) early_reset_static_seq_done (
    .in(early_reset_static_seq_done_in),
    .out(early_reset_static_seq_done_out)
);
std_wire # (
    .WIDTH(1)
) wrapper_early_reset_cond0_go (
    .in(wrapper_early_reset_cond0_go_in),
    .out(wrapper_early_reset_cond0_go_out)
);
std_wire # (
    .WIDTH(1)
) wrapper_early_reset_cond0_done (
    .in(wrapper_early_reset_cond0_done_in),
    .out(wrapper_early_reset_cond0_done_out)
);
std_wire # (
    .WIDTH(1)
) while_wrapper_early_reset_static_seq_go (
    .in(while_wrapper_early_reset_static_seq_go_in),
    .out(while_wrapper_early_reset_static_seq_go_out)
);
std_wire # (
    .WIDTH(1)
) while_wrapper_early_reset_static_seq_done (
    .in(while_wrapper_early_reset_static_seq_done_in),
    .out(while_wrapper_early_reset_static_seq_done_out)
);
std_wire # (
    .WIDTH(1)
) tdcc_go (
    .in(tdcc_go_in),
    .out(tdcc_go_out)
);
std_wire # (
    .WIDTH(1)
) tdcc_done (
    .in(tdcc_done_in),
    .out(tdcc_done_out)
);
wire _guard0 = 1;
wire _guard1 = tdcc_done_out;
wire _guard2 = early_reset_cond0_go_out;
wire _guard3 = early_reset_static_seq_go_out;
wire _guard4 = _guard2 | _guard3;
wire _guard5 = fsm_out != 3'd0;
wire _guard6 = early_reset_cond0_go_out;
wire _guard7 = _guard5 & _guard6;
wire _guard8 = fsm_out != 3'd3;
wire _guard9 = early_reset_static_seq_go_out;
wire _guard10 = _guard8 & _guard9;
wire _guard11 = fsm_out == 3'd0;
wire _guard12 = early_reset_cond0_go_out;
wire _guard13 = _guard11 & _guard12;
wire _guard14 = fsm_out == 3'd3;
wire _guard15 = early_reset_static_seq_go_out;
wire _guard16 = _guard14 & _guard15;
wire _guard17 = _guard13 | _guard16;
wire _guard18 = early_reset_cond0_go_out;
wire _guard19 = early_reset_cond0_go_out;
wire _guard20 = fsm_out == 3'd0;
wire _guard21 = early_reset_static_seq_go_out;
wire _guard22 = _guard20 & _guard21;
wire _guard23 = fsm_out == 3'd0;
wire _guard24 = early_reset_static_seq_go_out;
wire _guard25 = _guard23 & _guard24;
wire _guard26 = early_reset_cond0_go_out;
wire _guard27 = fsm_out == 3'd3;
wire _guard28 = early_reset_static_seq_go_out;
wire _guard29 = _guard27 & _guard28;
wire _guard30 = _guard26 | _guard29;
wire _guard31 = early_reset_cond0_go_out;
wire _guard32 = fsm_out == 3'd3;
wire _guard33 = early_reset_static_seq_go_out;
wire _guard34 = _guard32 & _guard33;
wire _guard35 = _guard31 | _guard34;
wire _guard36 = wrapper_early_reset_cond0_go_out;
wire _guard37 = fsm_out == 3'd0;
wire _guard38 = signal_reg_out;
wire _guard39 = _guard37 & _guard38;
wire _guard40 = fsm_out == 3'd0;
wire _guard41 = fsm_out == 3'd1;
wire _guard42 = _guard40 | _guard41;
wire _guard43 = early_reset_static_seq_go_out;
wire _guard44 = _guard42 & _guard43;
wire _guard45 = fsm_out == 3'd1;
wire _guard46 = early_reset_static_seq_go_out;
wire _guard47 = _guard45 & _guard46;
wire _guard48 = fsm_out == 3'd0;
wire _guard49 = early_reset_static_seq_go_out;
wire _guard50 = _guard48 & _guard49;
wire _guard51 = while_wrapper_early_reset_static_seq_done_out;
wire _guard52 = ~_guard51;
wire _guard53 = fsm0_out == 2'd2;
wire _guard54 = _guard52 & _guard53;
wire _guard55 = tdcc_go_out;
wire _guard56 = _guard54 & _guard55;
wire _guard57 = invoke0_go_out;
wire _guard58 = fsm_out == 3'd0;
wire _guard59 = early_reset_static_seq_go_out;
wire _guard60 = _guard58 & _guard59;
wire _guard61 = _guard57 | _guard60;
wire _guard62 = fsm_out == 3'd0;
wire _guard63 = early_reset_static_seq_go_out;
wire _guard64 = _guard62 & _guard63;
wire _guard65 = invoke0_go_out;
wire _guard66 = invoke0_done_out;
wire _guard67 = ~_guard66;
wire _guard68 = fsm0_out == 2'd0;
wire _guard69 = _guard67 & _guard68;
wire _guard70 = tdcc_go_out;
wire _guard71 = _guard69 & _guard70;
wire _guard72 = fsm0_out == 2'd3;
wire _guard73 = fsm0_out == 2'd0;
wire _guard74 = invoke0_done_out;
wire _guard75 = _guard73 & _guard74;
wire _guard76 = tdcc_go_out;
wire _guard77 = _guard75 & _guard76;
wire _guard78 = _guard72 | _guard77;
wire _guard79 = fsm0_out == 2'd1;
wire _guard80 = wrapper_early_reset_cond0_done_out;
wire _guard81 = _guard79 & _guard80;
wire _guard82 = tdcc_go_out;
wire _guard83 = _guard81 & _guard82;
wire _guard84 = _guard78 | _guard83;
wire _guard85 = fsm0_out == 2'd2;
wire _guard86 = while_wrapper_early_reset_static_seq_done_out;
wire _guard87 = _guard85 & _guard86;
wire _guard88 = tdcc_go_out;
wire _guard89 = _guard87 & _guard88;
wire _guard90 = _guard84 | _guard89;
wire _guard91 = fsm0_out == 2'd0;
wire _guard92 = invoke0_done_out;
wire _guard93 = _guard91 & _guard92;
wire _guard94 = tdcc_go_out;
wire _guard95 = _guard93 & _guard94;
wire _guard96 = fsm0_out == 2'd3;
wire _guard97 = fsm0_out == 2'd2;
wire _guard98 = while_wrapper_early_reset_static_seq_done_out;
wire _guard99 = _guard97 & _guard98;
wire _guard100 = tdcc_go_out;
wire _guard101 = _guard99 & _guard100;
wire _guard102 = fsm0_out == 2'd1;
wire _guard103 = wrapper_early_reset_cond0_done_out;
wire _guard104 = _guard102 & _guard103;
wire _guard105 = tdcc_go_out;
wire _guard106 = _guard104 & _guard105;
wire _guard107 = early_reset_static_seq_go_out;
wire _guard108 = early_reset_static_seq_go_out;
wire _guard109 = while_wrapper_early_reset_static_seq_go_out;
wire _guard110 = fsm_out == 3'd0;
wire _guard111 = signal_reg_out;
wire _guard112 = _guard110 & _guard111;
wire _guard113 = fsm_out == 3'd0;
wire _guard114 = signal_reg_out;
wire _guard115 = ~_guard114;
wire _guard116 = _guard113 & _guard115;
wire _guard117 = wrapper_early_reset_cond0_go_out;
wire _guard118 = _guard116 & _guard117;
wire _guard119 = _guard112 | _guard118;
wire _guard120 = fsm_out == 3'd0;
wire _guard121 = signal_reg_out;
wire _guard122 = ~_guard121;
wire _guard123 = _guard120 & _guard122;
wire _guard124 = wrapper_early_reset_cond0_go_out;
wire _guard125 = _guard123 & _guard124;
wire _guard126 = fsm_out == 3'd0;
wire _guard127 = signal_reg_out;
wire _guard128 = _guard126 & _guard127;
wire _guard129 = wrapper_early_reset_cond0_done_out;
wire _guard130 = ~_guard129;
wire _guard131 = fsm0_out == 2'd1;
wire _guard132 = _guard130 & _guard131;
wire _guard133 = tdcc_go_out;
wire _guard134 = _guard132 & _guard133;
wire _guard135 = fsm_out == 3'd1;
wire _guard136 = early_reset_static_seq_go_out;
wire _guard137 = _guard135 & _guard136;
wire _guard138 = fsm_out == 3'd1;
wire _guard139 = early_reset_static_seq_go_out;
wire _guard140 = _guard138 & _guard139;
wire _guard141 = fsm0_out == 2'd3;
wire _guard142 = fsm_out == 3'd2;
wire _guard143 = early_reset_static_seq_go_out;
wire _guard144 = _guard142 & _guard143;
wire _guard145 = fsm_out == 3'd0;
wire _guard146 = fsm_out == 3'd2;
wire _guard147 = _guard145 | _guard146;
wire _guard148 = early_reset_static_seq_go_out;
wire _guard149 = _guard147 & _guard148;
wire _guard150 = fsm_out == 3'd2;
wire _guard151 = early_reset_static_seq_go_out;
wire _guard152 = _guard150 & _guard151;
wire _guard153 = early_reset_cond0_go_out;
wire _guard154 = fsm_out == 3'd3;
wire _guard155 = early_reset_static_seq_go_out;
wire _guard156 = _guard154 & _guard155;
wire _guard157 = _guard153 | _guard156;
wire _guard158 = early_reset_cond0_go_out;
wire _guard159 = fsm_out == 3'd3;
wire _guard160 = early_reset_static_seq_go_out;
wire _guard161 = _guard159 & _guard160;
wire _guard162 = _guard158 | _guard161;
wire _guard163 = comb_reg_out;
wire _guard164 = ~_guard163;
wire _guard165 = fsm_out == 3'd0;
wire _guard166 = _guard164 & _guard165;
assign done = _guard1;
assign fsm_write_en = _guard4;
assign fsm_clk = clk;
assign fsm_reset = reset;
assign fsm_in =
  _guard7 ? adder_out :
  _guard10 ? adder0_out :
  _guard17 ? 3'd0 :
  3'd0;
always_comb begin
  if(~$onehot0({_guard17, _guard10, _guard7})) begin
    $fatal(2, "Multiple assignment to port `fsm.in'.");
end
end
assign adder_left =
  _guard18 ? fsm_out :
  3'd0;
assign adder_right =
  _guard19 ? 3'd1 :
  3'd0;
assign add2_left = counter_out;
assign add2_right = 4'd1;
assign comb_reg_write_en = _guard30;
assign comb_reg_clk = clk;
assign comb_reg_reset = reset;
assign comb_reg_in =
  _guard35 ? lt_out :
  1'd0;
assign early_reset_cond0_done_in = ud_out;
assign early_reset_cond0_go_in = _guard36;
assign wrapper_early_reset_cond0_done_in = _guard39;
assign val_write_en = _guard44;
assign val_clk = clk;
assign val_reset = reset;
assign val_in =
  _guard47 ? add_out :
  _guard50 ? mem_read_data :
  'x;
always_comb begin
  if(~$onehot0({_guard50, _guard47})) begin
    $fatal(2, "Multiple assignment to port `val.in'.");
end
end
assign while_wrapper_early_reset_static_seq_go_in = _guard56;
assign counter_write_en = _guard61;
assign counter_clk = clk;
assign counter_reset = reset;
assign counter_in =
  _guard64 ? add2_out :
  _guard65 ? 4'd0 :
  'x;
always_comb begin
  if(~$onehot0({_guard65, _guard64})) begin
    $fatal(2, "Multiple assignment to port `counter.in'.");
end
end
assign invoke0_go_in = _guard71;
assign tdcc_go_in = go;
assign fsm0_write_en = _guard90;
assign fsm0_clk = clk;
assign fsm0_reset = reset;
assign fsm0_in =
  _guard95 ? 2'd1 :
  _guard96 ? 2'd0 :
  _guard101 ? 2'd3 :
  _guard106 ? 2'd2 :
  2'd0;
always_comb begin
  if(~$onehot0({_guard106, _guard101, _guard96, _guard95})) begin
    $fatal(2, "Multiple assignment to port `fsm0.in'.");
end
end
assign adder0_left =
  _guard107 ? fsm_out :
  3'd0;
assign adder0_right =
  _guard108 ? 3'd1 :
  3'd0;
assign invoke0_done_in = counter_done;
assign early_reset_static_seq_go_in = _guard109;
assign signal_reg_write_en = _guard119;
assign signal_reg_clk = clk;
assign signal_reg_reset = reset;
assign signal_reg_in =
  _guard125 ? 1'd1 :
  _guard128 ? 1'd0 :
  1'd0;
always_comb begin
  if(~$onehot0({_guard128, _guard125})) begin
    $fatal(2, "Multiple assignment to port `signal_reg.in'.");
end
end
assign wrapper_early_reset_cond0_go_in = _guard134;
assign add_left = val_out;
assign add_right = 4'd2;
assign early_reset_static_seq_done_in = ud0_out;
assign tdcc_done_in = _guard141;
assign mem_write_en = _guard144;
assign mem_clk = clk;
assign mem_addr0 =
  _guard149 ? 1'd0 :
  1'd0;
assign mem_reset = reset;
assign mem_write_data = val_out;
assign lt_left =
  _guard157 ? counter_out :
  4'd0;
assign lt_right =
  _guard162 ? 4'd7 :
  4'd0;
assign while_wrapper_early_reset_static_seq_done_in = _guard166;
// COMPONENT END: main
endmodule

