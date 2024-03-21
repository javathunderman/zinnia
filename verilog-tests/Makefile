TOP = test_counter
COUNTER_MODULE = MyCounter
CHISEL_SRC = $(COUNTER_MODULE).scala
CHISEL_VERILOG = $(COUNTER_MODULE)-chisel.v
CALYX_VERILOG = $(COUNTER_MODULE)-calyx.v
TEST_SRC= $(COUNTER_MODULE)Test.v
CHISEL_BIN = $(TOP)-chisel.vvp

$(CHISEL_BIN): $(CHISEL_VERILOG) $(TEST_SRC)
	iverilog -o $(CHISEL_BIN) -s $(TOP) $(CHISEL_VERILOG) $(TEST_SRC)

.PHONY: all clean test-chisel

all: $(BIN)

test-chisel: $(CHISEL_BIN)
	vvp $(CHISEL_BIN)

test-calyx: # This is for being able to see the wave table
	fud e $(COUNTER_MODULE).futil -o wave2.vcd -s verilog.data data.json --through icarus-verilog

test-calyx-passthrough:
	fud exec $(COUNTER_MODULE).futil --to dat --through icarus-verilog -s verilog.data data.json

$(CALYX_VERILOG):
	fud exec $(COUNTER_MODULE).futil --to verilog > $(CALYX_VERILOG)

$(CHISEL_VERILOG):
	scala-cli $(CHISEL_SRC) > $(CHISEL_VERILOG)

clean:
	rm -f *.vvp *.vcd MyCounter-*