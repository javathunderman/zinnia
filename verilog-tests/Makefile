TOP = test_counter
COUNTER_MODULE = MyCounter
CHISEL_SRC = $(COUNTER_MODULE).scala
CHISEL_VERILOG = $(COUNTER_MODULE)-chisel.v
CALYX_VERILOG = $(COUNTER_MODULE)-calyx.sv
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

synthesize-calyx:
	fud exec $(COUNTER_MODULE).futil --to synth-verilog > $(CALYX_VERILOG) && sv2v $(CALYX_VERILOG) > deploy/synthesizable.v

$(CHISEL_VERILOG):
	scala-cli $(CHISEL_SRC) > $(CHISEL_VERILOG)

deploy-calyx: $(CALYX_VERILOG)
	cd deploy && apio build && apio upload

# Essentially a no-op circuit
min_example_synth:
	fud exec min_example.futil --to verilog > min_example.sv && sv2v min_example.sv > min_example-synthesizable.v

clean:
	rm -f *.vvp *.vcd MyCounter-* min_example.sv *-synthesizable.v deploy/hardware* clock-div-test/hardware* deploy/synthesizable.v
