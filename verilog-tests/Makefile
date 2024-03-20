TOP = test_counter
COUNTER_MODULE = MyCounter
CHISEL_SRC = $(COUNTER_MODULE).scala
CHISEL_VERILOG = $(COUNTER_MODULE).v
TEST_SRC= $(COUNTER_MODULE)Test.v
BIN = $(TOP).vvp

$(BIN): $(SRC) $(TEST_SRC)
	iverilog -o $(BIN) -s $(TOP) $(CHISEL_VERILOG) $(TEST_SRC)

.PHONY: all clean test-chisel

all: $(BIN)

test-chisel: $(BIN)
	vvp $(BIN)

test-calyx:
	fud exec $(COUNTER_MODULE).futil --to dat --through icarus-verilog -s verilog.data data.json

$(CHISEL_VERILOG):
	scala-cli $(CHISEL_SRC) > $(CHISEL_VERILOG)

clean:
	rm -f *.vvp *.vcd