import cocotb
from cocotb.triggers import RisingEdge, ClockCycles
from cocotb.clock import Clock

@cocotb.test()
async def first_test(module):
    # Required for all cocotb testbenches. Included for completeness.
    cocotb.start_soon(Clock(module.clk, 2, units="ns").start())

    # Reset Calyx-generated control registers
    module.reset.value = 1
    await ClockCycles(module.clk, 5)
    module.reset.value = 0

    # Start execution of control sequence
    module.go.value = 1
    await ClockCycles(module.clk, 7)
    module._log.info(f"Memory value (bitwise) {module.mem_write_data.value}")
    assert module.mem_write_data.value.integer == 2, "memory value is not 2!"
    # Succeeds on the write wires, but NOT on the read wires, regardless of the number of cycles - need to investigate why
    # await RisingEdge(module.done)
