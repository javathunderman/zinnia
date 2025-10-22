mkdir out
cargo run $1
fud2 out/output.futil --to vcd -o wave.vcd -s sim.data=out/data.json --through verilator
