mkdir out
cargo run $1
fud2 out/output.futil --to dat -s sim.data=out/data.json --through verilator
