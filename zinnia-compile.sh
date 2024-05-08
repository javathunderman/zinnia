mkdir out
cargo run $1
fud exec out/output.futil --to dat -s verilog.data out/data.json --through icarus-verilog