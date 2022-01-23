trap "exit" INT
cargo build --release

for file in tests/benchmark/*.lox; do
  echo Running $file...
  ./target/release/rust-lox.exe $file bytecode
done