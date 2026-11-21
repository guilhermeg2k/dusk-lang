# Build

## Dependencies

- [Zig 0.16.0](https://ziglang.org/download/)

## Building

- Clone the project: `git clone https://github.com/guilhermeg2k/dusk-lang.git <dir>`
- Navigate to the cloned directory: `cd <dir>`
- If you want the latest stable version, check out the `release` branch
- Run the build command: `zig build -Doptimize=ReleaseFast`
- A binary file should be generated at `./zig-out/bin`
- Execute a Dusk file using the generated binary: `./zig-out/bin/dusk <your-file.dsk>`

## Running

After cloning the project you can also run a file directly:

- Run the command: `zig build run -- <file.dsk>`

## Tests

Run the test suite with:

- `zig build test --summary all`

