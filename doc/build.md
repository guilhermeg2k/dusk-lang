# Build

## Dependencies

- [Zig 0.16.0](https://ziglang.org/download/)

## Platform Support

Currently only **Linux** (x86_64, aarch64) is supported. macOS and Windows
support is planned but not yet implemented.

## Building

- Clone the project: `git clone https://github.com/guilhermeg2k/dusk-lang.git <dir>`
- Navigate to the cloned directory: `cd <dir>`
- Checkout on the release branch `git checkout release`
- Run the build command: `zig build`
- A binary file should be generated at `./zig-out/bin`
- Execute a Dusk file using the generated binary: `./zig-out/bin/dusk <your-file.dsk>`

## Running

After cloning the project you can also direct running it

- Run the command: `zig build run -- <.dsk file-dir>`

## Tests

That is a simple test suit

- To run it use the command `zig build test`
