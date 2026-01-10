# Build

## Dependencies

- [Zig 0.15.2](https://ziglang.org/download/)

## Building

- Clone the project: `git clone https://github.com/guilhermeg2k/dusk-lang.git <dir>`
- Navigate to the cloned directory: `cd <dir>`
- Checkout on the last release commit [Releases](https://github.com/guilhermeg2k/dusk-lang/releases/)
- Run the build command: `zig build`
- A binary file should be generated at `./zig-out/bin`
- Execute a Dusk file using the generated binary: `./zig-out/bin/dusk <your-file.dsk>`

## Running

After cloning the project you can also direct running it

- Run the command: `zig build run -- <.dsk file-dir>`

## Tests

That is a simple test suit currently only checking if compiler does not crash while building some files:

- To run it use the command `zig build test`

