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

Named timezone rules are included in the local `vendor/timezone-data` dependency. Normal builds do not regenerate them or require the sibling tooling repository. See the [timezone data update procedure](../vendor/timezone-data/README.md) for provenance and maintenance.

## Running

After cloning the project you can also run a file directly:

- Run the command: `zig build run -- <file.dsk>`

## Tests

The suite compiles and runs assertion cases, checks expected diagnostics, and exercises native helpers and task/I/O integration. It also builds the `dusk` binary and a small helper program, and runs `dusk` as a separate process for what only the command line shows: arguments and exit codes, signals, child processes, and the local time zone under different `TZ` settings. Run it in both modes:

```sh
zig build test --summary all
zig build test -Doptimize=ReleaseFast --summary all
```

Cross-builds check compilation and linking; runtime behavior, particularly system-local discovery, must also be tested on the target operating system.

