# Build

## Dependencies

- [Zig 0.16.0](https://ziglang.org/download/)

## Building

- Clone the project: `git clone https://github.com/guilhermeg2k/dusk-lang.git <dir>`
- Navigate to the cloned directory: `cd <dir>`
- If you want most stable version checkout on the `release` branch
- Run the build command: `zig build -Doptimize=ReleaseFast`
- A binary file should be generated at `./zig-out/bin`
- Execute a Dusk file using the generated binary: `./zig-out/bin/dusk <your-file.dsk>`

Named timezone rules are included in the local `vendor/timezone-data` dependency. Normal builds do not regenerate them or require the sibling tooling repository. See the [timezone data update procedure](../vendor/timezone-data/README.md) for provenance and maintenance.

## Running

After cloning the project you can also direct running it

- Run the command: `zig build run -- <.dsk file-dir>`

## Tests

The suite compiles and runs assertion cases, checks expected diagnostics, and exercises native helpers and task/I/O integration. Run it in both modes:

```sh
zig build test --summary all
zig build test -Doptimize=ReleaseFast --summary all
```

The local-time CLI checks cover explicit configuration, file snapshots, failure/retry, and shutdown in isolated processes:

```sh
zig build
python3 test/time_local_cli.py
zig build -Doptimize=ReleaseFast --prefix .zig-cache/validation-release
python3 test/time_local_cli.py --dusk .zig-cache/validation-release/bin/dusk
```

Run the time example with `./zig-out/bin/dusk examples/time.dsk`. Named-zone examples use bundled rules and do not depend on the host's timezone setting.

Cross-builds check compilation and linking; runtime behavior, particularly system-local discovery, must also be tested on the target operating system.

