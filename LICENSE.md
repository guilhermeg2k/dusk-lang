MIT License

Copyright (c) 2023 Guilherme Nascimento

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

## Linked libraries

Dusk statically links these libraries. Each one comes from a fork in the
duskilang GitHub organization that holds the upstream release unchanged plus
a build.zig.

| Library | Version | License |
| :------ | :------ | :------ |
| [libuv](https://github.com/duskilang/libuv/tree/v1.53.0-dusk) | 1.53.0 | [MIT](https://github.com/duskilang/libuv/blob/v1.53.0-dusk/LICENSE) (parts: MIT, BSD 2-Clause, ISC; see [LICENSE-extra](https://github.com/duskilang/libuv/blob/v1.53.0-dusk/LICENSE-extra)) |
| [Mbed TLS](https://github.com/duskilang/mbedtls/tree/v4.1.1-dusk) (with TF-PSA-Crypto) | 4.1.1 | [Apache 2.0](https://github.com/duskilang/mbedtls/blob/v4.1.1-dusk/LICENSE) (dual-licensed Apache 2.0 or GPL 2.0 or later; used under Apache 2.0) |
| [llhttp](https://github.com/duskilang/llhttp/tree/v9.4.3-dusk) | 9.4.3 | [MIT](https://github.com/duskilang/llhttp/blob/v9.4.3-dusk/LICENSE) |
| [zlib-ng](https://github.com/duskilang/zlib-ng/tree/v2.3.3-dusk) | 2.3.3 | [zlib](https://github.com/duskilang/zlib-ng/blob/v2.3.3-dusk/LICENSE.md) |
