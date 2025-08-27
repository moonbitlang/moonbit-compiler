<div align="center">
  <picture>
    <img src="logo.png" height="400px"/>
  </picture> 

  <h1> MoonBit Compiler

[MoonBit] | [Documentation] | [Tour] | [Core]
</div>

This is the source code repository for [MoonBit], a programming language that is user-friendly, builds fast, and produces high quality output.

[Moonbit]: https://www.moonbitlang.com
[Tour]: https://tour.moonbitlang.com
[Documentation]: https://docs.moonbitlang.com
[Core]: https://github.com/moonbitlang/core

## Roadmap

Building a programming language is a long journey. It took Rust 9 years and Go 5 years to reach 1.0. Made by a young and driven team, MoonBit is steadily moving forward. We understand that community adoption and expansion are key to a new language, and we’re fully committed to nurturing an engaged and collaborative community around MoonBit. So far, we have open-sourced [the core library](https://github.com/moonbitlang/core) and most tools, including [build tools](https://github.com/moonbitlang/moon), [lex](https://github.com/moonbit-community/moonlex), [markdown](https://github.com/moonbit-community/cmark), and more to come. Having the compiler source available is important for security measures. Open-sourcing the Wasm backend is another major step, and it is on our roadmap to open source more (moonfmt, moondoc) in the future.


## Build from source

### Prerequisites

- OCaml 4.14.2
- [OPAM](https://opam.ocaml.org/)

### Build

Build with following scripts:

```
opam switch create 4.14.2
opam install -y dune
dune build -p moonbit-lang
```

## Use Wasm-based MoonBit Compiler

First, you need to install Node.js and curl. Then, run the following commands in a temp directory:

```shell
curl -fSL -O https://github.com/moonbitlang/moonbit-compiler/releases/latest/download/moonbit-wasm.tar.gz
tar -zxvf moonbit-wasm.tar.gz
mkdir -p $HOME/.moon
MOON_VERSION=$(cat ./moon_version)
MOON_HOME="$HOME/.moon"
BIN_DIR="$MOON_HOME/bin"
mkdir -p "$BIN_DIR"
git clone https://github.com/moonbitlang/moon
cd moon
git reset --hard "$MOON_VERSION"
cargo build --release
cp target/release/moon "$BIN_DIR"
cp target/release/moonrun "$BIN_DIR"
sed -i '1 i #!/usr/bin/env -S node --stack-size=4096' moonc.js
sed -i '1 i #!/usr/bin/env -S node --stack-size=4096' moonfmt.js
sed -i '1 i #!/usr/bin/env -S node --stack-size=4096' mooninfo.js
cp moonc.js moonfmt.js mooninfo.js moonc.assets moonfmt.assets mooninfo.assets "$BIN_DIR" -r
mv "$BIN_DIR/moonc.js" "$BIN_DIR/moonc"
mv "$BIN_DIR/moonfmt.js" "$BIN_DIR/moonfmt"
mv "$BIN_DIR/mooninfo.js" "$BIN_DIR/mooninfo"
chmod +x "$BIN_DIR/moonc"
chmod +x "$BIN_DIR/moonfmt"
chmod +x "$BIN_DIR/mooninfo"
cp lib include "$MOON_HOME"
CORE_VERSION=$(cat ./core_version)
git clone https://github.com/moonbitlang/core "$MOON_HOME/lib/core"
cd "$MOON_HOME/lib/core"
git reset --hard "$CORE_VERSION"
moon bundle --target all
```

## Contributing

The project is evolving extremely fast that it is not yet ready for massive community 
contributions. 

If you do have interest in contributing, thank you!

Please sign the [CLA](https://www.moonbitlang.com/cla/moonc) first.
For small bug fixes, you are welcome to send the patch to [our email](mailto:jichuruanjian@idea.edu.cn). For large contributions, it is recommended to open a discussion first in our [community forum](https://discuss.moonbitlang.com). 

## LICENSE

MoonBit adopts MoonBit Public License which is a relaxed SSPL (Server Side Public License) with two key exceptions:

-  Artifacts produced by the MoonBit compiler may be licensed by the user under any license of their choosing, users have the freedom to choose the license for their own MoonBit source code and generated artifacts.
- Modifications to the compiler are allowed for non-commercial purposes.
   
While we value openness, we chose the relaxed SSPL instead of a fully permissive license for two main reasons:

- MoonBit is still in its beta-preview stage. Introducing forks at this point could risk destabilizing the project. We aim to reach a more mature and stable status before welcoming community contributions.
- We want to safeguard against large cloud vendors leveraging our work for commercial purposes in a way that could undermine our efforts.


In the past two years, our team worked hard to improve MoonBit and its toolchain, staying true to our vision of creating a fast, simple, and efficient language. By open sourcing MoonBit, we would like to reassure our users that our team remains dedicated to MoonBit's pace of growth and innovation. We also want to ensure our users that MoonBit is not going to adopt [open-core](https://en.wikipedia.org/wiki/Open-core_model) model, all MoonBit users will get the best developed compiler and IDE support. MoonBit team will try to generate revenue through cloud hosting services and hardware SDKs in the longer term.

# Credits 

We are grateful for the support of the community. 
Special thanks to Jane Street for their excellent PPX libraries,
this repo has used some of their [PPX functions](./src/hash.c).

