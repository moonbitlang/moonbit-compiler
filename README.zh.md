<div align="center">
  <picture>
    <img src="logo.png" height="400px"/>
  </picture> 

  <h1> MoonBit 编译器

[MoonBit] | [文档] | [概览] | [标准库]
</div>

这里是 [MoonBit] 的编译器。MoonBit 是一个用户友好，构建快，产出质量高的编程语言。

[Moonbit]: https://www.moonbitlang.cn
[概览]: https://tour.moonbitlang.com
[文档]: https://docs.moonbitlang.com/zh-cn/
[标准库]: https://github.com/moonbitlang/core

## 路线图

构建一个编程语言是一个漫长的旅程。Rust 和 Go 分别用了 9 年和 5 年到达 1.0 版本。MoonBit 由一个年轻而有干劲的团队开发，正在稳步前进。我们明白，社区的采用和扩展对编程语言来说十分关键，并且我们也致力于打造一个围绕 MoonBit 的积极参与、合作共赢的社区。到目前为止，我们已经开源了[标准库](https://github.com/moonbitlang/core)和绝大多数工具，包括[构建系统](https://github.com/moonbitlang/moon)，[词法分析](https://github.com/moonbit-community/moonlex)，[markdown 解析](https://github.com/moonbit-community/cmark)等，将来还会有更多项目。开放编译器源代码对于安全来说十分重要。开源 Wasm 后端是重要一步，并且我们计划在将来开源更多组建（ moonfmt、moondoc ）。


## 从源代码构建

### 开发环境

- OCaml 4.14.2
- [OPAM](https://opam.ocaml.org/)

### 构建

使用下列脚本构建

```
opam switch create 4.14.2
opam install -y dune
dune build -p moonbit-lang
```

## 使用wasm版的MoonBit编译器

首先需要安装nodejs和curl, 然后在任意的临时目录下执行以下命令

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

## 贡献

这个项目正在快速演进，因此还没有准备好接受大量社区贡献。

如果你有兴趣贡献，首先，十分感谢！

请签署 [CLA](https://www.moonbitlang.com/cla/moonc)。
对于小的 Bug 修复，欢迎向[我们的邮箱](mailto:jichuruanjian@idea.edu.cn)发送补丁。对于大的贡献，推荐先在[我们的论坛](https://discuss.moonbitlang.com)进行讨论。

## 许可证

MoonBit 采用 MoonBit Public License，一个放宽的 SSPL (Server Side Public License)。有两个关键的区别：

- 用户可以任意选择许可证来对 MoonBit 编译器构建的产物进行许可。用户可以自由使用他们的 MoonBit 源代码以及生成的产物。
- 允许以非商业目的对编译器的修改。
   
虽然我们拥抱开放，出于下列两个原因，我们没有选择完全开放的许可证，而是选择了放宽后的 SSPL:

- MoonBit 依然在 beta-preview 的阶段。在这个阶段引入分叉可能影响项目的稳定。我们希望达到一个更成熟、更稳定的状态后接受社区贡献。
- 我们希望避免大型云服务商利用团队的成果进行商业化。

在过去两年中，我们的团队努力改进 MoonBit 和它的工具链，始终守护我们的愿景：开发一个快速、简单、高效的编程语言。通过开源 MoonBit，我们希望可以让我们的用户相信，我们致力于 MoonBit 的增长和创新。我们同时希望我们的用户可以放心，MoonBit 不会采用 [open-core 的模式](https://en.wikipedia.org/wiki/Open-core_model)，所有的 MoonBit 用户都会获得最好的开发编译器和 IDE 支持。 MoonBit 团队的愿景是通过云平台服务以及硬件 SDK 等来获得持续增长。

## 致谢

我们十分感谢社区对我们的支持。  
特别感谢 Jane Street 的优秀的 PPX 库，这个仓库使用了一些他们的 [PPX 函数](./src/hash.c)。

