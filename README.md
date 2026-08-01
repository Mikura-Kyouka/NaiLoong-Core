# NaiLoong Core

NaiLoong Core 是一个以 Chisel 编写的 `LoongArch32 Reduced`（LA32R）乱序超标量处理器核。

通过本项目的构建系统可生成 SystemVerilog，接入 ChipLab 使用 Verilator 进行仿真，也可接入 Vivado 工程进行上板流程。当前版本可在龙芯实验箱上以约 80 MHz 稳定运行“龙芯杯”性能测试，并启动 Linux。

## 架构概览

### ISA 相关

指令集支持 LA32R，覆盖除浮点相关指令外的大多数指令，并实现 CSR、异常和中断相关支持。

### 前端

4 路取指、译码，BPU 采用 BTB、BHT、PHT 与 RAS 组合预测。

### 后端

后端采用两级寄存器重命名、5 路发射（2 ALU、1 MDU、1 LSU、1 BRU）、部分乱序发射队列。

### 乱序相关参数

64 个物理寄存器、32 项 ROB，每周期提交最多 2 条指令。

### 访存子系统

IFU 含 16 KiB、直接映射、流水化 ICache。

LSU 含 64 KiB、2 路组相联、写回法 DCache，并配合访存队列。

16 项全相联 TLB/MMU。

32 位 AXI 主设备接口。

## 仓库结构

```text
.
├── chisel-playground/          # Chisel/Mill 工程与构建入口
│   ├── Makefile                # SystemVerilog 生成及 ChipLab/Vivado 集成目标
│   ├── build.mill              # Scala、Chisel 依赖定义
│   └── playground/src/
│       ├── core/               # 前端、后端、Cache、执行单元与顶层 Core
│       ├── Mmu/                # TLB 与地址转换逻辑
│       ├── utils/              # 通用组件与 DiffTest 桥接模块
│       └── Elaborate.scala     # 生成 SystemVerilog 的入口
├── tools/                      # 性能测试及对拍相关辅助文件
└── temp/                       # 实验性代码
```

## 环境要求

- JDK 与 [Mill](https://mill-build.org/)；构建配置使用 Scala 2.13.18、Chisel 7.7.0。
- 若使用 ChipLab 流程，需要准备 ChipLab，并设置 `CHIPLAB_HOME`。
- 若使用 `solo` 或 `single` 目标，需要将 `SOLO_DIR` 指向相应 SoC 工程的 `rtl` 目录。

首次构建时，Mill 会下载 Scala/Chisel 依赖。

## 快速开始

进入 Chisel 工程后生成 SystemVerilog：

```bash
cd chisel-playground
make verilog
```

生成结果位于 `chisel-playground/build/`，以 `.sv` 文件形式输出。可通过以下命令查看生成器支持的选项：

```bash
make help
```

生成器的常用开关如下：

- `--use-diff`：启用 DiffTest 相关逻辑。
- `--use-simu`：使用仿真用 IP。
- `--use-count`：启用性能计数逻辑。

Makefile 中的各目标也会为对应场景设置合适的值。

## ChipLab 与上板集成

设置 ChipLab 根目录后，可使用以下目标：

```bash
export CHIPLAB_HOME=/path/to/ChipLab
cd chisel-playground

# 生成含 DiffTest、仿真 IP 的设计，并安装到 $CHIPLAB_HOME/IP/myCPU
make chiplab

# 生成用于 Vivado 上板的设计，并安装到 $CHIPLAB_HOME/IP/myCPU
make vivado
```

`chiplab` 和 `vivado` 会清理 `build/` 以及 `$CHIPLAB_HOME/IP/myCPU` 中已有的大部分生成文件，然后复制新的 `.sv` 文件和必要的桥接和顶层文件。请不要将未备份的手工修改保存在这些目录中。

## 许可证

许可证采用 [MIT License](LICENSE)。
