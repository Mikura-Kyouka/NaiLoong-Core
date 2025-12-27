# NaiLoong Core
NaiLoong Core 是一个使用 LoongArch 32 Reduced 指令集的乱序超标量处理器核，使用 Chisel 语言编写，并作为 2025 年龙芯杯团队赛中，吉林大学我才是奶龙队的参赛作品。

## 特点
NaiLoong Core 支持 `LA32R` 指令集中除浮点指令之外绝大多数指令，能在龙芯实验箱上启动 Linux。

已实现：
- 16KB 流水化直接映射 ICache，32KB 直接映射 DCache（大小可配置）
- BTB、BHT、PHT 相结合的 BPU
- 前端4路译码，后端5路发射
- 两级流水的寄存器重命名
- 16项全相联 TLB MMU
- 部分发射队列乱序
- 访存队列
- 除浮点相关外所有 CSR

## 编译&使用方法
正确设置环境变量 `CHIPLAB_HOME` 后，进入 `chisel-playground`，使用
```bash
make chiplab
```
生成用于 `Verilator` 仿真的代码。

使用
```bash
make vivado
```
生成用于 `Vivado` 上板的代码。