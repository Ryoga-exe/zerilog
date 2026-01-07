# Zerilog Reference Manual

## 1. Introduction

Zerilog は Verilog / SystemVerilog の置き換えを目指したハードウェア記述言語です。
Zig 言語をベースにした記法・文法を採用しています。

現在は SystemVerilog へのトランスパイルを目標にしています。

## 2. Basic Concept

Zig 言語をベースに、SystemVerilog 風の文法でコードを記述します。
拡張子は `.zer` です。

```zer
const Clock = clock_posedge;
const Reset = reset_async_low;

pub module Counter (
    clk: input Clock,
    rst: input Reset,
) {
    var r: logic8;

    always_ff {
        if_reset {
            r = 0;
        } else {
            r += 1;
        }
    }
}
```

このコードは

```sv
module Counter (
    input logic clk,
    input logic rst,
);
    logic [7:0] r;

    always_ff @(posedge clk or negedge rst) begin
        if (!rst) begin
            r <= 0;
        end else begin
            r <= r + 1;
        end
    end
endmodule
```

このようなコードに変換されます。

### 組込み型

- `logic`:
  - 幅指定可能な4値データ型です。
  - 幅は `logic8` のように `logic` の後に数字を書くことで指定できます。
- `bit`:
  - 幅指定可能な2値データ型です。
  - 幅は `bit8` のように、`bit` の後に数字を書くことで指定できます。
- `slogic`/`sbit`:
  - MSB は符号ビットとして扱われます。
  - `slogic8` のように幅を指定します。

TODO: tri (トライステート型)

クロック型やリセット型

- `clock_posedge`/`clock_negedge`:
  - `clock` 型です。
  - `always_ff` 内で自動的に使用されます。
- `reset_async_low`/`reset_async_high`/`reset_sync_low`/`reset_sync_high`:
  - `reset` 型です。
  - `always_ff` や `if_reset` で自動的に使用されます。
- `anyclock`/`anyreset`:
  - 与えられた引数の型によって `always_ff` などに使われます。
  - Zig の `anytype` のように働きます。
