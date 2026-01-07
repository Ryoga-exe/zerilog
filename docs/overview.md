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
