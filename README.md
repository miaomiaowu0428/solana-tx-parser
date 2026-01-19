```rust
use solana_sdk::borsh1;
/// 通过此格式定义合约指令解析器
/// 使用类型名下的from_indexed_instruction(ix)->Option<T>方法匹配
/// 成功为 Some(T) 失败为 None
/// 配合Iterator接口可快速匹配模式
instruction!(
    program_id: "11111111111111111111111111111111",
    name: Transfer,
    discriminator: [0x02,0x00,0x00,0x00],
    accounts: {
        from: {
            writable: true,
            signer: true
        },
        to: {
            writable: true,
            signer: false
        }
    },
    data: {
        lamports: u64,
    },
);

```