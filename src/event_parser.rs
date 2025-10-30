use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Ident, Result, Token, Type, braced, bracketed,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
};

/// 事件定义的解析结构
pub struct EventDef {
    pub discriminator: Vec<u8>,
    pub name: Ident,
    pub program_id: syn::Expr, // 改为必填，不再是 Option
    pub fields: Vec<FieldDef>,
}

/// 字段定义
pub struct FieldDef {
    pub name: Ident,
    pub ty: Type,
}

impl Parse for EventDef {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut discriminator = None;
        let mut name = None;
        let mut program = None;
        let mut fields = Vec::new();

        while !input.is_empty() {
            let ident: Ident = input.parse()?;
            input.parse::<Token![:]>()?;

            match ident.to_string().as_str() {
                "discriminator" => {
                    let content;
                    bracketed!(content in input);
                    let values: Punctuated<syn::LitInt, Token![,]> =
                        content.parse_terminated(syn::LitInt::parse, Token![,])?;

                    let disc_bytes: Result<Vec<u8>> = values
                        .iter()
                        .map(|lit| {
                            lit.base10_parse::<u8>().map_err(|_| {
                                syn::Error::new(lit.span(), "Invalid discriminator byte")
                            })
                        })
                        .collect();

                    discriminator = Some(disc_bytes?);
                }
                "name" => {
                    name = Some(input.parse::<Ident>()?);
                }
                "program" => {
                    program = Some(input.parse::<syn::Expr>()?);
                }
                "fields" => {
                    let content;
                    braced!(content in input);

                    while !content.is_empty() {
                        let field_name: Ident = content.parse()?;
                        content.parse::<Token![:]>()?;
                        let field_type: Type = content.parse()?;

                        fields.push(FieldDef {
                            name: field_name,
                            ty: field_type,
                        });

                        if !content.is_empty() {
                            content.parse::<Token![,]>()?;
                        }
                    }
                }
                _ => return Err(syn::Error::new(ident.span(), "Unknown field")),
            }

            if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
        }

        Ok(EventDef {
            discriminator: discriminator
                .ok_or_else(|| syn::Error::new(input.span(), "Missing discriminator"))?,
            name: name.ok_or_else(|| syn::Error::new(input.span(), "Missing name"))?,
            program_id: program.ok_or_else(|| syn::Error::new(input.span(), "Missing program"))?,
            fields,
        })
    }
}

pub fn parse_event(input: TokenStream) -> TokenStream {
    let event_def = parse_macro_input!(input as EventDef);

    let name = &event_def.name;
    let discriminator = &event_def.discriminator;
    let program_id = &event_def.program_id;

    // 生成字段
    let event_fields = event_def.fields.iter().map(|field| {
        let field_name = &field.name;
        let field_type = &field.ty;
        quote! {
            pub #field_name: #field_type,
        }
    });

    // 生成程序地址检查代码（现在是必填的）
    let program_check = quote! {
        // 检查程序地址
        const EXPECTED_PROGRAM_ID: solana_sdk::pubkey::Pubkey = #program_id;
        if (inner_instruction.instruction.program_id_index as usize) >= tx.account_keys.len() {
            continue;
        }
        let actual_program_id = &tx.account_keys[inner_instruction.instruction.program_id_index as usize];
        if actual_program_id != &EXPECTED_PROGRAM_ID {
            continue;
        }
    };

    let expanded = quote! {
        #[derive(Debug, Clone, borsh::BorshDeserialize)]
        pub struct #name {
            #(#event_fields)*
            /// 事件发生的 slot 号（自动添加）
            #[borsh(skip)]
            pub slot: u64,
            /// 事件来源的交易sig（自动添加）
            #[borsh(skip)]
            pub sig: solana_sdk::signature::Signature,
        }

        impl #name {
            /// 返回事件的 discriminator
            pub fn discriminator() -> &'static [u8] {
                &[#(#discriminator),*]
            }

            /// 从交易中解析所有匹配的事件
            pub fn from(tx: &grpc_client::TransactionFormat) -> Vec<Self> {
                let mut results = Vec::new();

                // 检查内部指令中的 CPI 日志
                if let Some(ref meta) = tx.meta {
                    if let Some(ref inner_instructions) = meta.inner_instructions {
                        for inner_ix_group in inner_instructions {
                            for inner_instruction in &inner_ix_group.instructions {
                                #program_check
                                if let Some(mut event) = Self::check_cpi_log(&inner_instruction.instruction, &tx.signature.to_string()) {
                                    // 设置 slot 号和sig
                                    event.slot = tx.slot;
                                    event.sig = tx.signature.clone();
                                    results.push(event);
                                }
                            }
                        }
                    }
                }

                results
            }

            /// 直接从数据解析事件
            pub fn from_data(data: &[u8]) -> Option<Self> {
                Self::from_data_with_slot(data, 0)
            }

            /// 从数据解析事件并设置 slot
            pub fn from_data_with_slot(data: &[u8], slot: u64) -> Option<Self> {
                // 使用 borsh 反序列化事件数据
                match borsh1::try_from_slice_unchecked::<Self>(data) {
                    Ok(mut event) => {
                        event.slot = slot;
                        event.sig = solana_sdk::signature::Signature::default();
                        Some(event)
                    },
                    Err(e) => {
                        log::info!("{} 解析失败: {:?}", stringify!(#name), e);
                        None
                    }
                }
            }

            /// 从完整数据解析事件（包含16字节标识符）
            pub fn from_full_data(data: &[u8]) -> Option<Self> {
                Self::from_full_data_with_slot(data, 0)
            }

            /// 从完整数据解析事件并设置 slot（包含16字节标识符）
            pub fn from_full_data_with_slot(data: &[u8], slot: u64) -> Option<Self> {
                // Anchor self CPI log 的标识符 (前8字节)
                const ANCHOR_SELF_CPI_LOG: &[u8] = &[228, 69, 165, 46, 81, 203, 154, 29];
                const EVENT_DISCRIMINATOR: &[u8] = &[#(#discriminator),*];

                if data.len() < 16 {
                    // log::info!("{} 数据长度不足16字节: {}", stringify!(#name), data.len());
                    return None;
                }

                // 检查前8字节是否是 Anchor self CPI log 标识符
                if &data[0..8] != ANCHOR_SELF_CPI_LOG {
                    // log::info!("{} Anchor self CPI log 标识符不匹配", stringify!(#name));
                    return None;
                }

                // 检查第9-16字节是否匹配事件 discriminator
                if data.len() < 8 + EVENT_DISCRIMINATOR.len() {
                    // log::info!("{} 数据长度不足以包含事件 discriminator", stringify!(#name));
                    return None;
                }

                if &data[8..8 + EVENT_DISCRIMINATOR.len()] != EVENT_DISCRIMINATOR {
                    // log::info!("{} 事件 discriminator 不匹配", stringify!(#name));
                    return None;
                }

                // 解析事件数据 (从第17字节开始，跳过前16字节的标识符)
                let event_data = &data[8+EVENT_DISCRIMINATOR.len()..];
                // log::info!("{} 解析事件数据，长度: {}", stringify!(#name), event_data.len());

                // 使用 borsh 反序列化事件数据
                match borsh1::try_from_slice_unchecked::<Self>(event_data) {
                    Ok(mut event) => {
                        event.slot = slot;
                        event.sig = solana_sdk::signature::Signature::default();
                        Some(event)
                    },
                    Err(e) => {
                        log::info!("{} 从完整数据解析失败: {:?}", stringify!(#name), e);
                        log::info!("事件数据 hex: {:02x?}", event_data);
                        None
                    }
                }
            }

            /// 带调试信息的数据解析方法（内部使用）
            fn from_data_with_debug(data: &[u8], tx_signature: &str) -> Option<Self> {
                // 使用 borsh 反序列化事件数据
                match borsh1::try_from_slice_unchecked::<Self>(data) {
                    Ok(mut event) => {
                        // 这里 slot 暂时设为 0，会在调用方设置正确的值
                        event.slot = 0;
                        event.sig = tx_signature.parse().unwrap_or_default();
                        Some(event)
                    },
                    Err(e) => {
                        log::info!("{} 解析失败 (tx: {}): {:?}", stringify!(#name), tx_signature, e);
                        log::info!("data hex: {:02x?}", data);
                        None
                    }
                }
            }

            fn check_cpi_log(
                instruction: &solana_sdk::message::compiled_instruction::CompiledInstruction,
                tx_signature: &str,
            ) -> Option<Self> {
                // Anchor self CPI log 的标识符 (前8字节)
                const ANCHOR_SELF_CPI_LOG: &[u8] = &[228, 69, 165, 46, 81, 203, 154, 29];
                const EVENT_DISCRIMINATOR: &[u8] = &[#(#discriminator),*];

                if instruction.data.len() < 16 {
                    return None;
                }

                // 检查前8字节是否是 Anchor self CPI log 标识符
                if &instruction.data[0..8] != ANCHOR_SELF_CPI_LOG {
                    return None;
                }

                // 检查第9-16字节是否匹配事件 discriminator
                if instruction.data.len() < 8 + EVENT_DISCRIMINATOR.len() {
                    return None;
                }

                if &instruction.data[8..8 + EVENT_DISCRIMINATOR.len()] != EVENT_DISCRIMINATOR {
                    return None;
                }

                // 两段标识符都匹配成功，打印前16字节原始数据用于调试
                // log::info!("前16字节 u8: {:?}, hex: {:02x?}", &instruction.data[0..16], &instruction.data[0..16]);

                // 解析事件数据 (从第17字节开始，跳过前16字节的标识符)
                let event_data = &instruction.data[8+EVENT_DISCRIMINATOR.len()..];
                // log::info!("事件数据长度: {}, 数据: {:?}", event_data.len(), event_data);

                // 调用带调试信息的解析方法
                Self::from_data_with_debug(event_data, tx_signature)
            }
        }
    };

    TokenStream::from(expanded)
}
