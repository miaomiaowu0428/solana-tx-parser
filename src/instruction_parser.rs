use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Ident, LitStr, Result, Token, Type, braced, bracketed,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
};

/// 指令定义的解析结构
pub struct InstructionDef {
    pub program_id: LitStr,
    pub name: Ident,
    pub discriminator: Vec<u8>,
    pub accounts: Vec<AccountDef>,
    pub data_fields: Vec<DataFieldDef>,
    pub bound_event: Option<Ident>, // 可选的绑定事件类型
}

/// 账户定义
pub struct AccountDef {
    pub name: Ident,
    pub writable: bool,
    pub signer: bool,
}

/// 数据字段定义
pub struct DataFieldDef {
    pub name: Ident,
    pub ty: Type,
}

impl Parse for InstructionDef {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut program_id = None;
        let mut name = None;
        let mut discriminator = None;
        let mut accounts = Vec::new();
        let mut data_fields = Vec::new();
        let mut bound_event = None;

        while !input.is_empty() {
            let ident: Ident = input.parse()?;
            input.parse::<Token![:]>()?;

            match ident.to_string().as_str() {
                "program_id" => {
                    program_id = Some(input.parse::<LitStr>()?);
                }
                "name" => {
                    name = Some(input.parse::<Ident>()?);
                }
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
                "bound_event" => {
                    bound_event = Some(input.parse::<Ident>()?);
                }
                "accounts" => {
                    let content;
                    braced!(content in input);

                    while !content.is_empty() {
                        let account_name: Ident = content.parse()?;
                        content.parse::<Token![:]>()?;

                        let account_content;
                        braced!(account_content in content);

                        let mut writable = false;
                        let mut signer = false;

                        while !account_content.is_empty() {
                            let prop: Ident = account_content.parse()?;
                            account_content.parse::<Token![:]>()?;
                            let value: syn::LitBool = account_content.parse()?;

                            match prop.to_string().as_str() {
                                "writable" => writable = value.value,
                                "signer" => signer = value.value,
                                _ => {
                                    return Err(syn::Error::new(
                                        prop.span(),
                                        "Unknown account property",
                                    ));
                                }
                            }

                            if !account_content.is_empty() {
                                account_content.parse::<Token![,]>()?;
                            }
                        }

                        accounts.push(AccountDef {
                            name: account_name,
                            writable,
                            signer,
                        });

                        if !content.is_empty() {
                            content.parse::<Token![,]>()?;
                        }
                    }
                }
                "data" => {
                    let content;
                    braced!(content in input);

                    while !content.is_empty() {
                        let field_name: Ident = content.parse()?;
                        content.parse::<Token![:]>()?;
                        let field_type: Type = content.parse()?;

                        data_fields.push(DataFieldDef {
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

        Ok(InstructionDef {
            program_id: program_id
                .ok_or_else(|| syn::Error::new(input.span(), "Missing program_id"))?,
            name: name.ok_or_else(|| syn::Error::new(input.span(), "Missing name"))?,
            discriminator: discriminator
                .ok_or_else(|| syn::Error::new(input.span(), "Missing discriminator"))?,
            accounts,
            data_fields,
            bound_event,
        })
    }
}

pub fn parse_instruction(input: TokenStream) -> TokenStream {
    let instruction_def = parse_macro_input!(input as InstructionDef);

    let name = &instruction_def.name;
    let program_id_str = &instruction_def.program_id;
    let discriminator = &instruction_def.discriminator;

    // 生成账户字段
    let account_fields = instruction_def.accounts.iter().map(|account| {
        let field_name = &account.name;
        quote! {
            pub #field_name: solana_sdk::pubkey::Pubkey,
        }
    });

    // 生成数据字段
    let data_field_tokens = instruction_def.data_fields.iter().map(|field| {
        let field_name = &field.name;
        let field_type = &field.ty;
        quote! {
            pub #field_name: #field_type,
        }
    });
    let data_fields = &instruction_def.data_fields;

    // 生成账户元数据 (Into<Instruction> 使用)
    let account_metas = instruction_def.accounts.iter().map(|account| {
        let field_name = &account.name;
        let writable = account.writable;
        let signer = account.signer;

        if writable {
            quote! {
                solana_sdk::instruction::AccountMeta::new(self.#field_name, #signer),
            }
        } else {
            quote! {
                solana_sdk::instruction::AccountMeta::new_readonly(self.#field_name, #signer),
            }
        }
    });

    // === 新增/修改: 针对 CompiledInstruction 的账户解析 (旧功能) ===
    let compiled_account_parsing = instruction_def
        .accounts
        .iter()
        .enumerate()
        .map(|(i, account)| {
            let field_name = &account.name;
            quote! {
                #field_name: accounts[instruction.accounts[#i] as usize],
            }
        });

    // === 新增: 针对 IndexedInstruction 的账户解析 (新功能) ===
    let indexed_account_parsing = instruction_def
        .accounts
        .iter()
        .enumerate()
        .map(|(i, account)| {
            let field_name = &account.name;
            quote! {
                #field_name: accounts[#i],
            }
        });

    let account_count = instruction_def.accounts.len();
    let has_data_fields = !instruction_def.data_fields.is_empty();

    // 如果有数据字段，生成一个单独的数据结构体
    let data_struct = if has_data_fields {
        let data_struct_name = quote::format_ident!("{}Data", name);
        let data_field_defs = data_fields.iter().map(|field| {
            let field_name = &field.name;
            let field_type = &field.ty;
            quote! {
                pub #field_name: #field_type,
            }
        });
        quote! {
            #[derive(Debug, Clone, borsh::BorshDeserialize)]
            struct #data_struct_name {
                #(#data_field_defs)*
            }
        }
    } else {
        quote! {}
    };

    // 生成默认账户解析（用于 from_full_data）
    let default_account_parsing = instruction_def.accounts.iter().map(|account| {
        let field_name = &account.name;
        quote! {
            #field_name: solana_sdk::pubkey::Pubkey::default(),
        }
    });

    // === 修改: 针对 CompiledInstruction 的指令解析 (旧功能) ===
    let compiled_instruction_parsing = if has_data_fields {
        let data_struct_name = quote::format_ident!("{}Data", name);
        let data_parsing = instruction_def.data_fields.iter().map(|field| {
            let field_name = &field.name;
            quote! { #field_name: parsed_data.#field_name, }
        });
        
        quote! {
            let data_bytes = &instruction.data[DISCRIMINATOR.len()..];
            let parsed_data = match borsh1::try_from_slice_unchecked::<#data_struct_name>(data_bytes) {
                Ok(data) => data,
                Err(_) => return None,
            };
            Some(Self {
                #(#compiled_account_parsing)*
                #(#data_parsing)*
                slot: 0, // slot 在 from 函数中设置
            })
        }
    } else {
        quote! {
            Some(Self {
                #(#compiled_account_parsing)*
                slot: 0, // slot 在 from 函数中设置
            })
        }
    };

    // === 新增: 针对 IndexedInstruction 的指令解析 (新功能) ===
    let indexed_instruction_parsing = if has_data_fields {
        let data_struct_name = quote::format_ident!("{}Data", name);
        let data_parsing = instruction_def.data_fields.iter().map(|field| {
            let field_name = &field.name;
            quote! { #field_name: parsed_data.#field_name, }
        });

        quote! {
            // 跳过 discriminator，解析剩余的数据
            let data_bytes = &instruction.data[DISCRIMINATOR.len()..];
            let parsed_data = match borsh1::try_from_slice_unchecked::<#data_struct_name>(data_bytes) {
                Ok(data) => data,
                Err(e) => {
                    log::info!("指令数据Borsh反序列化失败: {:?}", e);
                    return None;
                }
            };

            Some(Self {
                #(#indexed_account_parsing)* // <--- 使用新解析
                #(#data_parsing)*
                slot: indexed_instruction.slot, // <--- 使用 indexed_instruction.slot
            })
        }
    } else {
        quote! {
            Some(Self {
                #(#indexed_account_parsing)* // <--- 使用新解析
                slot: indexed_instruction.slot, // <--- 使用 indexed_instruction.slot
            })
        }
    };

    // 为 from_full_data 生成指令解析逻辑 (保持不变)
    let instruction_parsing_for_full_data_with_slot = if has_data_fields {
        let data_struct_name = quote::format_ident!("{}Data", name);
        // 生成数据字段解析
        let data_parsing = instruction_def.data_fields.iter().map(|field| {
            let field_name = &field.name;
            quote! {
                #field_name: parsed_data.#field_name,
            }
        });

        quote! {
            let parsed_data = match borsh1::try_from_slice_unchecked::<#data_struct_name>(data_bytes) {
                Ok(data) => data,
                Err(e) => {
                    log::info!("{} 从完整数据解析失败: {:?}", stringify!(#name), e);
                    log::info!("数据 hex: {:02x?}", data_bytes);
                    return None;
                }
            };

            // 注意：from_full_data 不包含账户信息，账户字段将使用默认值
            Some(Self {
                // 账户字段使用默认值（空的 Pubkey）
                #(#default_account_parsing)*
                #(#data_parsing)*
                slot: slot,
            })
        }
    } else {
        quote! {
            // 没有数据字段，只返回默认账户
            Some(Self {
                #(#default_account_parsing)*
                slot: slot,
            })
        }
    };

    // 生成绑定事件信息 (保持不变)
    let if_bound_event = if let Some(ref event_type) = instruction_def.bound_event {
        let event_type_str = event_type.to_string();
        quote! {
            Some(#event_type_str)
        }
    } else {
        quote! {
            None
        }
    };

    let expanded = quote! {
        #data_struct

        #[derive(Debug, Clone)]
        pub struct #name {
            #(#account_fields)*
            #(#data_field_tokens)*
            /// 指令发生的 slot 号（自动添加）
            pub slot: u64,
        }

        impl #name {
            /// 返回指令的 discriminator (保持不变)
            pub fn discriminator() -> &'static [u8] {
                &[#(#discriminator),*]
            }

            /// 返回绑定的事件类型名称（如果有的话）(保持不变)
            pub fn bound_event_type() -> Option<&'static str> {
                #if_bound_event
            }

            /// 从交易中解析所有匹配的指令 (保持不变)
            pub fn from(tx: &grpc_client::TransactionFormat) -> Vec<Self> {
                let mut results = Vec::new();
                let accounts = &tx.account_keys;

                // 检查主指令
                for instruction in tx.transation.message.instructions() {
                    if let Some(mut parsed) = Self::parse_instruction(instruction, accounts) {
                        parsed.slot = tx.slot;
                        results.push(parsed);
                    }
                }

                // 检查内部指令
                if let Some(ref meta) = tx.meta {
                    if let Some(ref inner_instructions) = meta.inner_instructions {
                        for inner_ix_group in inner_instructions {
                            for inner_instruction in &inner_ix_group.instructions {
                                if let Some(mut parsed) = Self::parse_instruction(&inner_instruction.instruction, accounts) {
                                    parsed.slot = tx.slot;
                                    results.push(parsed);
                                }
                            }
                        }
                    }
                }

                results
            }

            /// 从完整数据解析指令（包含discriminator）(保持不变)
            pub fn from_full_data(data: &[u8]) -> Option<Self> {
                Self::from_full_data_with_slot(data, 0)
            }

            /// 从完整数据解析指令并设置 slot（包含discriminator）(保持不变)
            pub fn from_full_data_with_slot(data: &[u8], slot: u64) -> Option<Self> {
                const DISCRIMINATOR: &[u8] = &[#(#discriminator),*];

                if data.len() < DISCRIMINATOR.len() {
                    log::info!("{} 数据长度不足以包含 discriminator: {}", stringify!(#name), data.len());
                    return None;
                }

                // 检查 discriminator
                if &data[0..DISCRIMINATOR.len()] != DISCRIMINATOR {
                    log::info!("{} discriminator 不匹配", stringify!(#name));
                    return None;
                }

                // 跳过 discriminator，解析剩余的数据
                let data_bytes = &data[DISCRIMINATOR.len()..];
                log::info!("{} 解析指令数据，长度: {}", stringify!(#name), data_bytes.len());

                // 解析数据字段（如果有的话）
                #instruction_parsing_for_full_data_with_slot
            }

            /// 旧功能：从 CompiledInstruction 解析指令 (针对 solana_sdk::message::compiled_instruction)
            fn parse_instruction(
                instruction: &solana_sdk::message::compiled_instruction::CompiledInstruction,
                accounts: &[solana_sdk::pubkey::Pubkey],
            ) -> Option<Self> {
                // 检查 discriminator
                const DISCRIMINATOR: &[u8] = &[#(#discriminator),*];
                if instruction.data.len() < DISCRIMINATOR.len()
                    || &instruction.data[0..DISCRIMINATOR.len()] != DISCRIMINATOR {
                    return None;
                }

                // 检查程序地址
                let program_id_str = #program_id_str;
                let expected_program_id = program_id_str.parse::<solana_sdk::pubkey::Pubkey>()
                    .expect("Invalid program_id format");
                let actual_program_id = accounts[instruction.program_id_index as usize];
                if actual_program_id != expected_program_id {
                    return None;
                }

                // 检查账户数量
                if instruction.accounts.len() < #account_count {
                    return None;
                }

                #compiled_instruction_parsing
            }
            
            /// 新功能：从 IndexedInstruction 解析指令 (针对 utils::IndexedInstruction)
            fn from_indexed_instruction(indexed_instruction: utils::IndexedInstruction) -> Option<Self, > {
                // accounts 是 ParsedInstruction.accounts 的引用
                let accounts = &indexed_instruction.instruction.accounts;
                // instruction 是 ParsedInstruction 的引用
                let instruction = &indexed_instruction.instruction;

                // 检查 discriminator
                const DISCRIMINATOR: &[u8] = &[#(#discriminator),*];
                if instruction.data.len() < DISCRIMINATOR.len()
                    || &instruction.data[0..DISCRIMINATOR.len()] != DISCRIMINATOR {
                    return None;
                }

                // 检查程序地址
                let program_id_str = #program_id_str;
                let expected_program_id = program_id_str.parse::<solana_sdk::pubkey::Pubkey>()
                    .expect("Invalid program_id format");
                // 直接使用 instruction.program 字段
                if instruction.program != expected_program_id {
                    return None;
                }

                // 检查账户数量
                if instruction.accounts.len() < #account_count {
                    return None;
                }
                
                #indexed_instruction_parsing
            }
        }

        impl Into<solana_sdk::instruction::Instruction> for #name {
            fn into(self) -> solana_sdk::instruction::Instruction {
                let program_id_str = #program_id_str;
                let program_id = program_id_str.parse::<solana_sdk::pubkey::Pubkey>()
                    .expect("Invalid program_id format");

                solana_sdk::instruction::Instruction {
                    program_id,
                    accounts: vec![
                        #(#account_metas)*
                    ],
                    data: vec![#(#discriminator),*],
                }
            }
        }
    };

    TokenStream::from(expanded)
}