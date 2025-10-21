use proc_macro::TokenStream;
use quote::quote;
use syn::{
    Ident, Result, Token, Type, braced, bracketed,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
};

/// 账户数据定义的解析结构
pub struct AccountDataDef {
    pub discriminator: Vec<u8>,
    pub name: Ident,
    pub fields: Vec<FieldDef>,
}

/// 字段定义
pub struct FieldDef {
    pub name: Ident,
    pub ty: Type,
}

impl Parse for AccountDataDef {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut discriminator = None;
        let mut name = None;
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

        Ok(AccountDataDef {
            discriminator: discriminator
                .ok_or_else(|| syn::Error::new(input.span(), "Missing discriminator"))?,
            name: name.ok_or_else(|| syn::Error::new(input.span(), "Missing name"))?,
            fields,
        })
    }
}

/// 新的宏，支持 discriminator
pub fn parse_account_data(input: TokenStream) -> TokenStream {
    let account_def = parse_macro_input!(input as AccountDataDef);

    let name = &account_def.name;
    let discriminator = &account_def.discriminator;

    // 生成字段
    let account_fields = account_def.fields.iter().map(|field| {
        let field_name = &field.name;
        let field_type = &field.ty;
        quote! {
            pub #field_name: #field_type,
        }
    });

    let expanded = quote! {
        #[derive(Debug, Clone, borsh::BorshDeserialize)]
        pub struct #name {
            #(#account_fields)*
        }

        impl #name {
            /// 返回账户的 discriminator
            pub fn discriminator() -> &'static [u8] {
                &[#(#discriminator),*]
            }

            /// 从原始字节数据反序列化账户数据（不包含 discriminator）
            pub fn from_data(data: &[u8]) -> Result<Self, Box<dyn std::error::Error>> {
                borsh1::try_from_slice_unchecked::<Self>(data).map_err(|e| Box::new(e) as Box<dyn std::error::Error>)
            }

            /// 从完整数据解析账户（包含 discriminator）
            pub fn from_full_data(data: &[u8]) -> Result<Self, Box<dyn std::error::Error>> {
                const ACCOUNT_DISCRIMINATOR: &[u8] = &[#(#discriminator),*];

                if data.len() < ACCOUNT_DISCRIMINATOR.len() {
                    return Err(format!("数据长度不足以包含 discriminator: {} < {}",
                        data.len(), ACCOUNT_DISCRIMINATOR.len()).into());
                }

                // 检查 discriminator
                if &data[0..ACCOUNT_DISCRIMINATOR.len()] != ACCOUNT_DISCRIMINATOR {
                    return Err("Discriminator 不匹配".into());
                }

                // 解析账户数据 (跳过 discriminator)
                let account_data = &data[ACCOUNT_DISCRIMINATOR.len()..];
                Self::from_data(account_data)
            }

            /// 验证数据是否匹配此账户类型的 discriminator
            pub fn verify_discriminator(data: &[u8]) -> bool {
                const ACCOUNT_DISCRIMINATOR: &[u8] = &[#(#discriminator),*];

                if data.len() < ACCOUNT_DISCRIMINATOR.len() {
                    return false;
                }

                &data[0..ACCOUNT_DISCRIMINATOR.len()] == ACCOUNT_DISCRIMINATOR
            }

            /// 从 RPC 客户端直接获取并解析账户数据
            pub async fn fetch(
                rpc_client: &solana_client::nonblocking::rpc_client::RpcClient,
                address: &solana_sdk::pubkey::Pubkey
            ) -> Result<Self, Box<dyn std::error::Error + Send + Sync>> {
                let account = rpc_client.get_account(address).await
                    .map_err(|e| format!("获取账户失败: {}", e))?;

                Self::from_full_data(&account.data)
                    .map_err(|e| format!("解析账户数据失败: {}", e).into())
            }
        }
    };

    TokenStream::from(expanded)
}
