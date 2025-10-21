use proc_macro::TokenStream;

mod account_data_parser;
mod event_parser;
mod instruction_parser;

#[proc_macro]
pub fn instruction(input: TokenStream) -> TokenStream {
    instruction_parser::parse_instruction(input)
}

#[proc_macro]
pub fn event(input: TokenStream) -> TokenStream {
    event_parser::parse_event(input)
}

#[proc_macro]
pub fn account_data(input: TokenStream) -> TokenStream {
    account_data_parser::parse_account_data(input)
}
