use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Fields};

#[proc_macro_attribute]
pub fn make_partial(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);

    let struct_name = &input.ident;
    let partial_name = format_ident!("Partial{}", struct_name);
    let generics = &input.generics;
    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let fields = match &input.data {
        Data::Struct(data) => match &data.fields {
            Fields::Named(fields) => &fields.named,
            _ => panic!("make_partial only supports structs with named fields"),
        },
        _ => panic!("make_partial only supports structs"),
    };

    let partial_fields: Vec<_> = fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            let ty = &f.ty;
            quote! {
                #name: TomlValue<#ty>
            }
        })
        .collect();

    // let from_toml_fields: Vec<_> = fields
    //     .iter()
    //     .map(|f| {
    //         let name = &f.ident;
    //         let name_str = name.as_ref().unwrap().to_string();
    //         quote! {
    //             #name: helper.get(#name_str)
    //         }
    //     })
    //     .collect();

    let collect_errors_fields: Vec<_> = fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            quote! {
                self.#name.register_error(errors)
            }
        })
        .collect();

    let can_concrete_fields: Vec<_> = fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            quote! {
                self.#name.has_value()
            }
        })
        .collect();

    let to_concrete_fields: Vec<_> = fields
        .iter()
        .map(|f| {
            let name = &f.ident;
            quote! {
                #name: self.#name.unwrap()
            }
        })
        .collect();

    let expanded = quote! {
        //#[derive(Debug)]
        #input

        #[derive(Debug)]
        struct #partial_name #generics #where_clause {
            #(#partial_fields,)*
        }

        // impl #impl_generics FromTomlTable<()> for #partial_name #ty_generics #where_clause {
        //     fn from_toml_table(helper: &mut TomlHelper<'_>, _partial: &()) -> Self {
        //         #partial_name {
        //             #(#from_toml_fields,)*
        //         }
        //     }
        // }

        impl #impl_generics ToConcrete<#struct_name #ty_generics> for #partial_name #ty_generics #where_clause {
            fn collect_errors(&self, errors: &Rc<RefCell<Vec<AnnotatedError>>>) {
                #(#collect_errors_fields;)*
            }

            fn can_concrete(&self) -> bool {
                #(#can_concrete_fields)&&*
            }

            fn to_concrete(self) -> Option<#struct_name #ty_generics> {
                Some(#struct_name {
                    #(#to_concrete_fields,)*
                })
            }
        }

        // impl #impl_generics Default for #partial_name #ty_generics #where_clause {
        //     fn default() -> Self {
        //         Self {
        //             #(#from_toml_fields,)*
        //         }
        //     }
        // }

        impl #impl_generics PartialEq for #partial_name #ty_generics #where_clause {
            fn eq(&self, other: &Self) -> bool {
                true
            }
        }
    };

    TokenStream::from(expanded)
}
