extern crate proc_macro;
extern crate proc_macro2;
#[macro_use]
extern crate quote;
extern crate strfmt;
#[macro_use]
extern crate syn;

use std::collections::HashMap;
use std::str::FromStr;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use strfmt::strfmt;
use syn::{AttrStyle, Attribute, Data, DeriveInput, Fields, Lit, Meta, MetaNameValue, NestedMeta};

const NAME_SEPARATOR: char = '.';

/// A proc-macro attribute, which can format documentation strings with specified attributes.
///
/// Write documentation using `{variable}`,
/// ```
/// # #![feature(custom_attribute)]
/// # extern crate doc_macro;
/// # use doc_macro::format_doc;
/// #
/// #[format_doc]
/// enum SomeEnum {
///     #[owner(name = "Boncheol Gu", number = "000-0000-0000")]
///     /// The owner of Variant1 is {owner.name}, of #{owner.number}.
///     Variant1,
///     #[owner(name = "Junmo Gu", number = "000-0000-0000")]
///     /// The owner of Variant2 is {owner.name}, of #{owner.number}.
///     Variant2,
/// }
/// ```
/// Documentation comments are filled with `name` and `number` attributes as follows:
/// ```
/// # #![feature(custom_attribute)]
/// # extern crate doc_macro;
/// # use doc_macro::format_doc;
/// #
/// #[format_doc]
/// enum SomeEnum {
///     #[owner(name = "Boncheol Gu", number = "000-0000-0000")]
///     /// The owner of Variant1 is Boncheol Gu, of #000-0000-0000.
///     Variant1,
///     #[owner(name = "Junmo Gu", number = "000-0000-0000")]
///     /// The owner of Variant2 is Junmo Gu, of #000-0000-0000.
///     Variant2,
/// }
/// ```
#[proc_macro_attribute]
pub fn format_doc(_args: TokenStream, input: TokenStream) -> TokenStream {
    let mut input: DeriveInput = syn::parse(input).unwrap();

    format_doc_impl(&mut input.attrs);
    match input.data {
        Data::Struct(ref mut data_struct) => match data_struct.fields {
            Fields::Named(ref mut fields_named) => {
                for field in &mut fields_named.named {
                    format_doc_impl(&mut field.attrs);
                }
            }
            Fields::Unnamed(ref mut fields_unnamed) => {
                for field in &mut fields_unnamed.unnamed {
                    format_doc_impl(&mut field.attrs);
                }
            }
            _ => {}
        },
        Data::Enum(ref mut data_enum) => {
            for variant in &mut data_enum.variants {
                format_doc_impl(&mut variant.attrs);
            }
        }
        Data::Union(ref mut data_union) => {
            for field in &mut data_union.fields.named {
                format_doc_impl(&mut field.attrs);
            }
        }
    }

    let r = quote!{
        #input
    };
    r.into()
}

fn format_doc_impl(attrs: &mut [Attribute]) {
    let vars = get_attribute_map(attrs);

    for attribute in attrs {
        if attribute.path == parse_quote!{doc} {
            let text = strfmt(&attribute.tts.to_string(), &vars).unwrap();
            let text = TokenStream2::from_str(&text).unwrap();
            attribute.tts = quote! {
                #text
            }
        }
    }
}

fn get_attribute_map(attrs: &[Attribute]) -> HashMap<String, String> {
    let mut result = HashMap::new();

    for attr in attrs {
        if attr.style != AttrStyle::Outer {
            continue;
        }

        if let Some(meta) = attr.interpret_meta() {
            get_attribute_map_impl(&mut result, &meta, &meta.name().to_string());
        }
    }

    result
}

fn get_attribute_map_impl(set: &mut HashMap<String, String>, meta: &Meta, prefix: &str) {
    if let Meta::List(meta_list) = meta {
        for nested_meta in &meta_list.nested {
            if let NestedMeta::Meta(meta) = nested_meta {
                if let Meta::NameValue(MetaNameValue {
                    ref ident,
                    lit: Lit::Str(ref lit_str),
                    ..
                }) = meta
                {
                    set.insert(
                        format!("{}{}{}", prefix, NAME_SEPARATOR, ident),
                        lit_str.value(),
                    );
                } else {
                    get_attribute_map_impl(
                        set,
                        &meta,
                        &format!("{}{}{}", prefix, NAME_SEPARATOR, meta.name()),
                    );
                }
            }
        }
    }
}

#[allow(dead_code)]
fn get_attribute_value(attrs: &[Attribute], id: &str) -> Option<String> {
    let id: Vec<_> = id.split('.').collect();
    for attr in attrs {
        if attr.style != AttrStyle::Outer {
            continue;
        }

        if let Some(meta) = attr.interpret_meta() {
            if let Some(value) = get_attribute_value_impl(&meta, &id) {
                return Some(value);
            }
        }
    }

    None
}

fn get_attribute_value_impl<'a>(meta: &'a Meta, id: &[&str]) -> Option<String> {
    if id.len() < 2 {
        return None;
    }

    if let Meta::List(meta_list) = meta {
        if meta_list.ident != id[0] {
            return None;
        }

        for nested_meta in &meta_list.nested {
            if let NestedMeta::Meta(meta) = nested_meta {
                if let Meta::NameValue(MetaNameValue {
                    ref ident,
                    lit: Lit::Str(ref lit_str),
                    ..
                }) = *meta
                {
                    if ident == id[1] {
                        return Some(lit_str.value());
                    }
                } else {
                    return get_attribute_value_impl(meta, &id[1..]);
                }
            }
        }
    }

    None
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_get_attribute_value_impl() {
        let attr: Attribute = parse_quote!(#[level0(level1 = "hi", level1_1(level2 = "bye"))]);

        let meta = attr.interpret_meta().unwrap();

        assert_eq!(get_attribute_value_impl(&meta, &[]), None);

        assert_eq!(get_attribute_value_impl(&meta, &["not"]), None);

        assert_eq!(get_attribute_value_impl(&meta, &["level0"]), None);

        assert_eq!(
            get_attribute_value_impl(&meta, &["level0", "level1"]),
            Some("hi".to_string())
        );

        assert_eq!(
            get_attribute_value_impl(&meta, &["level0", "level1_1"]),
            None
        );

        assert_eq!(
            get_attribute_value_impl(&meta, &["level0", "level1_1", "level2"]),
            Some("bye".to_string())
        );
    }

    #[test]
    fn test_get_attribute_value() {
        let attr: Attribute = parse_quote!(#[level0(level1 = "hi", level1_1(level2 = "bye"))]);
        let attr = [attr];

        assert_eq!(get_attribute_value(&attr, ""), None);

        assert_eq!(get_attribute_value(&attr, "not"), None);

        assert_eq!(get_attribute_value(&attr, "level0"), None);

        assert_eq!(
            get_attribute_value(&attr, "level0.level1"),
            Some("hi".to_string())
        );

        assert_eq!(get_attribute_value(&attr, "level0.level1_1"), None);

        assert_eq!(
            get_attribute_value(&attr, "level0.level1_1.level2"),
            Some("bye".to_string())
        );
    }

    #[test]
    fn test_get_attribute_map_impl() {
        let attr: Attribute = parse_quote!(#[level0(level1 = "hi", level1_1(level2 = "bye"))]);

        let meta = attr.interpret_meta().unwrap();

        let mut result = HashMap::new();
        get_attribute_map_impl(&mut result, &meta, "level0");
        assert_eq!(result.len(), 2);
        assert_eq!(
            result,
            vec![
                ("level0.level1".to_string(), "hi".to_string()),
                ("level0.level1_1.level2".to_string(), "bye".to_string()),
            ].into_iter()
            .collect()
        );
    }

    #[test]
    fn test_get_attribute_map() {
        assert_eq!(
            get_attribute_map(&[
                parse_quote!(#[level0(level1 = "hi", level1_1(level2 = "bye"))]),
                parse_quote!(#[gen0(gen1 = "amoeba", gen1_1 = "monad", gen1_2(gen2 = "monoid"))])
            ]),
            vec![
                ("level0.level1".to_string(), "hi".to_string()),
                ("level0.level1_1.level2".to_string(), "bye".to_string()),
                ("gen0.gen1".to_string(), "amoeba".to_string()),
                ("gen0.gen1_1".to_string(), "monad".to_string()),
                ("gen0.gen1_2.gen2".to_string(), "monoid".to_string()),
            ].into_iter()
            .collect()
        );
    }

    #[test]
    fn test_format_doc_impl() {
        let mut attrs: Vec<Attribute> = vec![
            parse_quote!{
                #[owner(name = "Boncheol Gu", number = "000-0000-0000")]
            },
            parse_quote!{
                /// The owner of Variant1 is {owner.name}, of #{owner.number}.
            },
        ];

        format_doc_impl(&mut attrs);
        assert_eq!{
            attrs[1],
            parse_quote!{
                /// The owner of Variant1 is Boncheol Gu, of #000-0000-0000.
            },
        };
    }
}
