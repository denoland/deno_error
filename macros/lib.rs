// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
#![deny(clippy::unnecessary_wraps)]
#![deny(clippy::print_stderr)]
#![deny(clippy::print_stdout)]

use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use quote::ToTokens;
use syn::parse::Parse;
use syn::parse::ParseStream;
use syn::parse2;
use syn::spanned::Spanned;
use syn::Attribute;
use syn::Data;
use syn::DeriveInput;
use syn::Error;
use syn::Field;
use syn::Fields;
use syn::LitStr;
use syn::Member;
use syn::Meta;
use syn::Token;
use syn::Type;

const IDENTIFIABLE_ERRORS: [&str; 7] = [
  "Error",
  "RangeError",
  "TypeError",
  "SyntaxError",
  "URIError",
  "ReferenceError",
  "NotSupportedError",
];

#[proc_macro_derive(JsError, attributes(class, property, inherit))]
pub fn derive_js_error(
  item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
  match js_error(item.into()) {
    Ok(output) => output.into(),
    Err(err) => err.into_compile_error().into(),
  }
}

fn js_error(item: TokenStream) -> Result<TokenStream, Error> {
  let input = parse2::<DeriveInput>(item)?;

  let (class, out_properties) = match input.data {
    Data::Enum(data) => {
      let top_class_attr = input
        .attrs
        .into_iter()
        .find_map(|attr| ClassAttrValue::from_attribute(attr).transpose())
        .transpose()?;
      if let Some(top_class_attr) = &top_class_attr {
        if matches!(top_class_attr, ClassAttrValue::Inherit(_)) {
          return Err(Error::new(
            top_class_attr.to_tokens(&None).unwrap_err().span(),
            "top level class attribute cannot be inherit",
          ));
        }
      }

      let mut get_class = vec![];
      let mut get_properties = vec![];

      for variant in data.variants {
        let class_attr = variant
          .attrs
          .into_iter()
          .find_map(|attr| ClassAttrValue::from_attribute(attr).transpose())
          .unwrap_or_else(|| {
            top_class_attr.clone().ok_or_else(|| {
              Error::new(variant.ident.span(), "class attribute is missing")
            })
          })?;

        let (class, properties, inherit_member, parsed_properties) =
          handle_variant_or_struct(class_attr, variant.fields)?;

        let variant_ident = variant.ident;

        let match_arm_identifiers = {
          let mut parsed_properties = parsed_properties
            .into_iter()
            .enumerate()
            .map(|(i, property)| {
              let i = format_ident!("__{i}");
              let member = property.ident;
              quote!(#member: #i,)
            })
            .collect::<Vec<_>>();

          if let Some((member, _)) = &inherit_member {
            parsed_properties.push(quote!(#member: inherit,));
          }

          parsed_properties
        };

        let match_arm =
          quote!(Self::#variant_ident { #(#match_arm_identifiers)* .. });

        get_class.push(quote! {
          #match_arm => #class,
        });

        get_properties.push(quote! {
          #match_arm => {
            #properties
          }
        });
      }

      (
        quote! {
          match self {
            #(#get_class)*
          }
        },
        quote! {
          match self {
            #(#get_properties)*
          }
        },
      )
    }
    Data::Struct(data) => {
      let class_attr = input
        .attrs
        .into_iter()
        .find_map(|attr| ClassAttrValue::from_attribute(attr).transpose())
        .unwrap_or_else(|| {
          if data.fields.len() == 1 {
            Ok(ClassAttrValue::Inherit(kw::inherit::default()))
          } else {
            Err(Error::new(
              input.ident.span(),
              "class attribute is missing and could not be inferred",
            ))
          }
        })?;

      let (class, properties, member, parsed_properties) =
        handle_variant_or_struct(class_attr, data.fields)?;

      let specifier_var = member.map(|(member, _)| {
        quote! {
          let inherit = &self.#member;
        }
      });

      let parsed_properties = parsed_properties
        .into_iter()
        .enumerate()
        .map(|(i, property)| {
          let i = format_ident!("__{i}");
          let member = property.ident;
          quote! {
            let #i = &self.#member;
          }
        })
        .collect::<Vec<_>>();

      (
        quote! {
          #specifier_var
          #class
        },
        quote! {
          #specifier_var
           #(#parsed_properties)*
          #properties
        },
      )
    }
    Data::Union(_) => {
      return Err(Error::new(input.span(), "Unions are not supported"))
    }
  };

  let ident = input.ident;

  Ok(quote! {
    #[allow(unused_qualifications)]
    impl ::deno_error::JsErrorClass for #ident {
      fn get_class(&self) -> &'static str {
        #class
      }
      fn get_message(&self) -> ::std::borrow::Cow<'static, str> {
        self.to_string().into()
      }
      fn get_additional_properties(
        &self
      ) -> Option<Vec<(::std::borrow::Cow<'static, str>, ::std::borrow::Cow<'static, str>)>> {
        #out_properties
      }
    }
  })
}

#[allow(clippy::type_complexity)]
fn handle_variant_or_struct(
  class_attr: ClassAttrValue,
  fields: Fields,
) -> Result<
  (
    TokenStream,
    TokenStream,
    Option<(Member, TokenStream)>,
    Vec<ParsedProperty>,
  ),
  Error,
> {
  let parsed_properties = get_properties_from_fields(&fields)?;

  let properties = if !parsed_properties.is_empty() {
    let properties = parsed_properties
      .iter()
      .enumerate()
      .map(|(i, property)| {
        let i = format_ident!("__{i}");
        let ident_str = &property.name;

        quote! {
          (
            ::std::borrow::Cow::Borrowed(#ident_str),
            ::std::borrow::Cow::Owned(#i.to_string()),
          ),
        }
      })
      .collect::<Vec<_>>();

    Some(quote!(vec![#(#properties),*]))
  } else {
    None
  };

  let inherit_member = match fields {
    Fields::Named(fields_named) => {
      let field = if fields_named.named.len() == 1
        && matches!(class_attr, ClassAttrValue::Inherit(_))
      {
        fields_named.named.first()
      } else {
        fields_named.named.iter().find(get_inherit_attr_field)
      };

      field.map(|field| {
        (
          Member::Named(field.ident.clone().unwrap()),
          field_inherit_reference(field),
        )
      })
    }
    Fields::Unnamed(fields_unnamed) => {
      let field = if fields_unnamed.unnamed.len() == 1
        && matches!(class_attr, ClassAttrValue::Inherit(_))
      {
        fields_unnamed.unnamed.first().map(|field| (0, field))
      } else {
        fields_unnamed
          .unnamed
          .iter()
          .enumerate()
          .find(|(_, field)| get_inherit_attr_field(field))
      };

      field.map(|(i, field)| {
        (
          Member::Unnamed(syn::Index::from(i)),
          field_inherit_reference(field),
        )
      })
    }
    Fields::Unit => None,
  };

  let class = class_attr.to_tokens(&inherit_member)?;

  let properties = if let Some((_, tokens)) = &inherit_member {
    let inherited_properties = quote!(::deno_error::JsErrorClass::get_additional_properties(
      #tokens
    ));

    if let Some(properties) = properties {
      quote! {
        let mut properties = #properties;
        if let Some(inherited_properties) = #inherited_properties {
          properties.extend(inherited_properties);
        }
        properties
      }
    } else {
      inherited_properties
    }
  } else {
    properties
      .map_or_else(|| quote!(None), |properties| quote!(Some(#properties)))
  };

  Ok((class, properties, inherit_member, parsed_properties))
}

fn get_inherit_attr_field(field: &&Field) -> bool {
  field
    .attrs
    .iter()
    .any(|attr| attr.path().is_ident("inherit"))
}

mod kw {
  syn::custom_keyword!(class);
  syn::custom_keyword!(property);
  syn::custom_keyword!(inherit);
}

#[derive(Debug, Clone)]
enum ClassAttrValue {
  Lit(syn::LitStr),
  Ident(Ident),
  Inherit(kw::inherit),
}

impl ClassAttrValue {
  fn from_attribute(attr: Attribute) -> Result<Option<Self>, Error> {
    if attr.path().is_ident("class") {
      let list = attr.meta.require_list()?;
      let value = list.parse_args::<Self>()?;

      match &value {
        ClassAttrValue::Lit(lit) => {
          if IDENTIFIABLE_ERRORS.contains(&lit.value().as_str()) {
            return Err(Error::new(
              lit.span(),
              format!("An identifier can be used instead of '{}'", lit.value()),
            ));
          }
        }
        ClassAttrValue::Ident(ident) => {
          let ident_str = ident.to_string();

          // needs to call to_lowercase to handle _ since checking if its both
          // lower or uppercase returns false
          if ident_str.to_lowercase() != ident_str {
            return Err(Error::new(
              ident.span(),
              "Identifier passed is not lowercase",
            ));
          }
        }
        ClassAttrValue::Inherit(_) => {}
      }

      return Ok(Some(value));
    }

    Ok(None)
  }

  fn to_tokens(
    &self,
    inherit_member: &Option<(Member, TokenStream)>,
  ) -> Result<TokenStream, Error> {
    let class_tokens = match self {
      ClassAttrValue::Lit(lit) => quote!(#lit),
      ClassAttrValue::Ident(ident) => {
        let error_name =
          format_ident!("{}_ERROR", ident.to_string().to_uppercase());
        quote!(::deno_error::builtin_classes::#error_name)
      }
      ClassAttrValue::Inherit(inherit) => {
        let (_, tokens) = inherit_member.as_ref().ok_or_else(|| {
          Error::new(
            inherit.span,
            "class attribute was set to inherit, but multiple fields are available and none was marked as inherit",
          )
        })?;

        quote!(::deno_error::JsErrorClass::get_class(#tokens))
      }
    };

    Ok(class_tokens)
  }
}

impl Parse for ClassAttrValue {
  fn parse(input: ParseStream) -> syn::Result<Self> {
    let lookahead = input.lookahead1();

    if lookahead.peek(syn::LitStr) {
      Ok(Self::Lit(input.parse()?))
    } else if lookahead.peek(kw::inherit) {
      Ok(Self::Inherit(input.parse()?))
    } else if lookahead.peek(syn::Ident) {
      Ok(Self::Ident(input.parse()?))
    } else if lookahead.peek(Token![type]) {
      let type_token = input.parse::<Token![type]>()?;
      Ok(Self::Ident(Ident::new("type", type_token.span)))
    } else {
      Err(lookahead.error())
    }
  }
}

#[derive(Debug)]
struct ParsedProperty {
  ident: Member,
  name: String,
}

fn get_properties_from_fields(
  fields: &Fields,
) -> Result<Vec<ParsedProperty>, Error> {
  const PROPERTY_IDENT: &str = "property";
  let mut out_fields = vec![];

  match fields {
    Fields::Named(named) => {
      for field in &named.named {
        for attr in &field.attrs {
          if attr.path().is_ident(PROPERTY_IDENT) {
            let name = match &attr.meta {
              Meta::Path(_) => None,
              Meta::List(list) => {
                return Err(Error::new(
                  list.delimiter.span().open(),
                  "expected `=`",
                ));
              }
              Meta::NameValue(meta) => {
                Some(parse2::<LitStr>(meta.value.to_token_stream())?.value())
              }
            };

            let ident = field.ident.clone().unwrap();
            let name = name.unwrap_or_else(|| ident.to_string());
            let ident = Member::Named(field.ident.clone().unwrap());
            out_fields.push(ParsedProperty { name, ident });

            break;
          }
        }
      }
    }
    Fields::Unnamed(unnamed) => {
      for (i, field) in unnamed.unnamed.iter().enumerate() {
        for attr in &field.attrs {
          if attr.path().is_ident(PROPERTY_IDENT) {
            let name_value = attr.meta.require_name_value()?;
            let name =
              parse2::<LitStr>(name_value.value.to_token_stream())?.value();

            let ident = Member::Unnamed(syn::Index::from(i));
            out_fields.push(ParsedProperty { name, ident });

            break;
          }
        }
      }
    }
    Fields::Unit => {}
  }

  Ok(out_fields)
}

fn field_inherit_reference(field: &Field) -> TokenStream {
  let is_wrapped = match &field.ty {
    Type::Path(e) => {
      if let Some(first) = e.path.segments.last() {
        matches!(first.ident.to_string().as_str(), "Box" | "Rc" | "Arc")
      } else {
        false
      }
    }
    _ => false,
  };

  if is_wrapped {
    quote!(&**inherit)
  } else {
    quote!(inherit)
  }
}
