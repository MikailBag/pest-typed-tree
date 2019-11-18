use heck::CamelCase;
use pest_meta::ast::{Expr, Rule};
use proc_macro2::{Span, TokenStream, TokenTree};
use quote::quote;
use std::collections::HashSet;

fn rule_idents(expr: &Expr, out: &mut HashSet<String>) {
    let iter = pest_meta::ast::ExprTopDownIterator::new(expr);
    for ex in iter {
        if let Expr::Ident(rule_ref) = ex {
            out.insert(rule_ref);
        }
    }
}

fn generate_module_header() -> TokenStream {
    quote! {
            use super::Rule;
            use pest::iterators::{Pair, Pairs};

            fn report_rule_mismatch(expected: Rule, actual: Rule) {
                panic!("expected {:?}, got {:?}", expected, actual);
            }
    /*
            pub trait Node {

            }*/
        }
}

fn is_rule_special(name: &str) -> bool {
    name.chars().all(|c| !c.is_ascii_lowercase())
}

fn make_ident(s: &str) -> syn::Ident {
    syn::Ident::new(s, Span::call_site())
}

fn generate_rule_struct(rule: Rule) -> TokenStream {
    let rule_name = &rule.name;
    let rule_name_ident = make_ident(&rule.name);
    let struct_name = rule.name.to_camel_case();
    let struct_name = make_ident(&struct_name);
    let struct_def = quote! {
          pub struct #struct_name<'i>(Pair<'i, Rule>);

          impl<'i> std::fmt::Debug for #struct_name<'i> {
              fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                    let mut helper = f.debug_tuple(#rule_name);
                    helper.field(&self.0);
                    helper.finish()
              }
          }

          impl<'i> std::clone::Clone for #struct_name<'i> {
              fn clone(&self) -> Self {
                  Self(self.0.clone())
              }
          }
    };
    let mut out = struct_def;

    let common_fns = quote! {
        impl<'i> #struct_name<'i> {
            pub const RULE: Rule = Rule::#rule_name_ident;

            pub fn new(p: Pair<'i, Rule>) -> #struct_name {
                if p.as_rule() != Self::RULE {
                    report_rule_mismatch(Self::RULE, p.as_rule());
                }
                #struct_name(p)
            }

            pub fn text(&self) -> &str {
                self.0.as_str()
            }

            pub fn span(&self) -> pest::Span {
                self.0.as_span()
            }

            pub fn pairs(&self) -> Pairs<'i, Rule> {
               self.0.clone().into_inner()
            }
        }
    };

    out.extend(common_fns);

    let mut idents = HashSet::new();
    rule_idents(&rule.expr, &mut idents);

    for child_rule in idents {
        if is_rule_special(&child_rule) {
            continue;
        }
        let getter_fn_ident = make_ident(&format!("get_{}", &child_rule));

        let child_rule_struct_name = make_ident(&child_rule.to_camel_case());

        let get_child_fn = quote! {
             impl<'i> #struct_name<'i> {
                 pub fn #getter_fn_ident (&self) -> #child_rule_struct_name {
                        self.pairs().find(|pair| pair.as_rule() == #child_rule_struct_name::RULE).map(|pair| #child_rule_struct_name::new(pair)).expect("bug in pest-typed-tree: child of requested type missing")
                 }
             }
        };
        out.extend(get_child_fn);
    }

    out
}

pub fn generate(grammar: String) -> proc_macro::TokenStream {
    let pest_ast = pest_meta::parser::parse(pest_meta::parser::Rule::grammar_rules, &grammar)
        .expect("failed to parse pest grammar");
    let pest_ast = pest_meta::parser::consume_rules(pest_ast).expect("failed to build grammar ast");
    let mut module = TokenStream::new();
    module.extend(generate_module_header());
    for rule in pest_ast {
        module.extend(generate_rule_struct(rule));
    }
    // now let's wrap it into module
    let mut out: TokenStream = quote! {
        mod nodes
    };

    let braced_def = TokenTree::Group(proc_macro2::Group::new(
        proc_macro2::Delimiter::Brace,
        module,
    ));

    out.extend(std::iter::once(braced_def));
    out.into()
}
