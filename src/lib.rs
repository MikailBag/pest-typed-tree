extern crate proc_macro;

mod generate;

#[proc_macro_derive(TypedTree, attributes(grammar))]
pub fn derive_typed_tree(stream: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(stream as syn::DeriveInput);
    if input.attrs.len() != 1 {
        panic!(r#"derive input must contain exactly once #[grammar = "path/to/pest/grammar""#)
    }
    let attr = input.attrs.into_iter().next().unwrap();
    let mut gram_path = attr.tokens.to_string();
    // FIXME: dirty hack
    gram_path = gram_path.trim_start_matches('=').trim().replace('"', "");
    // following is copy-pasted from pest_generator
    let root = std::env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
    let gram_path = std::path::Path::new(&root).join("src/").join(&gram_path);

    let grammar = std::fs::read_to_string(&gram_path).unwrap_or_else(|err| {
        panic!(
            "failed to read parser grammar file {}: {}",
            gram_path.display(),
            err
        )
    });
    generate::generate(grammar)
}
