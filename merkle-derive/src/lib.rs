use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput};

/*
This function generates the outer `impl` block for our trait and returns the final token stream
to the compiler. Usually it's best to convert to proc_macro::TokenStream as late as possible,
and this is the outermost layer in our code generation.
*/
fn impl_merkle_tree(ty: &syn::Ident, hash_updates: proc_macro2::TokenStream) -> TokenStream {
    TokenStream::from(quote!(
        impl ::merkle::MerkleTree for #ty {
            fn merkle(&self) -> ::sha2::Sha256 {
                use ::merkle::MerkleTree;
                let mut full_hash = ::sha2::Sha256::default();
                #hash_updates
                full_hash
            }
        }
    ))
}

#[proc_macro_derive(MerkleTree)]
pub fn derive_merkle_tree(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);

    match &input.data {
        Data::Struct(s) => {
            /*
            Given the sample struct:

            struct Sample {
                id: u32,
                label: &'static str,
            }

            we want to generate the following implementation:

            impl ::merkle::MerkleTree for Sample {
                fn merkle(&self) -> ::sha2::Sha256 {
                    use ::merkle::MerkleTree;
                    let mut full_hash = ::sha2::Sha256::default();
                    full_hash.update(self.id.merkle().finalize());
                    full_hash.update(self.label.merkle().finalize());
                    full_hash
                }
            }

            This process will be spread over several steps as we go down the input AST:
            - one `impl` block for the whole structure
            - one `full_hash.update()` call for each field
            */

            /*
            Generate the code to update `full_hash` with the value of .merkle() for each field in `names`

            This is (mostly) doing text/AST transformations, and at no point before the final output the code
            must be valid. For example, here we never define the `full_hash` variable, but we're still referring
            to it. We simply expect that whoever calls `hash_fields` will ensure the variable exists, has
            the right type etc.

            We're using the TokenStream type from the proc_macro2 crate, which integrates nicely with `syn`
            and `quote`. The final result will have to be a `proc_macro::TokenStream` but we can wait with
            the conversion until the very end...
            */
            let members = s.fields.members();
            /*
            `members` is an iterator yielding values which can be converted to tokens, so we need
            to iterate over it and generate the appropriate code for each field.

            Rather than using a `for` loop, we can use the `quote!` macro's repetition feature:

            #( ... #members ... )*

            which will expand to the contents of the parentheses repeated for each element in `members`.
            */
            let field_hashes = quote!(
                #(full_hash.update(self.#members.merkle().finalize());)*
            );

            /*
            ... which is this function, containing all the boilerplate to implement the MerkleTree
            trait:

            impl ::merkle::MerkleTree for <type> {
                fn merkle(&self) -> ::sha2::Sha256 {
                    use ::merkle::MerkleTree;
                    let mut full_hash = ::sha2::Sha256::default();

                    // ...

                    full_hash
                }
            }
            */
            impl_merkle_tree(&input.ident, field_hashes)
        }
        Data::Enum(e) => {
            /*
            Given the sample enum:

            enum SampleEnum {
                Number(u32),
                String(&'static str),
                Empty,
                Named { id: u32, label: &'static str },
            }

            we want to generate the following implementation:

            impl ::merkle::MerkleTree for SampleEnum {
                fn merkle(&self) -> ::sha2::Sha256 {
                    use ::merkle::MerkleTree;
                    let mut full_hash = ::sha2::Sha256::default();
                    match self {
                        Self::Number { 0: f0 } => {
                            full_hash.update(f0.merkle().finalize());
                        }
                        Self::String { 0: f0 } => {
                            full_hash.update(f0.merkle().finalize());
                        }
                        Self::Empty {} => {}
                        Self::Named { id: f0, label: f1 } => {
                            full_hash.update(f0.merkle().finalize());
                            full_hash.update(f1.merkle().finalize());
                        }
                    }
                    full_hash
                }
            }

            Note that unlike structs, enum variants don't have their own type, and we cannot
            have a single variable representing a particular variant. This means we need
            to unpack the variant's fields into individual variables that all need their
            individual names.

            We cannot use the `Member` type (which gets rendered e.g., as `0` or `id`), because
            those are not valid variable names for tuple variants. So we may as well be
            consistent and use generated variable names in all cases, including variants with named
            fields.

            Most of the implementation boilerplate can be generated by impl_merkle_tree,
            we only need to generate the `match` block, but first, let's define some helper
            functions to work with the generated identifiers.

            `with_idents` takes an iterator of items and generates a new iterator of pairs
            `(item, ident)`, where `ident` is a fresh identifier of the form f<number>.
            We'll use it on the left side of the match arm (the pattern).

            `idents_for` takes an iterator of items and generates a new iterator of as many new
            identifiers as there are items in the input iterator. We'll use it on the right side
            of the match arm (the hash computation).
            */
            fn with_idents<I, T>(items: I) -> impl Iterator<Item = (T, syn::Ident)>
            where
                I: Iterator<Item = T>,
            {
                items.zip(
                    (0..).map(|i| syn::Ident::new(format!("f{}", i).as_str(), Span::call_site())),
                )
            }

            fn idents_for<I, T>(items: I) -> impl Iterator<Item = syn::Ident>
            where
                I: Iterator<Item = T>,
            {
                with_idents(items).map(|(_, ident)| ident)
            }

            let variant_hashes = e.variants.iter().map(|v| {
                /*
                Have you noticed the weird syntax for accessing fields in a tuple variant?
                Usually, you would write something like:

                match self {
                    Self::Number(f0) => {
                        full_hash.update(f0.merkle().finalize());
                    }
                    Self::String(f0) => {
                        full_hash.update(f0.merkle().finalize());
                    }
                    Self::Empty => {}
                    Self::Named { id: f0, label: f1 } => {
                        full_hash.update(f0.merkle().finalize());
                        full_hash.update(f1.merkle().finalize());
                    }
                }

                but in procedural macros, using the unified syntax can be simpler.
                */

                /*
                1. Generate the match pattern. It should look like:

                   Self::<variant name> { <field_name: f<X>>, ... }

                   Hint: `#(#foo)*` emits all elements of `foo` without additional separators.
                          To add a comma between the elements, put it just before the final `*`:
                          `#(#foo),*`
                */
                let field_names_with_idents =
                    with_idents(v.fields.members()).map(|(member, ident)| todo!());
                let variant_ident = &v.ident;
                let pattern = todo!();

                /*
                2. Generate the match arm code, which is basically `<pattern> => { <hash each field in turn> }`

                Note that the fields are bound to f0, f1, etc., so we need to use those names again.
                */
                let names = idents_for(v.fields.members());
                todo!()
            });

            impl_merkle_tree(&input.ident, todo!())
        }
        Data::Union(_) => TokenStream::from(
            syn::Error::new(input.ident.span(), "Cannot derive MerkleTree for a union")
                .to_compile_error(),
        ),
    }
}
