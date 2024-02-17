use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Field, Fields};

/*
Okay, we're taking the training wheels off :)

This is a highly generic helper function (note there are *no* concrete types in its signature!)
which takes:
- any iterator yielding references to an arbitrary T: `Iterator<Item = &T>`
- a function which takes a `usize` and an element from the iterator and returns an arbitrary R:
  `FnMut(usize, &T) -> R`

and returns some (unnamed) iterator type yielding `R`s: `impl Iterator<Item = R>`

The lifetimes are pretty scary here, but you can usually let the compiler guide you to insert
explicit lifetime annotations where necessary.

Rewritten in a more procedural style, it would be (roughly, this is not valid code for multiple
reasons):

for i in 0..iter.len() {
    yield func(i, iter[i]);
}
*/
fn map_with_ids<'a, F, T, R, I>(iter: I, mut func: F) -> impl Iterator<Item = R> + 'a
where
    I: Iterator<Item = &'a T> + 'a,
    F: FnMut(usize, &T) -> R + 'a,
    T: 'a,
{
    iter.enumerate().map(move |(id, field)| func(id, field))
}

/*
Use the helper above to map a function over a sequence of fields of any kind:
- named fields: struct Foo { a: u32, b: u32 }
- unnamed fields: struct Foo(u32, u32);
- no fields at all: struct Foo;

Note that we cannot return an unnamed type here. All returned values must have the same type,
it's not enough that they all fit the same trait bounds and in this case all three match arms
would have different types (the type `map_with_ids` returns is a Voldemort type due to the closure
in the call to `map`). So we collect all the values into a vector and return that.
*/
fn map_fields_with_ids<'a, F, R>(fields: &'a Fields, func: F) -> Vec<R>
where
    F: FnMut(usize, &Field) -> R + 'a,
{
    match fields {
        Fields::Named(n) => map_with_ids(n.named.iter(), func).collect(),
        Fields::Unnamed(u) => map_with_ids(u.unnamed.iter(), func).collect(),
        Fields::Unit => vec![],
    }
}

/*
Generate the code to update `full_hash` with the value of .merkle() for each field in `names`

This is (mostly) doing text/AST transformations and at no point before the final output the code
must be valid. For example, here we're referring to a `full_hash` variable that's not defined
anywhere in the function. We simply expect that whoever calls `hash_fields` will ensure the variable
exists, has the right type etc.

We're using the TokenStream type from the proc_macro2 crate, which integrates nicely with `syn`
and `quote`. The final result will have to be a `proc_macro::TokenStream` but we can wait with
the conversion until the very end...
*/
fn hash_fields(
    names: &[proc_macro2::TokenStream],
) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
    names.iter().map(|name| {
        quote!(
            let field_hash = #name.merkle();
            full_hash.update(field_hash.finalize());
        )
    })
}

/*
... which happens here. This function generates the outer `impl` block for our trait
and return the final token stream to the compiler.
*/
fn impl_merkle_tree(ty: &syn::Ident, iter_over_fields: proc_macro2::TokenStream) -> TokenStream {
    TokenStream::from(quote!(
        impl ::merkle::MerkleTree for #ty {
            fn merkle(&self) -> ::sha2::Sha256 {
                use ::merkle::MerkleTree;
                let mut full_hash = ::sha2::Sha256::default();
                #iter_over_fields
                full_hash
            }
        }
    ))
}

#[proc_macro_derive(MerkleTree)]
pub fn derive_merkle_tree(item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as DeriveInput);

    match input.data {
        Data::Struct(s) => {
            /*
            Given the sample struct:

            struct Sample {
                id: u32,
                label: &'static str,
            }

            we generate the following implementation:

            impl ::merkle::MerkleTree for Sample {
                fn merkle(&self) -> ::sha2::Sha256 {
                    use ::merkle::MerkleTree;
                    let mut full_hash = ::sha2::Sha256::default();
                    let field_hash = self.id.merkle();
                    full_hash.update(field_hash.finalize());
                    let field_hash = self.label.merkle();
                    full_hash.update(field_hash.finalize());
                    full_hash
                }
            }

            Note that for a tuple struct, we want to refer to the fields as self.<number>
            (since they don't have names)

            First, let's generate the field accessors. Using the `map_fields_with_ids` helper,
            we end up with a vector of `self.name` or `self.<number>` (depending on the struct kind)

            Note that we do need to wrap the numeric index in a `syn::Index`, otherwise it would
            be emitted in the generated code as `0usize` which is not valid for tuple field access
            (if you get this wrong, you actually get a helpful message from the compiler)
            */
            let field_names = map_fields_with_ids(&s.fields, |id, field| {
                if let Some(ref name) = field.ident {
                    quote!(self.#name)
                } else {
                    let index = syn::Index::from(id);
                    quote!(self.#index)
                }
            });

            /*
            Now, let's wrap all the field names in the code required to actually hash that field,
            i.e. go from `self.foo` to:

            let field_hash = self.foo.merkle();
            full_hash.update(field_hash.finalize());

            The `full_hash` variable will have to be created in the code that puts all these
            fragments together...
            */
            let field_hashes = hash_fields(&field_names);

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
            impl_merkle_tree(
                &input.ident,
                quote!(
                    #(#field_hashes)*
                ),
            )
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
                        Self::Number(f0) => {
                            let field_hash = f0.merkle();
                            full_hash.update(field_hash.finalize());
                        }
                        Self::String(f0) => {
                            let field_hash = f0.merkle();
                            full_hash.update(field_hash.finalize());
                        }
                        Self::Empty => {}
                        Self::Named { id, label } => {
                            let field_hash = id.merkle();
                            full_hash.update(field_hash.finalize());
                            let field_hash = label.merkle();
                            full_hash.update(field_hash.finalize());
                        }
                    }
                    full_hash
                }
            }

            Note that variants with named fields are expanded using their names, but for tuple
            (unnamed) variants, we cannot name them as 0, 1, 2 etc. in the pattern, so we need to
            introduce some other names (f<number> in this example)

            Most of the implementation boilerplate can be generated by impl_merkle_tree,
            we only need to generate the `match` block.
            */

            let variant_hashes = e.variants.iter().map(|v| {
                /*
                For each enum variant we need to:

                1. Generate the names to use in the match arm. When the name is present, just follow
                the code for structs (note: you do not want `self.name`, just `name`). For unnamed
                fields, we need to create a new ident:

                let name = syn::Ident::new(format!("f{}", id).as_str(), Span::call_site());

                (the Span type comes from the proc_macros2 crate)
                */
                let field_names = map_fields_with_ids(&v.fields, |id, field| {
                    if let Some(ref name) = field.ident {
                        quote!(#name)
                    } else {
                        let name = syn::Ident::new(format!("f{}", id).as_str(), Span::call_site());
                        quote!(#name)
                    }
                });

                /*
                2. Generate the match pattern. Depending on the enum type (variant of v.fields),
                it should be:
                - for named fields: Self::<variant name> { <comma-separated field_names> }
                - for unnamed fields: Self::<variant name> ( <comma-separated field_names> }
                - for enums without fields: Self::<variant name>

                    Hint: `#(#foo)*` emits all elements of `foo` without additional separators.
                          To add a comma between the elements, put it just before the final `*`:
                          `#(#foo),*`
                */
                let ident = &v.ident;
                let pattern = match v.fields {
                    Fields::Named(_) => quote!(Self::#ident { #(#field_names),* }),
                    Fields::Unnamed(_) => quote!(Self::#ident ( #(#field_names),* )),
                    Fields::Unit => quote!(Self::#ident),
                };

                /*
                3. Generate the match arm code, which is basically `<pattern> => { <hash each field in turn> }`
                */
                let field_hashes = hash_fields(&field_names);
                quote!(
                    #pattern => {
                        #(#field_hashes)*
                    }
                )
            });

            impl_merkle_tree(
                &input.ident,
                quote!(
                    match self {
                        #(#variant_hashes)*
                    }
                ),
            )
        }
        Data::Union(_) => TokenStream::from(
            syn::Error::new(input.ident.span(), "Cannot derive MerkleTree for a union")
                .to_compile_error(),
        ),
    }
}
