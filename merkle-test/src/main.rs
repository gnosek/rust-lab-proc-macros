use merkle::MerkleTree;
use merkle_derive::MerkleTree;
use pretty_hex::PrettyHex;
use sha2::Digest;

#[derive(Debug, MerkleTree)]
struct Sample {
    id: u32,
    label: &'static str,
}

/*
#[derive(Debug, MerkleTree)]
enum SampleEnum {
    Number(u32),
    String(&'static str),
    Empty,
    Named { id: u32, label: &'static str },
}
*/

macro_rules! dump_merkle_hash {
    ($val:expr) => {
        let val = $val;
        let hash = val.merkle().finalize();
        println!("merkle hash of {:?} is:\n {:?}", val, hash.hex_dump());
    };
}

pub fn main() {
    dump_merkle_hash!(100u8);
    dump_merkle_hash!(100i8);
    dump_merkle_hash!(100u16);
    dump_merkle_hash!(100i16);
    dump_merkle_hash!(100u32);
    dump_merkle_hash!(100i32);
    dump_merkle_hash!(100u64);
    dump_merkle_hash!(100i64);
    dump_merkle_hash!(100u128);
    dump_merkle_hash!(100i128);
    dump_merkle_hash!(Sample {
        id: 123,
        label: "hello",
    });
    /*
    dump_merkle_hash!(SampleEnum::Number(100));
    dump_merkle_hash!(SampleEnum::String("hello"));
    dump_merkle_hash!(SampleEnum::Empty);
    dump_merkle_hash!(SampleEnum::Named {
        id: 123,
        label: "hello",
    });
*/
}
