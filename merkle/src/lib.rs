use sha2::digest::Update;
use sha2::Sha256;

pub trait MerkleTree {
    fn merkle(&self) -> Sha256;
}

impl MerkleTree for &'_ str {
    fn merkle(&self) -> Sha256 {
        let mut digest = Sha256::default();
        digest.update(self.as_bytes());
        digest
    }
}

impl MerkleTree for u8 {
    fn merkle(&self) -> Sha256 {
        let mut digest = Sha256::default();
        digest.update(self.to_le_bytes().as_slice());
        digest
    }
}

/*
impl_merkle_tree_for_numeric_type!(i8);
impl_merkle_tree_for_numeric_type!(u8);
impl_merkle_tree_for_numeric_type!(i16);
impl_merkle_tree_for_numeric_type!(u16);
impl_merkle_tree_for_numeric_type!(i32);
impl_merkle_tree_for_numeric_type!(u32);
impl_merkle_tree_for_numeric_type!(i64);
impl_merkle_tree_for_numeric_type!(u64);
impl_merkle_tree_for_numeric_type!(i128);
impl_merkle_tree_for_numeric_type!(u128);
*/
