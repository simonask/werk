use std::hash::Hash as _;

/// Same as `std::hash::Hash`, except that it only includes semantically
/// relevant information. For example, for AST nodes the spans are excluded.
pub trait SemanticHash {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H);
}

impl<T: SemanticHash> SemanticHash for &[T] {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for item in self.iter() {
            item.semantic_hash(state);
        }
    }
}

impl<T: SemanticHash> SemanticHash for Option<T> {
    fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        if let Some(value) = self {
            value.semantic_hash(state);
        }
    }
}

macro_rules! hash_is_semantic {
    ($t:tt $(<$($lifetime_param:tt),+>)?) => {
        impl $(<$($lifetime_param),*>)* $crate::SemanticHash for $t $(<$($lifetime_param),+>)? {
            #[inline]
            fn semantic_hash<H: std::hash::Hasher>(&self, state: &mut H) {
                use std::hash::Hash;
                self.hash(state);
            }
        }
    };
}

pub(crate) use hash_is_semantic;

hash_is_semantic!(str);
