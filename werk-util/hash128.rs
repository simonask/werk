impl rustc_stable_hash::FromStableHash for Hash128 {
    type Hash = rustc_stable_hash::SipHasher128Hash;

    fn from(hash: Self::Hash) -> Self {
        let hi = u128::from(hash.0[0]) << 64;
        let lo = u128::from(hash.0[1]);
        Hash128(hi | lo)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Hash128(pub u128);
impl From<u128> for Hash128 {
    #[inline]
    fn from(n: u128) -> Self {
        Hash128(n)
    }
}

impl serde::Serialize for Hash128 {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // Serialize as hex string. Also, TOML doesn't support 64-bit integers.
        serializer.serialize_str(&format!("{:016x}", self.0))
    }
}

impl<'de> serde::Deserialize<'de> for Hash128 {
    fn deserialize<D>(deserializer: D) -> Result<Hash128, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let n = u128::from_str_radix(&s, 16).map_err(serde::de::Error::custom)?;
        Ok(Hash128(n))
    }
}
