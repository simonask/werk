use werk_fs::Absolute;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    List(Vec<Value>),
    String(StringValue),
}

impl Default for Value {
    #[inline]
    fn default() -> Self {
        Value::String(StringValue::default())
    }
}

#[derive(Debug, Clone, Default)]
pub struct StringValue {
    pub string: String,
    pub flags: StringFlags,
}

impl PartialEq for StringValue {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.string == other.string
    }
}

impl Eq for StringValue {}

impl PartialEq<str> for StringValue {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.string == other
    }
}

impl PartialOrd for StringValue {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for StringValue {
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.string.cmp(&other.string)
    }
}

impl std::hash::Hash for StringValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.string.hash(state);
    }
}

impl From<String> for StringValue {
    #[inline]
    fn from(string: String) -> Self {
        Self {
            string,
            flags: StringFlags::EMPTY,
        }
    }
}

impl From<StringValue> for String {
    #[inline]
    fn from(value: StringValue) -> Self {
        value.string
    }
}

impl From<&str> for StringValue {
    #[inline]
    fn from(string: &str) -> Self {
        string.to_owned().into()
    }
}

impl From<werk_fs::PathBuf> for StringValue {
    #[inline]
    fn from(value: werk_fs::PathBuf) -> Self {
        StringValue {
            string: value.into(),
            // TODO: Consider if abstract-pathiness should also be tracked.
            flags: StringFlags::EMPTY,
        }
    }
}

impl TryFrom<std::path::PathBuf> for StringValue {
    type Error = std::path::PathBuf;
    #[inline]
    fn try_from(value: std::path::PathBuf) -> Result<Self, Self::Error> {
        match value.into_os_string().into_string() {
            Ok(string) => Ok(StringValue {
                string,
                flags: StringFlags::CONTAINS_PATHS,
            }),
            Err(path) => Err(path.into()),
        }
    }
}

impl<'a> TryFrom<&'a std::path::Path> for StringValue {
    type Error = &'a std::path::Path;
    #[inline]
    fn try_from(value: &'a std::path::Path) -> Result<Self, Self::Error> {
        value.to_owned().try_into().map_err(|_| value)
    }
}

impl TryFrom<Absolute<std::path::PathBuf>> for StringValue {
    type Error = Absolute<std::path::PathBuf>;
    #[inline]
    fn try_from(value: Absolute<std::path::PathBuf>) -> Result<Self, Self::Error> {
        value
            .into_inner()
            .try_into()
            .map_err(Absolute::new_unchecked)
    }
}

impl<'a> TryFrom<&'a Absolute<std::path::Path>> for StringValue {
    type Error = &'a Absolute<std::path::Path>;
    #[inline]
    fn try_from(value: &'a Absolute<std::path::Path>) -> Result<Self, Self::Error> {
        value.as_inner().try_into().map_err(|_| value)
    }
}

impl std::ops::Deref for StringValue {
    type Target = str;

    #[inline]
    fn deref(&self) -> &str {
        &self.string
    }
}

impl std::borrow::Borrow<str> for StringValue {
    #[inline]
    fn borrow(&self) -> &str {
        &self.string
    }
}

impl AsRef<str> for StringValue {
    #[inline]
    fn as_ref(&self) -> &str {
        &self.string
    }
}

impl AsRef<std::ffi::OsStr> for StringValue {
    #[inline]
    fn as_ref(&self) -> &std::ffi::OsStr {
        self.string.as_ref()
    }
}

impl std::fmt::Display for StringValue {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self.string, f)
    }
}

bitflags::bitflags! {
    #[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
    pub struct StringFlags : u8 {
        const EMPTY = 0;
        /// When set, the string contains the result of `<...>` interpolations,
        /// and cannot be part of another `<...>` interpolation.
        const CONTAINS_PATHS = 0b01;
    }
}

impl PartialEq<String> for Value {
    #[inline]
    fn eq(&self, other: &String) -> bool {
        *self == *other.as_str()
    }
}

impl PartialEq<&str> for Value {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        *self == **other
    }
}

impl PartialEq<str> for Value {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        if let Value::String(s) = self {
            *s == *other
        } else {
            false
        }
    }
}

impl PartialEq<std::path::Path> for Value {
    #[inline]
    fn eq(&self, other: &std::path::Path) -> bool {
        if let Value::String(s) = self {
            std::path::Path::new(s) == other
        } else {
            false
        }
    }
}

impl PartialEq<Absolute<std::path::Path>> for Value {
    #[inline]
    fn eq(&self, other: &Absolute<std::path::Path>) -> bool {
        if let Value::String(s) = self {
            std::path::Path::new(s) == other
        } else {
            false
        }
    }
}

impl From<String> for Value {
    #[inline]
    fn from(value: String) -> Self {
        Value::String(value.into())
    }
}

impl From<&str> for Value {
    #[inline]
    fn from(value: &str) -> Self {
        Value::String(value.into())
    }
}

impl From<werk_fs::PathBuf> for Value {
    #[inline]
    fn from(value: werk_fs::PathBuf) -> Self {
        Value::String(value.into())
    }
}

impl From<Absolute<werk_fs::PathBuf>> for Value {
    #[inline]
    fn from(value: Absolute<werk_fs::PathBuf>) -> Self {
        value.into_inner().into()
    }
}

impl TryFrom<std::path::PathBuf> for Value {
    type Error = std::path::PathBuf;
    #[inline]
    fn try_from(value: std::path::PathBuf) -> Result<Self, Self::Error> {
        value.try_into().map(Value::String)
    }
}

impl<'a> TryFrom<&'a std::path::Path> for Value {
    type Error = &'a std::path::Path;
    #[inline]
    fn try_from(value: &'a std::path::Path) -> Result<Self, Self::Error> {
        value.try_into().map(Value::String)
    }
}

impl TryFrom<Absolute<std::path::PathBuf>> for Value {
    type Error = Absolute<std::path::PathBuf>;
    #[inline]
    fn try_from(value: Absolute<std::path::PathBuf>) -> Result<Self, Self::Error> {
        value.try_into().map(Value::String)
    }
}

impl<'a> TryFrom<&'a Absolute<std::path::Path>> for Value {
    type Error = &'a Absolute<std::path::Path>;
    #[inline]
    fn try_from(value: &'a Absolute<std::path::Path>) -> Result<Self, Self::Error> {
        value.try_into().map(Value::String)
    }
}

impl From<Vec<Value>> for Value {
    #[inline]
    fn from(v: Vec<Value>) -> Self {
        Value::List(v)
    }
}

impl Value {
    #[inline]
    #[must_use]
    #[expect(clippy::cast_sign_loss)]
    pub fn index(&self, index: i32) -> Option<&Value> {
        match (self, index) {
            (Value::String(_), 0 | -1) => Some(self),
            (Value::String(_), _) => None,
            (Value::List(list), index) if index >= 0 => list.get(index as usize),
            (Value::List(list), index) => {
                let actual_index = list.len().checked_sub(-index as usize)?;
                Some(&list[actual_index])
            }
        }
    }

    #[inline]
    #[must_use]
    #[expect(clippy::cast_sign_loss)]
    pub fn index_mut(&mut self, index: i32) -> Option<&mut Value> {
        match (self, index) {
            (this @ Value::String(_), 0 | -1) => Some(this),
            (Value::String(_), _) => None,
            (Value::List(list), index) => {
                if index >= 0 {
                    list.get_mut(index as usize)
                } else {
                    list.len()
                        .checked_sub(-index as usize)
                        .map(|actual_index| &mut list[actual_index])
                }
            }
        }
    }

    #[inline]
    #[must_use]
    pub fn len(&self) -> usize {
        match self {
            Value::List(values) => values.len(),
            Value::String(_) => 1,
        }
    }

    #[inline]
    #[must_use]
    pub fn is_empty(&self) -> bool {
        match self {
            Value::List(values) => values.is_empty(),
            Value::String(_) => false,
        }
    }

    pub fn visit<V: FnMut(StringValue)>(self, mut visitor: V) {
        fn visit<V: FnMut(StringValue)>(this: Value, visitor: &mut V) {
            match this {
                Value::List(values) => {
                    for value in values {
                        visit(value, visitor);
                    }
                }
                Value::String(string_value) => visitor(string_value),
            }
        }
        visit(self, &mut visitor);
    }

    pub fn try_visit<V: FnMut(StringValue) -> Result<(), E>, E>(
        self,
        mut visitor: V,
    ) -> Result<(), E> {
        fn try_visit<V: FnMut(StringValue) -> Result<(), E>, E>(
            this: Value,
            visitor: &mut V,
        ) -> Result<(), E> {
            match this {
                Value::List(vec) => {
                    for value in vec {
                        try_visit(value, visitor)?;
                    }
                    Ok(())
                }
                Value::String(s) => visitor(s),
            }
        }

        try_visit(self, &mut visitor)
    }

    pub fn visit_ref<V: FnMut(&StringValue)>(&self, mut visitor: V) {
        fn visit_ref<V: FnMut(&StringValue)>(this: &Value, visitor: &mut V) {
            match this {
                Value::List(vec) => {
                    for value in vec {
                        visit_ref(value, visitor);
                    }
                }
                Value::String(s) => visitor(s),
            }
        }

        visit_ref(self, &mut visitor);
    }

    pub fn try_visit_ref<V: FnMut(&StringValue) -> Result<(), E>, E>(
        &self,
        mut visitor: V,
    ) -> Result<(), E> {
        fn try_visit_ref<V: FnMut(&StringValue) -> Result<(), E>, E>(
            this: &Value,
            visitor: &mut V,
        ) -> Result<(), E> {
            match this {
                Value::List(vec) => {
                    for value in vec {
                        try_visit_ref(value, visitor)?;
                    }
                    Ok(())
                }
                Value::String(s) => visitor(s),
            }
        }

        try_visit_ref(self, &mut visitor)
    }

    pub fn visit_mut<V: FnMut(&mut StringValue)>(&mut self, mut visitor: V) {
        fn visit_mut<V: FnMut(&mut StringValue)>(this: &mut Value, visitor: &mut V) {
            match this {
                Value::List(vec) => {
                    for value in vec {
                        visit_mut(value, visitor);
                    }
                }
                Value::String(s) => visitor(s),
            }
        }

        visit_mut(self, &mut visitor);
    }

    pub fn try_visit_mut<V: FnMut(&mut StringValue) -> Result<(), E>, E>(
        &mut self,
        visitor: &mut V,
    ) -> Result<(), E> {
        fn try_visit_mut<V: FnMut(&mut StringValue) -> Result<(), E>, E>(
            this: &mut Value,
            visitor: &mut V,
        ) -> Result<(), E> {
            match this {
                Value::List(vec) => {
                    for value in vec {
                        try_visit_mut(value, visitor)?;
                    }
                    Ok(())
                }
                Value::String(s) => visitor(s),
            }
        }

        try_visit_mut(self, visitor)
    }

    /// Map strings in place, preserving the structure of the value.
    pub fn try_map_strings<V: FnMut(StringValue) -> Result<Value, E>, E>(
        mut self,
        mut map: V,
    ) -> Result<Value, E> {
        fn try_map_strings<V: FnMut(StringValue) -> Result<Value, E>, E>(
            this: &mut Value,
            visitor: &mut V,
        ) -> Result<(), E> {
            match this {
                Value::List(ref mut values) => {
                    for value in values {
                        try_map_strings(value, visitor)?;
                    }
                }
                Value::String(string_value) => {
                    *this = visitor(std::mem::take(string_value))?;
                }
            }
            Ok(())
        }

        try_map_strings(&mut self, &mut map)?;
        Ok(self)
    }

    pub fn collect_strings_into(self, strings: &mut Vec<StringValue>) {
        self.visit(|s| strings.push(s));
    }

    #[must_use]
    pub fn display_friendly(&self, max_width: usize) -> DisplayFriendly {
        DisplayFriendly(self, max_width)
    }
}

impl<T> PartialEq<[T]> for Value
where
    Value: PartialEq<T>,
{
    #[inline]
    fn eq(&self, other: &[T]) -> bool {
        match self {
            Value::List(v) => v.iter().zip(other.iter()).all(|(a, b)| a == b),
            Value::String(..) => false,
        }
    }
}

impl<T, const N: usize> PartialEq<[T; N]> for Value
where
    Value: PartialEq<T>,
{
    #[inline]
    fn eq(&self, other: &[T; N]) -> bool {
        self == other as &[T]
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::List(vec) => {
                f.write_str("[")?;
                for (i, item) in vec.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{item}")?;
                }
                f.write_str("]")
            }
            Value::String(s) => f.write_str(s),
        }
    }
}

/// Display helper for values.
pub struct DisplayFriendly<'a>(&'a Value, usize);
impl std::fmt::Display for DisplayFriendly<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn string_with_ellipsis(s: &str, max_width: usize) -> String {
            let escaped = s.escape_default().to_string();
            let escaped_len = escaped.chars().count();

            if escaped_len + 2 <= max_width {
                // The whole string fits.
                format!("\"{escaped}\"")
            } else if max_width >= 8 {
                // If we can write at least 3 chars from the string, write 8 chars.
                let prefix = escaped.chars().take(3).collect::<String>();
                format!("\"{prefix}...\"")
            } else {
                String::from("\"...\"")
            }
        }

        fn list_with_ellipsis(l: &[Value], max_width: usize) -> String {
            let mut rem_width = max_width.saturating_sub(2); // '[' and ']'

            let mut s = String::from("[");
            for (i, item) in l.iter().enumerate() {
                let is_first = i == 0;

                if !is_first {
                    s.push_str(", ");
                    rem_width = rem_width.saturating_sub(2);
                }

                let item_string = value_with_ellipsis(item, max_width);
                let item_len = item_string.chars().count();
                if item_len > rem_width {
                    s.push_str("...");
                    break;
                }
                s.push_str(&item_string);
                rem_width = rem_width.saturating_sub(item_len);
            }
            s.push(']');
            s
        }

        fn value_with_ellipsis(value: &Value, max_width: usize) -> String {
            match value {
                Value::List(vec) => list_with_ellipsis(vec, max_width),
                Value::String(s) => string_with_ellipsis(s, max_width),
            }
        }

        let DisplayFriendly(value, max_width) = self;
        f.write_str(&value_with_ellipsis(value, *max_width))
    }
}
