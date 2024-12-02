use std::future::Future;

#[derive(Debug, Clone)]
pub enum Value {
    List(Vec<Value>),
    String(String),
}

impl From<String> for Value {
    #[inline]
    fn from(s: String) -> Self {
        Value::String(s)
    }
}

impl From<&str> for Value {
    #[inline]
    fn from(s: &str) -> Self {
        Value::String(s.to_owned())
    }
}

impl From<Vec<Value>> for Value {
    #[inline]
    fn from(v: Vec<Value>) -> Self {
        Value::List(v)
    }
}

impl Value {
    pub fn try_collect_strings_recursive<F, E>(self, mut f: F) -> Result<(), E>
    where
        F: FnMut(String) -> Result<(), E>,
    {
        fn try_collect_strings_recursive<E>(
            this: Value,
            f: &mut impl FnMut(String) -> Result<(), E>,
        ) -> Result<(), E> {
            match this {
                Value::List(vec) => {
                    for s in vec {
                        try_collect_strings_recursive(s, f)?;
                    }
                    Ok(())
                }
                Value::String(s) => f(s),
            }
        }

        try_collect_strings_recursive(self, &mut f)
    }

    pub fn collect_strings(self) -> Vec<String> {
        let mut strings = Vec::new();
        self.collect_strings_into(&mut strings);
        strings
    }

    pub fn collect_strings_into(self, strings: &mut Vec<String>) {
        match self {
            Value::List(vec) => {
                for s in vec {
                    s.collect_strings_into(strings);
                }
            }
            Value::String(s) => strings.push(s),
        }
    }

    pub fn for_each_string_recursive<F>(&self, mut f: F)
    where
        F: FnMut(&String),
    {
        fn for_each_string_recursive(this: &Value, f: &mut impl FnMut(&String)) {
            match this {
                Value::List(v) => {
                    for item in v {
                        for_each_string_recursive(item, f);
                    }
                }
                Value::String(s) => f(s),
            }
        }

        for_each_string_recursive(self, &mut f);
    }

    pub fn try_for_each_string_recursive<F, E>(&self, mut f: F) -> Result<(), E>
    where
        F: FnMut(&String) -> Result<(), E>,
    {
        fn try_for_each_string_recursive<E>(
            this: &Value,
            f: &mut impl FnMut(&String) -> Result<(), E>,
        ) -> Result<(), E> {
            match this {
                Value::List(v) => {
                    for item in v {
                        try_for_each_string_recursive(item, f)?;
                    }
                    Ok(())
                }
                Value::String(s) => f(s),
            }
        }

        try_for_each_string_recursive(self, &mut f)
    }

    pub fn try_recursive_map_strings<F, E>(&mut self, mut f: F) -> Result<(), E>
    where
        F: FnMut(String) -> Result<String, E>,
    {
        fn try_recursive_map<F, E>(this: &mut Value, f: &mut F) -> Result<(), E>
        where
            F: FnMut(String) -> Result<String, E>,
        {
            match this {
                Value::List(v) => {
                    for item in v {
                        try_recursive_map(item, f)?;
                    }
                    Ok(())
                }
                Value::String(s) => {
                    let value = std::mem::replace(s, String::new());
                    *s = f(value)?;
                    Ok(())
                }
            }
        }

        try_recursive_map(self, &mut f)
    }

    pub async fn try_recursive_map_strings_async<'a, F, E, Fut>(
        &mut self,
        mut f: F,
    ) -> Result<(), E>
    where
        F: FnMut(String) -> Fut + 'a,
        Fut: Future<Output = Result<String, E>> + 'a,
    {
        async fn try_recursive_map<'a, F, E, Fut>(this: &mut Value, f: &mut F) -> Result<(), E>
        where
            F: FnMut(String) -> Fut + 'a,
            Fut: Future<Output = Result<String, E>>,
        {
            match this {
                Value::List(v) => {
                    Box::pin(async move {
                        for item in v {
                            try_recursive_map(item, f).await?;
                        }
                        Ok(())
                    })
                    .await
                }
                Value::String(s) => {
                    let value = std::mem::replace(s, String::new());
                    *s = f(value).await?;
                    Ok(())
                }
            }
        }

        try_recursive_map(self, &mut f).await
    }

    pub fn try_recursive_modify<F, E>(&mut self, mut f: F) -> Result<(), E>
    where
        F: FnMut(&mut String) -> Result<(), E>,
    {
        fn try_recursive_modify<F, E>(this: &mut Value, f: &mut F) -> Result<(), E>
        where
            F: FnMut(&mut String) -> Result<(), E>,
        {
            match this {
                Value::List(v) => {
                    for item in v {
                        try_recursive_modify(item, f)?;
                    }
                    Ok(())
                }
                Value::String(s) => f(s),
            }
        }

        try_recursive_modify(self, &mut f)
    }

    pub fn recursive_modify<F>(&mut self, mut f: F)
    where
        F: FnMut(&mut String),
    {
        fn recursive_modify<F>(this: &mut Value, f: &mut F)
        where
            F: FnMut(&mut String),
        {
            match this {
                Value::List(v) => {
                    for item in v {
                        recursive_modify(item, f);
                    }
                }
                Value::String(s) => f(s),
            }
        }

        recursive_modify(self, &mut f);
    }
}

impl PartialEq<str> for Value {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        match self {
            Value::String(s) => s == other,
            _ => false,
        }
    }
}

impl PartialEq<&str> for Value {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        self == *other
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
            _ => false,
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
