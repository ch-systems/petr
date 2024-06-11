use std::marker::PhantomData;

pub struct IndexMap<K, V> {
    _key:    PhantomData<K>,
    entries: Vec<V>,
}

impl<K, V> std::fmt::Debug for IndexMap<K, V> where V: std::fmt::Debug
{
    fn fmt(&self,
           f: &mut std::fmt::Formatter<'_>)
           -> std::fmt::Result {
        {
            f.debug_struct("IndexMap")
             .field("entries", &self.entries)
             .finish()
        }
    }
}

impl<K, V> Clone for IndexMap<K, V>
    where K: Clone,
          V: Clone
{
    fn clone(&self) -> Self {
        IndexMap { _key:    self._key,
                   entries: self.entries.clone(), }
    }
}

impl<K, V> IndexMap<K, V> {
    pub fn len(&self) -> usize {
        self.entries.len()
    }
}

impl<K, V> Default for IndexMap<K, V> where K: From<usize> + Into<usize>
{
    fn default() -> Self {
        Self { entries: Default::default(),
               _key:    PhantomData, }
    }
}

impl<K, V> IndexMap<K, V> where K: From<usize> + Into<usize>
{
    pub fn iter(&self) -> impl Iterator<Item = (K, &V)> {
        self.entries
            .iter()
            .enumerate()
            .map(|(k, v)| (K::from(k), v))
    }

    pub fn insert(&mut self,
                  value: V)
                  -> K {
        let key = self.entries.len();
        self.entries.push(value);
        key.into()
    }

    pub fn get(&self,
               k: K)
               -> &V {
        &self.entries[k.into()]
    }

    pub fn get_mut(&mut self,
                   k: K)
                   -> &mut V {
        self.entries
            .get_mut(k.into())
            .expect("IDs are handed out by insertion, so this should never fail")
    }

    pub fn into_iter(self) -> impl Iterator<Item = (K, V)> {
        self.entries
            .into_iter()
            .enumerate()
            .map(|(i, v)| (i.into(), v))
    }
}

impl<K, V> IndexMap<K, V>
    where K: From<usize>,
          V: PartialEq
{
    pub fn contains_value(&self,
                          value: V)
                          -> Option<K> {
        self.entries
            .iter()
            .position(|v| *v == value)
            .map(Into::into)
    }
}

#[macro_export]
macro_rules! idx_map_key {
    ($(#[$attr:meta])*
        $name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd)]
        $(#[$attr])*
        pub struct $name(usize);

        impl From<usize> for $name {
            fn from(value: usize) -> Self {
                Self(value)
            }
        }

        impl Into<usize> for $name {
            fn into(self) -> usize {
                self.0
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}{}", stringify!($name).to_lowercase(), self.0)
            }
        }
    };
}
