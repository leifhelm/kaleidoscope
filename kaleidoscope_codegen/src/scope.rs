use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

pub(crate) trait Scoped<K, V> {
    fn get<Q>(&self, key: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: ?Sized + Hash + Eq;
    fn insert<Q>(&mut self, key: &Q, value: V) -> Option<V>
    where
        K: Borrow<Q>,
        Q: ?Sized + ToOwned<Owned = K> + Hash + Eq;
    fn enter(&mut self);
    fn leave(&mut self);
    fn reserve(&mut self, additional: usize);
}

pub(crate) fn scoped<S, K, V, T>(s: &mut S, f: impl FnOnce(&mut S) -> T) -> T
where
    S: Scoped<K, V>,
{
    s.enter();
    let res = f(s);
    s.leave();
    res
}

pub(crate) struct ScopedHashMap<K, V> {
    values: HashMap<K, V>,
    old_values: Vec<HashMap<K, Option<V>>>,
}
impl<K, V> ScopedHashMap<K, V> {
    pub(crate) fn new() -> Self {
        ScopedHashMap {
            values: HashMap::new(),
            old_values: Vec::new(),
        }
    }
}
impl<K, V> Scoped<K, V> for ScopedHashMap<K, V>
where
    K: Hash + Eq,
    V: Clone,
{
    fn get<Q>(&self, key: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        HashMap::get(&self.values, key).map(|value| value.to_owned())
    }

    fn insert<Q>(&mut self, key: &Q, value: V) -> Option<V>
    where
        K: Borrow<Q>,
        Q: ?Sized + ToOwned<Owned = K> + Hash + Eq,
    {
        let old_value = self.values.insert(key.to_owned(), value);
        match self.old_values.last_mut() {
            Some(old_values) => match old_value {
                Some(old_value) => {
                    if old_values.contains_key(key.borrow()) {
                        Some(old_value)
                    } else {
                        old_values.insert(key.to_owned(), Some(old_value));
                        None
                    }
                }
                None => {
                    old_values.insert(key.to_owned(), None);
                    None
                }
            },
            None => old_value,
        }
    }

    fn enter(&mut self) {
        self.old_values.push(HashMap::new());
    }

    fn leave(&mut self) {
        for (key, value) in self.old_values.pop().expect("Wanting to leave last scope") {
            match value {
                Some(value) => self.values.insert(key, value),
                None => self.values.remove(&key),
            };
        }
    }

    fn reserve(&mut self, additional: usize) {
        self.values.reserve(additional);
        match self.old_values.last_mut() {
            Some(old_values) => old_values.reserve(additional),
            None => (),
        };
    }
}

pub(crate) trait HasScoped {
    type Scope;
    fn get_scope(&self) -> &Self::Scope;
    fn get_scope_mut(&mut self) -> &mut Self::Scope;
}

impl<HS, K, V, S> Scoped<K, V> for HS
where
    HS: HasScoped<Scope = S>,
    S: Scoped<K, V>,
{
    fn get<Q>(&self, key: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: ?Sized + Hash + Eq,
    {
        self.get_scope().get(key)
    }

    fn insert<Q>(&mut self, key: &Q, value: V) -> Option<V>
    where
        K: Borrow<Q>,
        Q: ?Sized + ToOwned<Owned = K> + Hash + Eq,
    {
        self.get_scope_mut().insert(key, value)
    }

    fn enter(&mut self) {
        self.get_scope_mut().enter()
    }

    fn leave(&mut self) {
        self.get_scope_mut().leave()
    }

    fn reserve(&mut self, additional: usize) {
        self.get_scope_mut().reserve(additional)
    }
}
