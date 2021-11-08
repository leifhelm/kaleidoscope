use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

pub(crate) trait State<S, A> {
    fn run(self, s: S) -> (A, S);
}

impl<S, A, F> State<S, A> for F
where
    F: FnOnce(S) -> (A, S),
{
    fn run(self, s: S) -> (A, S) {
        self(s)
    }
}

pub(crate) fn pure<S, A>(pure: A) -> impl State<S, A> {
    move |s| (pure, s)
}

pub(crate) fn bind<S, A, B, SB>(
    state: impl State<S, A>,
    f: impl FnOnce(A) -> SB,
) -> impl State<S, B>
where
    SB: State<S, B>,
{
    move |s| {
        let (a, s) = state.run(s);
        f(a).run(s)
    }
}

pub(crate) fn bind_<S, A, B>(a: impl State<S, A>, b: impl State<S, B>) -> impl State<S, B> {
    bind(a, |_| b)
}

pub(crate) fn modify<S>(f: impl FnOnce(S) -> S) -> impl State<S, ()> {
    move |s| ((), f(s))
}

pub(crate) fn map<S, A, B>(state: impl State<S, A>, f: impl FnOnce(A) -> B) -> impl State<S, B> {
    move |s| {
        let (a, s) = state.run(s);
        (f(a), s)
    }
}

pub(crate) fn try_bind<S, A, B, SB, E>(
    state: impl State<S, Result<A, E>>,
    f: impl FnOnce(A) -> Result<SB, E>,
) -> impl State<S, Result<B, E>>
where
    SB: State<S, Result<B, E>>,
{
    move |s| {
        let (a, s) = state.run(s);
        match a {
            Ok(a) => match f(a) {
                Ok(state) => state.run(s),
                Err(err) => (Err(err), s),
            },
            Err(err) => (Err(err), s),
        }
    }
}
pub(crate) fn try_bind_<S, A, B, SB, E>(
    state: impl State<S, Result<A, E>>,
    f: impl FnOnce(A) -> SB,
) -> impl State<S, Result<B, E>>
where
    SB: State<S, Result<B, E>>,
{
    try_bind(state, |a| Ok(f(a)))
}

pub(crate) fn try_result<S, A, E>(result: (Result<A, E>, S)) -> Result<(A, S), (E, S)> {
    match result {
        (Ok(a), s) => Ok((a, s)),
        (Err(err), s) => Err((err, s)),
    }
}

pub(crate) fn catch<S, A, E>(
    state: impl FnOnce(S) -> Result<(A, S), (E, S)>,
) -> impl State<S, Result<A, E>> {
    move |s| match state(s) {
        Ok((ok, s)) => (Ok(ok), s),
        Err((err, s)) => (Err(err), s),
    }
}

pub(crate) fn pure_err<S, A, E>(result: Result<A, E>, s: S) -> Result<(A, S), (E, S)> {
    match result {
        Ok(ok) => Ok((ok, s)),
        Err(err) => Err((err, s)),
    }
}

pub(crate) trait Scoped<K, V> {
    fn new() -> Self;
    fn get(&self, key: &K) -> Option<V>;
    fn insert<Q>(&mut self, key: &Q, value: V) -> Option<V>
    where
        K: Borrow<Q>,
        Q: ?Sized + ToOwned<Owned = K> + Hash + Eq;
    fn enter(self) -> Self;
    fn leave(self) -> Self;
}

pub(crate) fn get_scoped<'a, K, V, S, St>(key: &'a K) -> impl State<St, Option<V>> + 'a
where
    S: Scoped<K, V>,
    St: HasScoped<S>,
{
    move |s: St| {
        let value = HasScoped::get_scope(&s).get(key);
        (value, s)
    }
}

pub(crate) fn scoped<'a, K, V, S, St, A>(state: impl State<St, A>) -> impl State<St, A>
where
    S: Scoped<K, V>,
    St: HasScoped<S>,
{
    bind_(
        modify(|s| HasScoped::update(s, |s: S| s.enter())),
        bind(state, |res| {
            bind_(
                modify(|s| HasScoped::update(s, |s: S| s.leave())),
                pure(res),
            )
        }),
    )
}

pub(crate) struct ScopedHashMap<K, V> {
    values: HashMap<K, V>,
    old_values: Vec<HashMap<K, Option<V>>>,
}

pub(crate) trait HasScoped<S> {
    fn get_scope<'a>(&'a self) -> &'a S;
    fn get_scope_mut<'a>(&'a mut self) -> &'a mut S;
    fn update(self, f: impl FnOnce(S) -> S) -> Self;
}

impl<K, V> Scoped<K, V> for ScopedHashMap<K, V>
where
    K: Hash + Eq,
    V: Clone,
{
    fn new() -> Self {
        ScopedHashMap {
            values: HashMap::new(),
            old_values: Vec::new(),
        }
    }

    fn get(&self, key: &K) -> Option<V> {
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
            None => None,
        }
    }

    fn enter(mut self) -> Self {
        self.old_values.push(HashMap::new());
        self
    }

    fn leave(mut self) -> Self {
        for (key, value) in self.old_values.pop().expect("Wanting to leave last scope") {
            match value {
                Some(value) => self.values.insert(key, value),
                None => self.values.remove(&key),
            };
        }
        self
    }
}
