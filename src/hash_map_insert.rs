use std::collections::hash_map::Entry;

pub trait Insert {
    type V;

    fn insert(self, value: Self::V);
}

impl<'s, K, V> Insert for Entry<'s, K, V> {
    type V = V;

    fn insert(self, value: V) {
        match self {
            Entry::Occupied(mut entry) => _ = entry.insert(value),
            Entry::Vacant(entry) => _ = entry.insert(value),
        }
    }
}
