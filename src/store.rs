use serde::{Deserialize, Serialize};
use std::marker::PhantomData;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct ID<T> {
    id: usize,
    // This goes away after compliation
    _marker: PhantomData<T>,
}

impl<T> ID<T> {
    pub fn from(id: usize) -> ID<T> {
        ID {
            id,
            _marker: PhantomData,
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Store<T> {
    items: Vec<T>,
}

impl<T> Store<T> {
    pub fn new() -> Self {
        Store { items: vec![] }
    }

    pub fn insert(&mut self, item: T) -> ID<T> {
        self.items.push(item);
        let id = self.items.len() - 1;
        ID {
            id,
            _marker: PhantomData,
        }
    }

    pub fn get(&self, id: ID<T>) -> Option<&T> {
        self.items.get(id.id)
    }

    pub fn get_mut(&mut self, id: ID<T>) -> Option<&mut T> {
        self.items.get_mut(id.id)
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ReadStore<T> {
    items: Vec<T>,
}

impl<T> ReadStore<T> {
    pub fn new(store: Store<T>) -> Self {
        ReadStore { items: store.items }
    }

    pub fn get(&self, id: ID<T>) -> Option<&T> {
        self.items.get(id.id)
    }
}
