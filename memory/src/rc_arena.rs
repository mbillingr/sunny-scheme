use std::cell::RefCell;
use std::rc::Rc;

pub struct RcArena<T, const CHUNK_SIZE: usize> {
    current_chunk: RcChunk<T>,
}

impl<T, const CHUNK_SIZE: usize> RcArena<T, CHUNK_SIZE> {
    pub fn new() -> Self {
        RcArena {
            current_chunk: RcChunk::with_capacity(CHUNK_SIZE),
        }
    }

    pub fn alloc(&mut self, value: T) -> Ref<T> {
        let ptr = self.current_chunk.push(value);
        let refval = Ref {
            chunk: self.current_chunk.clone(),
            ptr,
        };

        if self.current_chunk.is_full() {
            self.current_chunk = RcChunk::with_capacity(CHUNK_SIZE);
        }

        refval
    }
}

pub struct Ref<T> {
    chunk: RcChunk<T>,
    ptr: *mut T,
}

impl<T> Ref<T> {
    pub fn downgrade(this: &Ref<T>) -> Weak<T> {
        Weak {
            chunk: this.chunk.downgrade(),
            ptr: this.ptr,
        }
    }

    pub fn as_ptr(&self) -> *const T {
        self.ptr
    }
}

impl<T> Clone for Ref<T> {
    fn clone(&self) -> Self {
        Ref {
            chunk: self.chunk.clone(),
            ptr: self.ptr,
        }
    }
}

impl<T> std::ops::Deref for Ref<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe {
            // safety: ptr points into the chunk, which is kept alive by
            // the reference we hold. So derefencing the pointer is safe.
            &*self.ptr
        }
    }
}

impl<T> std::ops::DerefMut for Ref<T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe {
            // safety: ptr points into the chunk, which is kept alive by
            // the reference we hold. So derefencing the pointer is safe.
            &mut *self.ptr
        }
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Ref<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        (**self).fmt(f)
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Ref<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        (**self).fmt(f)
    }
}

impl<T: std::fmt::Pointer> std::fmt::Pointer for Ref<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        (**self).fmt(f)
    }
}

impl<T> PartialEq for Ref<T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.ptr, other.ptr)
    }
}

pub struct Weak<T> {
    chunk: WeakChunk<T>,
    ptr: *mut T,
}

impl<T> Weak<T> {
    pub fn upgrade(&self) -> Option<Ref<T>> {
        self.chunk.upgrade().map(|chunk| Ref {
            chunk,
            ptr: self.ptr,
        })
    }
}

struct RcChunk<T>(Rc<Chunk<T>>);

impl<T> RcChunk<T> {
    fn with_capacity(capacity: usize) -> Self {
        RcChunk(Rc::new(Chunk::with_capacity(capacity)))
    }

    fn downgrade(&self) -> WeakChunk<T> {
        WeakChunk(Rc::downgrade(&self.0))
    }
}

impl<T> Clone for RcChunk<T> {
    fn clone(&self) -> Self {
        RcChunk(self.0.clone())
    }
}

impl<T> std::ops::Deref for RcChunk<T> {
    type Target = Chunk<T>;

    fn deref(&self) -> &Chunk<T> {
        &self.0
    }
}

struct WeakChunk<T>(std::rc::Weak<Chunk<T>>);

impl<T> WeakChunk<T> {
    fn upgrade(&self) -> Option<RcChunk<T>> {
        self.0.upgrade().map(|chunk| RcChunk(chunk))
    }
}

struct Chunk<T> {
    slots: RefCell<Vec<T>>,
}

impl<T> Chunk<T> {
    fn with_capacity(capacity: usize) -> Self {
        Chunk {
            slots: RefCell::new(Vec::with_capacity(capacity)),
        }
    }

    fn is_full(&self) -> bool {
        let data = self.slots.borrow();
        return data.len() >= data.capacity();
    }

    fn push(&self, value: T) -> *mut T {
        let mut data = self.slots.borrow_mut();
        data.push(value);
        data.last_mut().unwrap()
    }
}

#[derive(Debug)]
struct Report(&'static str);

impl Drop for Report {
    fn drop(&mut self) {
        println!("dropping {}", &self.0)
    }
}
