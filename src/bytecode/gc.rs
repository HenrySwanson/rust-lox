use std::collections::{HashMap, HashSet};
use std::rc::Rc;

pub struct GcHeap<T> {
    // The object itself lives at the pointer location; the Rc is only used
    // for reference counting.
    objects: HashMap<*mut T, Rc<()>>,
}

// Handle referring to an object in the heap. This handle doesn't root the object,
// so it is up to the user to only hold onto GcPtrs that correspond to reachable
// objects.
pub struct GcPtr<T> {
    // TODO stop making it pub
    pub raw_ptr: *mut T,
}

// Like GcPtr, but also roots the object (so that it will not be garbage collected).
pub struct GcStrong<T> {
    ptr: GcPtr<T>, // rather this than raw_ptr so that we can impl AsRef
    counter: Rc<()>, // reference counting for the pointer
}

pub trait Traceable {
    fn trace(&self) -> Vec<GcPtr<Self>>
    where
        Self: Sized;
}

impl<T: Traceable> GcHeap<T> {
    pub fn new() -> Self {
        GcHeap {
            objects: HashMap::new(),
        }
    }

    pub fn insert(&mut self, obj: T) -> GcStrong<T> {
        let ptr = Box::into_raw(Box::new(obj));
        let rc = Rc::new(());

        self.objects.insert(ptr, rc.clone());

        GcStrong {
            ptr: GcPtr{raw_ptr: ptr},
            counter: rc,
        }
    }

    pub fn get(&self, handle: impl AsRef<GcPtr<T>>) -> Option<&T> {
        let raw_ptr = handle.as_ref().raw_ptr;
        if self.objects.contains_key(&raw_ptr) {
            let t_ref: &T = unsafe { &*raw_ptr };
            Some(t_ref)
        } else {
            None
        }
    }

    pub fn get_unchecked(&self, handle: impl AsRef<GcPtr<T>>) -> &T {
        self.get(handle).expect("Object not found in heap")
    }

    pub fn get_mut(&mut self, handle: impl AsRef<GcPtr<T>>) -> Option<&mut T> {
        let raw_ptr = handle.as_ref().raw_ptr;
        if self.objects.contains_key(&raw_ptr) {
            let t_ref: &mut T = unsafe { &mut *raw_ptr };
            Some(t_ref)
        } else {
            None
        }
    }

    pub fn get_mut_unchecked(&mut self, handle: impl AsRef<GcPtr<T>>) -> &mut T {
        self.get_mut(handle).expect("Object not found in heap")
    }

    // TODO we could get fancy and do a tricolor thing
    pub fn collect_garbage(&mut self) {
        let mut frontier: Vec<*mut T> = vec![];

        // Create the frontier set; our objects with Rc-count > 1.
        for (ptr, counter) in self.objects.iter() {
            if Rc::strong_count(counter) > 1 {
                frontier.push(*ptr);
            }
        }

        // Continuously explore the frontier, marking it as we go
        let mut marked = HashSet::new();
        while let Some(ptr) = frontier.pop() {
            // Mark the node, but if it's already marked, don't recurse on it
            if marked.insert(ptr) {
                let obj = unsafe { &*ptr };
                for gc_ptr in obj.trace() {
                    frontier.push(gc_ptr.raw_ptr);
                }
            }
        }

        // Now kill all the objects that aren't marked
        self.objects.retain(|ptr, _| marked.contains(ptr));
    }
}

impl<T> Drop for GcHeap<T> {
    fn drop(&mut self) {
        for ptr in self.objects.keys() {
            let b = unsafe { Box::from_raw(*ptr) };
            drop(b)
        }
    }
}

// ---- GcPtr impls ----

impl<T> Clone for GcPtr<T> {
    fn clone(&self) -> Self {
        GcPtr {
            raw_ptr: self.raw_ptr,
        }
    }
}

impl<T> PartialEq<GcPtr<T>> for GcPtr<T> {
    fn eq(&self, other: &Self) -> bool {
        self.raw_ptr == other.raw_ptr
    }
}

impl<T> Eq for GcPtr<T> {}

impl<T> AsRef<GcPtr<T>> for GcPtr<T> {
    fn as_ref(&self) -> &GcPtr<T> {
        &self
    }
}

// ---- GcStrong impls ----

impl<T> GcStrong<T> {
    pub fn downgrade(&self) -> GcPtr<T> {
        self.ptr.clone()
    }
}

impl<T> Clone for GcStrong<T> {
    fn clone(&self) -> Self {
        GcStrong {
            ptr: self.ptr.clone(),
            counter: self.counter.clone(),
        }
    }
}

impl<T> PartialEq<GcStrong<T>> for GcStrong<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl<T> Eq for GcStrong<T> {}

impl<T> AsRef<GcPtr<T>> for GcStrong<T> {
    fn as_ref(&self) -> &GcPtr<T> {
        &self.ptr
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Traceable for String {
        fn trace(&self) -> Vec<GcPtr<Self>> {
            vec![]
        }
    }

    // Simple struct for testing references
    struct Foo {
        next: Option<GcPtr<Foo>>,
    }

    impl Foo {
        fn new() -> Self {
            Foo { next: None }
        }

        fn new_with_next(f: GcPtr<Foo>) -> Self {
            Foo { next: Some(f) }
        }
    }

    impl Traceable for Foo {
        fn trace(&self) -> Vec<GcPtr<Self>> {
            match &self.next {
                Some(s) => vec![s.clone()],
                None => vec![],
            }
        }
    }

    #[test]
    fn heap_test_simple() {
        let mut heap = GcHeap::<String>::new();

        let ptr_a = heap.insert("A".to_owned());
        let ptr_b = heap.insert("B".to_owned());

        let weak_ptr_a = ptr_a.downgrade();
        let weak_ptr_b = ptr_b.downgrade();

        assert_eq!("A", heap.get(&weak_ptr_a).unwrap());
        assert_eq!("B", heap.get(&weak_ptr_b).unwrap());

        heap.collect_garbage();

        assert_eq!("A", heap.get(&weak_ptr_a).unwrap());
        assert_eq!("B", heap.get(&weak_ptr_b).unwrap());

        std::mem::drop(ptr_a);
        heap.collect_garbage();

        assert_eq!(None, heap.get(&weak_ptr_a));
        assert_eq!("B", heap.get(&weak_ptr_b).unwrap());

        std::mem::drop(ptr_b);
        heap.collect_garbage();

        assert_eq!(None, heap.get(&weak_ptr_b));
    }

    #[test]
    fn heap_test_recursive() {
        let mut heap = GcHeap::<Foo>::new();

        let ptr_2 = heap.insert(Foo::new());
        let ptr_1 = heap.insert(Foo::new_with_next(ptr_2.downgrade()));

        let weak_ptr_1 = ptr_1.downgrade();
        let weak_ptr_2 = ptr_2.downgrade();

        assert!(heap.get(&weak_ptr_1).is_some());
        assert!(heap.get(&weak_ptr_2).is_some());

        heap.collect_garbage();

        assert!(heap.get(&weak_ptr_1).is_some());
        assert!(heap.get(&weak_ptr_2).is_some());

        // Dropping the pointer to #2 should be fine; #1 still maintains a reference to it
        std::mem::drop(ptr_2);
        heap.collect_garbage();

        assert!(heap.get(&weak_ptr_1).is_some());
        assert!(heap.get(&weak_ptr_2).is_some());

        // But after dropping #1, both should get collected
        std::mem::drop(ptr_1);
        heap.collect_garbage();

        assert!(heap.get(&weak_ptr_1).is_none());
        assert!(heap.get(&weak_ptr_2).is_none());
    }

    #[test]
    fn heap_test_cycles() {
        let mut heap = GcHeap::<Foo>::new();

        let ptr_3 = heap.insert(Foo::new());
        let ptr_2 = heap.insert(Foo::new_with_next(ptr_3.downgrade()));
        let ptr_1 = heap.insert(Foo::new_with_next(ptr_2.downgrade()));

        // Now set #3's next to point to #1
        heap.get_mut_unchecked(&ptr_3.downgrade()).next = Some(ptr_1.downgrade());

        let weak_ptr_1 = ptr_1.downgrade();
        let weak_ptr_2 = ptr_2.downgrade();
        let weak_ptr_3 = ptr_3.downgrade();

        assert!(heap.get(&weak_ptr_1).is_some());
        assert!(heap.get(&weak_ptr_2).is_some());
        assert!(heap.get(&weak_ptr_3).is_some());

        heap.collect_garbage();

        // Nothing should be collected here
        assert!(heap.get(&weak_ptr_1).is_some());
        assert!(heap.get(&weak_ptr_2).is_some());
        assert!(heap.get(&weak_ptr_3).is_some());

        // Keeping #3 alive keeps everyone alive
        std::mem::drop(ptr_1);
        std::mem::drop(ptr_2);
        heap.collect_garbage();

        assert!(heap.get(&weak_ptr_1).is_some());
        assert!(heap.get(&weak_ptr_2).is_some());
        assert!(heap.get(&weak_ptr_3).is_some());

        // Dropping #3 leaves the reference cycle, but it should
        // get collected anyways.
        std::mem::drop(ptr_3);
        heap.collect_garbage();

        assert!(heap.get(&weak_ptr_1).is_none());
        assert!(heap.get(&weak_ptr_2).is_none());
        assert!(heap.get(&weak_ptr_3).is_none());
    }
}
