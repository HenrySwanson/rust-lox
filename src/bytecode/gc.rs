use std::cell::{Cell, Ref, RefCell, RefMut};
use std::fmt;
use std::rc::Rc;

// Handle referring to an object in the heap. This handle doesn't "root" the object;
// if a sweep is made and the object is not marked, then the object will be deleted.
// It is up to the user to mark all objects reachable through a GcPtr
pub struct GcPtr<T> {
    ptr: Rc<GcBox<T>>,
}

// Owner of a heap-allocated object; contains the object and some garbage-collection
// metadata.
pub struct GcBox<T> {
    object: RefCell<Option<T>>,
    marked: Cell<bool>,
}

// Collection of GcBoxes, essentially. Responsible for the "sweep" in mark-and-sweep.
pub struct GcHeap<T: Traceable> {
    // Since the Rc needs to own the box, we really only hold GcPtrs. Every object
    // will be in this list exactly once.
    objects: Vec<GcPtr<T>>,
}

pub trait Traceable {
    // This function must obey the following criterion: if the object can produce a
    // GcPtr in _any_ way, that pointer must have mark() called on it. Failure to do
    // so may result in pointers to garbage.
    // (Safe garbage though; it'll just cause a panic.)
    fn trace(&self);
}

impl<T: Traceable> GcPtr<T> {
    pub fn try_borrow(&self) -> Ref<Option<T>> {
        self.ptr.object.borrow()
    }

    pub fn borrow(&self) -> Ref<T> {
        Ref::map(self.try_borrow(), |option| {
            option.as_ref().expect("Object was garbage collected!")
        })
    }

    pub fn try_borrow_mut(&self) -> RefMut<Option<T>> {
        self.ptr.object.borrow_mut()
    }

    pub fn borrow_mut(&mut self) -> RefMut<T> {
        RefMut::map(self.try_borrow_mut(), |option| {
            option.as_mut().expect("Object was garbage collected!")
        })
    }

    pub fn mark(&self) {
        // Only recurse if we're not marked yet
        if !self.ptr.marked.get() {
            self.ptr.marked.set(true);
            self.borrow().trace();
        }
    }

    fn unmark(&self) {
        self.ptr.marked.set(false);
    }

    fn is_marked(&self) -> bool {
        self.ptr.marked.get()
    }

    fn discard(&mut self) {
        // Get the Option<T> the object lives in, and then take it
        self.try_borrow_mut().take();
    }
}

impl<T> fmt::Debug for GcPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", Rc::as_ptr(&self.ptr))
    }
}

impl<T> Clone for GcPtr<T> {
    fn clone(&self) -> Self {
        GcPtr {
            ptr: self.ptr.clone(),
        }
    }
}

impl<T> PartialEq<GcPtr<T>> for GcPtr<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.ptr, &other.ptr)
    }
}

impl<T> Eq for GcPtr<T> {}

impl<T: Traceable> GcHeap<T> {
    pub fn new() -> Self {
        GcHeap { objects: vec![] }
    }

    pub fn insert(&mut self, obj: T) -> GcPtr<T> {
        let gc_box = GcBox {
            object: RefCell::new(Some(obj)),
            marked: Cell::new(false),
        };

        // Before we hand out the pointer, make a copy for ourselves
        let gc_ptr = GcPtr {
            ptr: Rc::new(gc_box),
        };

        self.objects.push(gc_ptr.clone());
        gc_ptr
    }

    pub fn sweep(&mut self) {
        // Discard all unmarked objects
        for ptr in self.objects.iter_mut() {
            if !ptr.is_marked() {
                ptr.discard();
            }
        }

        // Remove all handles to the deleted objects
        // TODO: is there a retain-and-modify-removed elements?
        self.objects.retain(|ptr| ptr.is_marked());

        // And reset their marks
        for ptr in self.objects.iter() {
            ptr.unmark();
        }
    }
}

impl<T: Traceable> Drop for GcHeap<T> {
    // We have to implement a custom drop; not because of unsafety, but because
    // it's our responsibility to kill everything in the heap
    fn drop(&mut self) {
        for ptr in self.objects.iter_mut() {
            ptr.discard();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl Traceable for String {
        fn trace(&self) {}
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
        fn trace(&self) {
            if let Some(next) = &self.next {
                next.mark();
            }
        }
    }

    #[test]
    fn heap_test_simple() {
        let mut heap = GcHeap::<String>::new();

        let ptr_a = heap.insert("A".to_owned());
        let ptr_b = heap.insert("B".to_owned());

        assert_eq!("A", *ptr_a.borrow());
        assert_eq!("B", *ptr_b.borrow());

        ptr_a.mark();
        ptr_b.mark();
        heap.sweep();

        assert_eq!("A", *ptr_a.borrow());
        assert_eq!("B", *ptr_b.borrow());

        // don't mark a
        ptr_b.mark();
        heap.sweep();

        assert_eq!(None, *ptr_a.try_borrow());
        assert_eq!("B", *ptr_b.borrow());

        // mark nothing
        heap.sweep();

        assert_eq!(None, *ptr_a.try_borrow());
        assert_eq!(None, *ptr_b.try_borrow());
    }

    #[test]
    fn heap_test_recursive() {
        let mut heap = GcHeap::<Foo>::new();

        let ptr_2 = heap.insert(Foo::new());
        let ptr_1 = heap.insert(Foo::new_with_next(ptr_2.clone()));

        assert!(ptr_1.try_borrow().is_some());
        assert!(ptr_2.try_borrow().is_some());

        ptr_1.mark();
        ptr_2.mark();
        heap.sweep();

        assert!(ptr_1.try_borrow().is_some());
        assert!(ptr_2.try_borrow().is_some());

        // Not marking pointer #2 should be fine; #1 still maintains a reference to it
        ptr_1.mark();
        heap.sweep();

        assert!(ptr_1.try_borrow().is_some());
        assert!(ptr_2.try_borrow().is_some());

        // But if neither are marked, both should be collected.
        heap.sweep();

        assert!(ptr_1.try_borrow().is_none());
        assert!(ptr_2.try_borrow().is_none());
    }

    #[test]
    fn heap_test_cycles() {
        let mut heap = GcHeap::<Foo>::new();

        let mut ptr_3 = heap.insert(Foo::new());
        let ptr_2 = heap.insert(Foo::new_with_next(ptr_3.clone()));
        let ptr_1 = heap.insert(Foo::new_with_next(ptr_2.clone()));

        // Now set #3's next to point to #
        ptr_3.borrow_mut().next = Some(ptr_1.clone());

        assert!(ptr_1.try_borrow().is_some());
        assert!(ptr_2.try_borrow().is_some());
        assert!(ptr_3.try_borrow().is_some());

        ptr_1.mark();
        ptr_2.mark();
        ptr_3.mark();
        heap.sweep();

        // Nothing should be collected here
        assert!(ptr_1.try_borrow().is_some());
        assert!(ptr_2.try_borrow().is_some());
        assert!(ptr_3.try_borrow().is_some());

        // Keeping #3 alive keeps everyone alive
        ptr_3.mark();
        heap.sweep();

        assert!(ptr_1.try_borrow().is_some());
        assert!(ptr_2.try_borrow().is_some());
        assert!(ptr_3.try_borrow().is_some());

        // Marking nothing means there is a reference cycle, which should
        // be collected.
        heap.sweep();

        assert!(ptr_1.try_borrow().is_none());
        assert!(ptr_2.try_borrow().is_none());
        assert!(ptr_3.try_borrow().is_none());
    }
}
