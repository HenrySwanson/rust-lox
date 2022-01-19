use std::cell::Cell;

#[derive(Debug)]
pub struct Gc<T> {
    index: usize,
    _marker: std::marker::PhantomData<*const T>,
}

#[derive(Debug)]
pub struct SubHeap<T> {
    objects: Vec<Option<HeapEntry<T>>>,
    free_list: Vec<usize>,
}

#[derive(Debug)]
struct HeapEntry<T> {
    content: T,
    marked: Cell<bool>,
}

impl<T> Gc<T> {
    // private, so that only SubHeap can create it
    fn new(index: usize) -> Self {
        Self {
            index,
            _marker: Default::default(),
        }
    }
}

impl<T> Copy for Gc<T> {}
impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<T> SubHeap<T> {
    pub fn new() -> Self {
        Self {
            objects: vec![],
            free_list: vec![],
        }
    }

    pub fn manage(&mut self, value: T) -> Gc<T> {
        let entry = Some(HeapEntry::new(value));
        match self.free_list.pop() {
            Some(index) => {
                self.objects[index] = entry;
                Gc::new(index)
            }
            None => {
                let index = self.objects.len();
                self.objects.push(entry);
                Gc::new(index)
            }
        }
    }

    fn get_entry(&self, ptr: Gc<T>) -> &HeapEntry<T> {
        let length = self.objects.len();
        match self.objects.get(ptr.index) {
            None => panic!(
                "gc pointed outside vector bounds: {} > {}",
                ptr.index, length
            ),
            Some(None) => panic!("gc pointed to freed object: {}", ptr.index),
            Some(Some(x)) => x,
        }
    }

    fn get_entry_mut(&mut self, ptr: Gc<T>) -> &mut HeapEntry<T> {
        let length = self.objects.len();
        match self.objects.get_mut(ptr.index) {
            Some(Some(x)) => x,
            Some(None) => panic!("gc pointed to freed object: {}", ptr.index),
            None => panic!(
                "gc pointed outside vector bounds: {} > {}",
                ptr.index, length
            ),
        }
    }

    pub fn get(&self, ptr: Gc<T>) -> &T {
        &self.get_entry(ptr).content
    }

    pub fn get_mut(&mut self, ptr: Gc<T>) -> &mut T {
        &mut self.get_entry_mut(ptr).content
    }

    pub fn mark_and_trace<'a, F>(&'a self, ptr: Gc<T>, trace: F)
    where
        F: Fn(&'a T) -> (),
    {
        // This is &self because we don't alter anything that isn't Cell.
        // More conceptually, we don't modify any of the structure of the heap.
        let entry = self.get_entry(ptr);
        if !entry.mark() {
            trace(&entry.content)
        }
    }

    pub fn sweep(&mut self) {
        // We must assume the user has correctly marked all their reachable objects.
        // Let's get sweeping.
        for (index, slot) in self.objects.iter_mut().enumerate() {
            // Skim through our object list, skipping empty slots
            if let Some(entry) = slot {
                // If it's marked, we just unmark it. Otherwise, we delete it and add its
                // index to the free list.
                if entry.is_marked() {
                    entry.unmark();
                } else {
                    *slot = None;
                    self.free_list.push(index);
                }
            }
        }
    }

    // TODO: should this count tombstones or not!?
    pub fn size(&self) -> usize {
        self.objects.len() * std::mem::size_of::<T>()
    }
}

impl<T> HeapEntry<T> {
    fn new(content: T) -> Self {
        Self {
            content,
            marked: Cell::new(false),
        }
    }
}

impl<T> HeapEntry<T> {
    pub fn mark(&self) -> bool {
        self.marked.replace(true)
    }

    pub fn unmark(&self) {
        self.marked.set(false);
    }

    pub fn is_marked(&self) -> bool {
        self.marked.get()
    }
}

pub trait Heap {
    fn sweep(&mut self);
}

pub trait HasSubHeap<T>: Heap {
    fn get_subheap(&self) -> &SubHeap<T>;
    fn get_subheap_mut(&mut self) -> &mut SubHeap<T>;
    fn trace(&self, content: &T);
}

pub trait Manages<T> {
    fn manage(&mut self, content: T) -> Gc<T>;
    fn get(&self, ptr: Gc<T>) -> &T;
    fn get_mut(&mut self, ptr: Gc<T>) -> &mut T;
    fn mark(&self, ptr: Gc<T>);
}

impl<H, T> Manages<T> for H
where
    H: HasSubHeap<T>,
{
    fn manage(&mut self, content: T) -> Gc<T> {
        self.get_subheap_mut().manage(content)
    }

    fn get(&self, ptr: Gc<T>) -> &T {
        self.get_subheap().get(ptr)
    }

    fn get_mut(&mut self, ptr: Gc<T>) -> &mut T {
        self.get_subheap_mut().get_mut(ptr)
    }

    fn mark(&self, ptr: Gc<T>) {
        self.get_subheap()
            .mark_and_trace(ptr, |content| self.trace(content))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // // Some helpful functions for testing
    impl<T> SubHeap<T> {
        fn definitely_invalid(&self, ptr: Gc<T>) -> bool {
            match self.objects.get(ptr.index) {
                Some(Some(_)) => false,
                Some(None) => true,
                None => true,
            }
        }

        fn number_alive(&self) -> usize {
            self.objects.iter().filter(|x| x.is_some()).count()
        }
    }

    struct StrHeap {
        heap: SubHeap<&'static str>,
    }

    impl StrHeap {
        fn new() -> Self {
            Self {
                heap: SubHeap::new(),
            }
        }
    }

    impl Heap for StrHeap {
        fn sweep(&mut self) {
            self.heap.sweep();
        }
    }

    impl HasSubHeap<&'static str> for StrHeap {
        fn get_subheap(&self) -> &SubHeap<&'static str> {
            &self.heap
        }

        fn get_subheap_mut(&mut self) -> &mut SubHeap<&'static str> {
            &mut self.heap
        }

        // do nothing
        fn trace(&self, _content: &&'static str) {}
    }

    #[test]
    fn heap_test_simple() {
        let mut heap = StrHeap::new();

        let ptr_a = heap.manage("A");
        let ptr_b = heap.manage("B");

        assert_eq!("A", *heap.get(ptr_a));
        assert_eq!("B", *heap.get(ptr_b));
        assert_eq!(heap.heap.number_alive(), 2);
        assert_eq!(heap.heap.free_list.len(), 0);

        heap.mark(ptr_a);
        heap.mark(ptr_b);
        heap.sweep();

        assert_eq!("A", *heap.get(ptr_a));
        assert_eq!("B", *heap.get(ptr_b));
        assert_eq!(heap.heap.number_alive(), 2);
        assert_eq!(heap.heap.free_list.len(), 0);

        // don't mark a
        heap.mark(ptr_b);
        heap.sweep();

        assert!(heap.heap.definitely_invalid(ptr_a));
        assert_eq!("B", *heap.get(ptr_b));
        assert_eq!(heap.heap.number_alive(), 1);
        assert_eq!(heap.heap.free_list.len(), 1);

        // mark nothing
        heap.sweep();

        assert!(heap.heap.definitely_invalid(ptr_a));
        assert!(heap.heap.definitely_invalid(ptr_b));
        assert_eq!(heap.heap.number_alive(), 0);
        assert_eq!(heap.heap.free_list.len(), 2);
    }

    // Simple struct for testing references
    struct Foo {
        next: Option<Gc<Foo>>,
    }

    impl Foo {
        fn new() -> Self {
            Foo { next: None }
        }

        fn new_with_next(f: Gc<Foo>) -> Self {
            Foo { next: Some(f) }
        }
    }

    struct FooHeap {
        heap: SubHeap<Foo>,
    }

    impl FooHeap {
        fn new() -> Self {
            Self {
                heap: SubHeap::new(),
            }
        }
    }

    impl Heap for FooHeap {
        fn sweep(&mut self) {
            self.heap.sweep();
        }
    }

    impl HasSubHeap<Foo> for FooHeap {
        fn get_subheap(&self) -> &SubHeap<Foo> {
            &self.heap
        }

        fn get_subheap_mut(&mut self) -> &mut SubHeap<Foo> {
            &mut self.heap
        }

        fn trace(&self, content: &Foo) {
            if let Some(next_foo) = content.next {
                self.mark(next_foo)
            }
        }
    }

    #[test]
    fn heap_test_recursive() {
        let mut heap = FooHeap::new();

        let ptr_2 = heap.manage(Foo::new());
        let ptr_1 = heap.manage(Foo::new_with_next(ptr_2));

        assert!(!heap.heap.definitely_invalid(ptr_1));
        assert!(!heap.heap.definitely_invalid(ptr_2));

        heap.mark(ptr_1);
        heap.mark(ptr_2);
        heap.sweep();

        assert!(!heap.heap.definitely_invalid(ptr_1));
        assert!(!heap.heap.definitely_invalid(ptr_2));

        // Not marking pointer #2 should be fine; #1 still maintains a reference to it
        heap.mark(ptr_1);
        heap.sweep();

        assert!(!heap.heap.definitely_invalid(ptr_1));
        assert!(!heap.heap.definitely_invalid(ptr_2));

        // But if neither are marked, both should be collected.
        heap.sweep();

        assert!(heap.heap.definitely_invalid(ptr_1));
        assert!(heap.heap.definitely_invalid(ptr_2));
    }

    #[test]
    fn heap_test_cycles() {
        let mut heap = FooHeap::new();

        let ptr_3 = heap.manage(Foo::new());
        let ptr_2 = heap.manage(Foo::new_with_next(ptr_3.clone()));
        let ptr_1 = heap.manage(Foo::new_with_next(ptr_2.clone()));

        // Now set #3's next to point to #
        heap.heap.get_mut(ptr_3).next = Some(ptr_1.clone());

        assert!(!heap.heap.definitely_invalid(ptr_1));
        assert!(!heap.heap.definitely_invalid(ptr_2));
        assert!(!heap.heap.definitely_invalid(ptr_3));

        heap.mark(ptr_1);
        heap.mark(ptr_2);
        heap.mark(ptr_3);
        heap.sweep();

        // Nothing should be collected here
        assert!(!heap.heap.definitely_invalid(ptr_1));
        assert!(!heap.heap.definitely_invalid(ptr_2));
        assert!(!heap.heap.definitely_invalid(ptr_3));

        // Keeping #3 alive keeps everyone alive
        heap.mark(ptr_3);
        heap.sweep();

        assert!(!heap.heap.definitely_invalid(ptr_1));
        assert!(!heap.heap.definitely_invalid(ptr_2));
        assert!(!heap.heap.definitely_invalid(ptr_3));

        // Marking nothing means there is a reference cycle, which should
        // be collected.
        heap.sweep();

        assert!(heap.heap.definitely_invalid(ptr_1));
        assert!(heap.heap.definitely_invalid(ptr_2));
        assert!(heap.heap.definitely_invalid(ptr_3));
    }
}
