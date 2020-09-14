use std::collections::HashSet;
use std::rc::Rc;

pub type InternedString = Rc<str>;

pub struct StringInterner {
    table: HashSet<Rc<str>>,
}

impl StringInterner {
    pub fn new() -> Self {
        StringInterner {
            table: HashSet::new(),
        }
    }

    // Imitating the HashMap signature was difficult; even though Rc<String>: Borrow<String>
    // and String: Borrow<str>, Rc<String> doesn't implement Borrow<str>.
    // But apparently, Rc<str> exists and works?? Looks like it was added for exactly this
    // purpose: https://github.com/rust-lang/rfcs/blob/master/text/1845-shared-from-slice.md
    pub fn get_interned<K>(&mut self, key: K) -> Rc<str>
    where
        K: AsRef<str>,
    {
        let string_slice = key.as_ref();
        match self.table.get(string_slice) {
            Some(rc) => rc.clone(),
            None => {
                let rc = Rc::<str>::from(string_slice);
                self.table.insert(rc.clone());
                rc
            }
        }
    }

    pub fn insert<K>(&mut self, key: K)
    where
        K: AsRef<str>,
    {
        self.get_interned(key);
    }

    pub fn clean(&mut self) {
        self.table.retain(|rc| Rc::strong_count(rc) > 1);
    }

    pub fn len(&self) -> usize {
        self.table.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_interning() {
        let mut table = StringInterner::new();

        let string = String::from("a");
        let str_slice = "a";

        let x = table.get_interned(string);
        let y = table.get_interned(str_slice);
        assert_eq!(x, y);

        table.insert("b");
        assert_eq!(table.len(), 2);

        table.clean();
        assert_eq!(table.len(), 1);

        std::mem::drop(x);
        table.clean();
        assert_eq!(table.len(), 1);

        std::mem::drop(y);
        table.clean();
        assert_eq!(table.len(), 0);
    }
}
