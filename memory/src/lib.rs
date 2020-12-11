pub mod gc;
pub mod rc;

#[cfg(test)]
mod tests {
    #![allow(unused_unsafe)]

    /// Reuse test suite for each module to make sure they export the same interface.
    macro_rules! test_module_interface {
        ($m:ident) => {
            mod $m {
                use crate::$m;

                #[test]
                fn insert_object_through_storage() {
                    let mut storage = $m::Storage::new(1);
                    let obj = storage.insert(0).unwrap();
                    assert_eq!(*obj, 0);
                }

                #[test]
                fn can_check_if_reference_is_valid() {
                    let mut storage = $m::Storage::new(1);
                    let obj = storage.insert(0).unwrap();
                    assert!(storage.is_valid(&obj));
                }

                #[test]
                fn can_trigger_garbage_collection() {
                    let mut storage = $m::Storage::new(3);
                    let a = storage.insert("foo").unwrap();
                    let b = storage.insert(a.clone()).unwrap();
                    let c = storage.insert(b.clone()).unwrap();

                    assert!(storage.is_valid(&a));
                    assert!(storage.is_valid(&b));
                    assert!(storage.is_valid(&c));

                    unsafe {
                        storage.collect_garbage(&b);
                    }

                    assert!(storage.is_valid(&a));
                    assert!(storage.is_valid(&b));
                }
            }
        };
    }

    test_module_interface!(gc);
    test_module_interface!(rc);
}
