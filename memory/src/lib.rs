pub mod gc;
pub mod rc;

#[cfg(test)]
mod tests {
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
            }
        };
    }

    test_module_interface!(gc);
    test_module_interface!(rc);
}
