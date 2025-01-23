(module
  (type $func_type (func (param i32 i32) (result i32)))
  (table 2 funcref)
  (elem (i32.const 0) $const_func $return_3)
  (func $const_func (type $func_type) ;; This type must remain $func_type after optimization since it is in a table
    local.get 0
    local.get 1
    i32.add
  )
  (func $return_3 (result i32)
    i32.const 3
  )
  (func (export "main") (result i32)
    (call_indirect (result i32) (i32.const 1))
    i32.const 9
    (call_indirect (type $func_type) (i32.const 0))
  )
)
