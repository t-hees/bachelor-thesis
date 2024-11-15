(module
  (type $add_type (func (param i32) (param i32) (result i32)))
  (table 2 funcref)
  (elem (i32.const 0) $unused_add $add)
  (func $unused_add (type $add_type)
    local.get 0
    local.get 1
    i32.add)
  (func $add (type $add_type)
    local.get 0
    local.get 1
    i32.add)
  (func (export "main") (result i32)
    i32.const 2
    i32.const 3
    (call_indirect (type $add_type) (i32.const 1))
  )
)
