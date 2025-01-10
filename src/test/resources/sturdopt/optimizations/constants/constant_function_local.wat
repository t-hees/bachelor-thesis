(module
  (type $func_type (func (param i32) (result i32)))
  (func $const_func (type $func_type)
    local.get 0
    local.get 0
    i32.sub
  )
  (func $return_3 (result i32)
    i32.const 3
  )
  (func (export "main") (result i32)
    call $return_3
    call $const_func
  )
)
