(module
  (type $func_type (func (param i32 i32 i32) (result i32)))
  (type $main_type (func (param i32) (result i32)))
  (func $add_func (type $func_type)
    local.get 0
    local.get 1
    i32.add
    local.get 2
    i32.add
  )
  (func $set_func (type $func_type)
    local.get 0
    local.tee 1 ;; This parameters is tee'd so it can't be removed anymore
    local.get 1
    i32.add
    local.get 2
    i32.add
  )
  (func (export "main") (type $main_type)
    local.get 0
    i32.const 4
    i32.const 5
    call $add_func

    local.get 0
    i32.const 4
    i32.const 5
    call $set_func

    i32.add
  )
)
