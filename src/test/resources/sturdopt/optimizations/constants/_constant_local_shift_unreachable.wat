(module
  (type $func_type (func (param i32) (result i32)))
  (memory $memory 1)
  (func $func (type $func_type) (local $loc1 i32)
    local.get 0       ;; i32.const 0
    if (result i32)
      local.get 0
    else
      i32.const 0
    end
    i32.load
    local.set 1      ;; needs to be shifted after opt
    local.get 1
  )
  (func (export "main") (type $func_type)
    i32.const 0
    local.get 0
    i32.store

    i32.const 0
    call $func
  )
)
