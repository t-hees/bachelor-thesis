(module
  (func (export "main") (param i32 i32)
    i32.const 2
    local.get 0
    local.get 1
    i32.add
    i32.const 3
    i32.sub
    i32.eqz
    drop
    drop
  )
)