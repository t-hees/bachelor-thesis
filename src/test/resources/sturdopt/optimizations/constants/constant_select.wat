(module
  (func (export "main") (result i32)
    i32.const 5
    i32.const 6
    i32.const 0
    select

    i32.const 5
    i32.const 6
    i32.const 1
    select

    i32.add
  )
)
