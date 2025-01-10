(module
  (func (export "main") (result i32)
    i32.const 1
    i32.const 2
    i32.add

    i32.const 5
    i32.const 5
    i32.sub
    i32.eqz

    i32.lt_u
  )
)
