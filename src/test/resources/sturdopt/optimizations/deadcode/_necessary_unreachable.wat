(module
  (func $main (result i32)
    (block (result i32)
      i32.const 1
      i32.const 1
      i32.eq
      (if 
        (then
          i32.const 87
          br 1
          i32.const 4
          drop
        )
        (else
        )
      )
      i32.const 2
      i32.const 5
      i32.add
    )
  )
  (export "main" (func $main))
)
