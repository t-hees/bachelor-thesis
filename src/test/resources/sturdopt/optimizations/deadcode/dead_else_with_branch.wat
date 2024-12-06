(module
  (func (export "main") (result i32)
    i32.const 1
    (if (result i32)
      (then
        i32.const 3
        br 0
        unreachable
      )
      (else
        unreachable
      )
    )
  )
)
