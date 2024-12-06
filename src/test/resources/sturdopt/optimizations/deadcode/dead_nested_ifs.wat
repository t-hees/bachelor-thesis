(module
  (func (export "main") (result i32)
    i32.const 1
    (if (result i32)
      (then
        i32.const 1
        (if (result i32)
          (then
            i32.const 1
            (if (result i32)
              (then
                i32.const 1
                br 2
                i32.const 0
              )
              (else
                i32.const 1
              )
            )
          )
          (else
            i32.const 3
          )
        )
      )
      (else
        i32.const 3
      )
    )
  )
)
