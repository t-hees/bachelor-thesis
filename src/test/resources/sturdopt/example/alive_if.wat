(module
  (func (export "main") (param $a i32) (result i32)
    local.get $a
    (if (result i32)
      (then
        i32.const 4
      )
      (else
        i32.const 5
      )
    )
  )
)
