(module
  (func $add (param $lhs i32) (param $rhs i32) (result i32)
    i32.const 1
    (if (result i32)
      (then
        local.get $lhs
        local.get $rhs
        i32.add
      )
      (else
        i32.const 0
        (if (result i32)
          (then
            i32.const 1
          )
          (else
            i32.const 0
          )
        )
      )
    )
  )
  (func (export "main") (result i32)
    i32.const 2
    i32.const 3
    call $add)
)
