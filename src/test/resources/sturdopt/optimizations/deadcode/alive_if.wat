(module
  (type $main_type (func (param i32) (result i32)))
  (func $main (type $main_type)
    local.get 0
    (if
      (then
      )
      (else
        i32.const 5
        drop
      )
    )
    local.get 0
    (if
      (then
        i32.const 4
        drop
      )
      (else
      )
    )
    local.get 0
    (if (result i32)
      (then
        i32.const 4
      )
      (else
        i32.const 5
      )
    )
    local.get 0
    (if
      (then
      )
      (else
      )
    )
  )
  (export "main" (func $main))
)
