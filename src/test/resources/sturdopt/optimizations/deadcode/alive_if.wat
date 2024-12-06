(module
  (type $main_type (func (param i32) (result i32)))
  (func $main (type $main_type)
    local.get 0
    (if (result i32)
      (then
        i32.const 4
      )
      (else
        i32.const 5
      )
    )
  )
  (export "main" (func $main))
)
