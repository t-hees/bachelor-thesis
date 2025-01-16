(module
  (memory $memory 1)
  (func (export "main") (param i32 i32 i32 i32)
    i32.const 9
    local.get 0
    (if (result i32)
      (then
        local.get 1
      )
      (else
        local.get 2
        local.get 3
        br_if 0
        i32.const 9
        i32.store
        local.get 2
      )
    )
    i32.add
    i32.const 4
    i32.sub
    drop
  )
)