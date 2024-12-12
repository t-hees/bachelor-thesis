(module
  (type $add_type (func (param i32) (param i32) (result i32)))
  (type (;1;) (func (result i32))) ;; Type created by if
  (type (;2;) (func )) ;; Type created by empty if
  (func $add (type $add_type)
    i32.const 1
    (if (result i32)
      (then
        local.get 0
        local.get 1
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
    i32.const 0
    (if
      (then
      )
      (else
        nop
      )
    )
  )
  (func (export "main") (result i32)
    i32.const 2
    i32.const 3
    call $add)
)
