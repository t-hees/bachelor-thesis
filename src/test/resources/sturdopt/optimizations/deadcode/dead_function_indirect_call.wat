(module
  (type $add_type (func (param i32) (param i32) (result i32)))
  (table 4 funcref)
  (elem (i32.const 1) 1 2 3) ;; const_1, dead_func, add
  (func (export "main") (result i32)
    call 1
    i32.const 3
    (call_indirect (type $add_type) (i32.const 3)) ;; call add
  )
  (func (result i32) ;; const_1
    i32.const 1
  )
  (func (result i32) ;; dead_func
    i32.const 9
  )
  (func (type $add_type) ;; add
    local.get 0
    local.get 1
    i32.add)
)
