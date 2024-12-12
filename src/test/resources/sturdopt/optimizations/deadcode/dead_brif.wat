(module
  (func (export "main") (param i32) (result i32)
    (block (result i32)
      (block (result i32)
        i32.const 1
        local.get 0
        br_if 1 ;; This br_if jump target is alive
        drop
        i32.const 2
        i32.const 0
        br_if 1 ;; This br_if jump target is dead but it references a block with an alive label.
        ;; In the sturdy cfg the only following node is still an EndLabel (of the inner block)!
      )
      drop
      i32.const 3
    )
  )
)
