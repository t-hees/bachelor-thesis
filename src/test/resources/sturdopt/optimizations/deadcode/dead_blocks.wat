(module
  (type $add_type (func (param i32) (param i32) (result i32)))
  (type (;1;) (func (param i32) (result i32))) ;; Type created by loop
  (type $result_type (func (result i32)))
  (func $add (type $add_type)
    local.get 0
    (loop $loop_without_branch (param i32) (result i32)
      local.get 1
      i32.add
    )
  )
  (func (export "main") (type $result_type) (result i32)
    (block $block_without_branch (result i32)
      i32.const 0
      i32.const 0
      br_if $block_without_branch
      drop
      i32.const 2
    )
    i32.const 3
    call $add
    return
    (block $unused_block (result i32)
      i32.const 1
    )
  )
)
