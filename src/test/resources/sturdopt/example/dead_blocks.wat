(module
  (func $add (param $lhs i32) (param $rhs i32) (result i32)
    local.get $lhs
    (loop $loop_without_branch (param i32) (result i32)
      local.get $rhs
      i32.add
    )
  )
  (func (export "main") (result i32)
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
