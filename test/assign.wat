(module
  (import "env" "print" (func $print (param i32)))
  (global $j (mut i32) (i32.const 0))
  (global $i (mut i32) (i32.const 0))
  (func $main
    i32.const 1
    global.set $i
    i32.const 2
    global.set $j
    global.get $i
    global.get $j
    i32.add
    call $print
  )

  (export "main" (func $main))
)
