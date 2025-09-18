(module
  (import "env" "print" (func $print (param i32)))
  (global $j (mut i32) (i32.const 0))
  (global $i (mut i32) (i32.const 0))
  (func $main
    i32.const 0
    global.set $i
    (block $L.1
      (loop $L.0
        global.get $i
        i32.const 10
        i32.lt_s
        i32.eqz
        br_if $L.1
        global.get $i
        i32.const 1
        i32.add
        global.set $i
        i32.const 0
        global.set $j
        (block $L.3
          (loop $L.2
            global.get $j
            i32.const 10
            i32.lt_s
            i32.eqz
            br_if $L.3
            global.get $j
            i32.const 1
            i32.add
            global.set $j
            global.get $j
            call $print
            br $L.2
            ) ;; loop
          ) ;; block
        br $L.0
        ) ;; loop
      ) ;; block
    global.get $i
    call $print
    global.get $j
    call $print
  )

  (export "main" (func $main))
)
