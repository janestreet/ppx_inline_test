(module
   (import "env" "Base_am_testing_flag"
      (global $Base_am_testing_flag (mut i32)))

   (func $set_am_testing
      (global.set $Base_am_testing_flag (i32.const 1)))

   (start $set_am_testing)
)
