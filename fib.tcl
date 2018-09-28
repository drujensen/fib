proc fib {n} {
    if {$n <= 1} {
        return 1
    }
    return [expr {[fib [expr {$n - 1}]] + [fib [expr {$n - 2}]]}]
}

puts [fib 46]
