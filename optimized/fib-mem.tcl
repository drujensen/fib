set ::memoized [dict create]

proc memoize {} {
		
	if {[info level] > 2 && [info level -2] == "memoize"} {
		# Memoizing a new value. Let the calling proc run normally.
		return
	}

	# Get the proc and args of the calling proc.
	set cmd [info level -1]

	if {[dict exists $::memoized $cmd]} {

		# Get value from cache
		set val [dict get $::memoized $cmd]
	} else {

		# Run the command again and cache result.
		set val [{*}$cmd]
		dict set ::memoized $cmd $val
	}

	# Tell the calling proc to return cached value.
	return -level 2 $val
}

proc fib {n} {
    memoize
    if {$n <= 1} {
        return 1
    }
    return [expr {[fib [expr {$n - 1}]] + [fib [expr {$n - 2}]]}]
}

puts [fib 46]
