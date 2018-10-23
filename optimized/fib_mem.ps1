$cache = New-Object System.Int64[] 47
function fib {
    param($num)
    if ($cache[$num] -eq 0) {
        $a = fib($num - 2)
        $b = fib($num - 1)
        $cache[$num] = $a+$b
    }
    $cache[$num]
}
$cache[0] = 1
$cache[1] = 1
fib(46)