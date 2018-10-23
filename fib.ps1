function fib {
    param($num)
    if ($num -le 1) {
        1
        return
    }
    $a = fib($num - 1)
    $a += fib($num - 2)
    Write-Host $a
    $a
}
fib(46);