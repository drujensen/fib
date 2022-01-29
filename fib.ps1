function fib {
    param($num)
    if ($num -le 1) {
        $num
        return
    }
    $a = fib($num - 1) + fib($num - 2)
    $a
}
fib(47);
