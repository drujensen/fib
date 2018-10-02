#[macro_use] extern crate cached;
#[macro_use] extern crate lazy_static;  

fn main() {
    println!("{}", fib_with_memoize(46));
}

cached!{
    FIB;
    fn fib_with_memoize(n: u64) -> u64 = {
        if n == 0 || n == 1 { return n; }
        fib_with_memoize(n-1) + fib_with_memoize(n-2)
    }
}
