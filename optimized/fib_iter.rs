fn main() {
    println!("{}", fib(47)); // Runs in ~800 us on my machine
}

fn fib(length: u64) -> u64 {
    let (mut current, mut last) = (0, 1);
    for _ in 0..length {
        std::mem::swap(&mut current, &mut last);
        current += last;
    }
    current
}
