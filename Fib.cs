using System;
using System.Runtime.CompilerServices;
public class Fib
{
#if !OPTIMISED
#if TCO
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
    static uint fib(uint n)
    {
        if (n <= 1) return 1;
        return fib(n - 1) + fib(n - 2);
    }

    public static void Main(string[] args)
    {
        Console.WriteLine(fib(46));
    }
#else
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    unsafe static uint fib(uint n, uint* cache)
    {
        if (n <= 1) return 1;
        var a = &cache[n - 1];
        var b = &cache[n - 2];
        if (*a == 0)
        {
            *a = fib(n - 1, cache);

        }
        if (*b == 0)
        {
            *b = fib(n - 2, cache);
        }
        return *a + *b;
    }
    unsafe static void Main(string[] args)
    {
        uint[] array = new uint[46];
        fixed (uint* cache = array)
        {
            Console.WriteLine(fib(46, cache));
        }
    }
#endif
}
