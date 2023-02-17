import java.math.*;

/**
 * This class tries to compute a fibonacci number without using recursion
 */
public class FibOptimized{
    public static BigInteger fib(long n) {
        BigInteger a = BigInteger.ZERO;
        BigInteger b =  BigInteger.ONE;
        for (int i = 0; i < n; i++) {
            BigInteger temp = a;
            a = b;
            b = b.add(temp);
        }
        return a;
    }
    public static void main(String[] args){
        System.out.println(fibonacci(47));
    }
}
