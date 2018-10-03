import java.math.*;
import java.time.*;

/**
 * This class tries to compute a fibonacci number without using recursion
 */
public class FibOptimized{
    private static BigInteger fibonacci(long i){
        // It is initialized to the first fibonacci number in the series
        BigInteger fibOne = new BigInteger("1");
        // It is initialized to the second fibonacci number in the series
        BigInteger fibTwo = new BigInteger("1");
        // This is a counter used as a cursor over the fibonacci series
        long count = 0;
        // We will loop over all numbers until we reach "i"
        for(int s = 0; s <= i; s++){
            // For the first two fibonacci numbers, we can return with a no ops since fibOne and fibTwo are already set with the correct answer
            if(s == 0 || s ==1)
                continue;
            // If the current number is even
            else if(count % 2 == 0){
                fibTwo = fibTwo.add(fibOne);
            }
            // if the current number is odd
            else{
                fibOne = fibOne.add(fibTwo);
            }
            // Move the cursor to the next number
            count+= 1;
        }
        return (fibOne.compareTo(fibTwo) > 0 ? fibOne : fibTwo);
    }
    public static void main(String[] args){
        System.out.println(fibonacci(46));
    }
}