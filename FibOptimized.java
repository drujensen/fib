import java.math.*;
import java.time.*;

public class FibOptimized{
    public static BigInteger one = new BigInteger("1");
    public static BigInteger two = new BigInteger("1");
    static long count =0;

    static void fibonnaci(long i){
        for(int s = 0; s < i; s++){
            if(s == 0 || s ==1)
                continue;
            else if(count%2 == 0){
                count+= 1;
                two = two.add(one);}
            else{
                count+=1;
                one = one.add(two);}
        }
    }
    public static void main(String[] args){
        if(args.length != 1){
            System.err.println("Error! expected one numebr argument");
            System.exit(1);
        }
        Instant start = Instant.now();
        System.out.println("Processing...");
        fibonnaci(Long.parseLong(args[0]));
        long duration = Duration.between(start, Instant.now()).toMillis();
        System.out.println("Result: " + (one.compareTo(two) > 0 ? one : two));
        System.out.println("Done in " +  duration + "ms");
    }
}