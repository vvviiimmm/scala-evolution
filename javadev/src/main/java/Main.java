import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.math.BigInteger;

public class Main {
    public static void main(String[] args) {
        System.out.print("Please enter a number: ");

        InputStreamReader read = new InputStreamReader(System.in);
        BufferedReader in = new BufferedReader(read);
        int number;

        try {
          number = Integer.parseInt(in.readLine());
          System.out.printf("Factorial of %d is %d", number, factorial(number));
        } catch (Exception e) {
            System.out.print("Invalid number");
        }
    }

    public static BigInteger factorial(int n) {
        BigInteger result = BigInteger.ONE;
        for (int i = 1; i <= n; i++) {
            result = result.multiply(BigInteger.valueOf(i));
        }
        return result;
    }
}
