
import java.util.Scanner;

public class Solution {
    public static void main(String[] args) {
        Scanner scanner = new Scanner("input.txt");
        int b = 57 * 100 + 100000; // Initial value computed for register b
        int c = b + 17000; // Initial value computed for register c
        int h = 0; // Initialize register h

        for (int x = b; x <= c; x += 17) { // Simulate the loop from b to c with step 17
            if (!isPrime(x)) { // Check if x is a prime number
                h++;
            }
        }

        System.out.println(h);
    }

    public static boolean isPrime(int num) {
        if (num <= 1) return false;
        for (int i = 2; i * i <= num; i++) {
            if (num % i == 0) return false;
        }
        return true;
    }
}
