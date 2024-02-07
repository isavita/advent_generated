import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {

    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line;
            int checksum = 0;
            int sum = 0;

            while ((line = reader.readLine()) != null) {
                String[] numbers = line.split("\\s+");
                
                int min = Integer.MAX_VALUE;
                int max = Integer.MIN_VALUE;
                for (String numStr : numbers) {
                    int num = Integer.parseInt(numStr);
                    min = Math.min(min, num);
                    max = Math.max(max, num);
                }
                
                checksum += max - min;
                
                for (int i = 0; i < numbers.length; i++) {
                    for (int j = i + 1; j < numbers.length; j++) {
                        int num1 = Integer.parseInt(numbers[i]);
                        int num2 = Integer.parseInt(numbers[j]);
                        if (num1 % num2 == 0) {
                            sum += num1 / num2;
                        } else if (num2 % num1 == 0) {
                            sum += num2 / num1;
                        }
                    }
                }
            }

            reader.close();
            
            System.out.println("/usr/bin/javac script.java " + checksum);
            System.out.println("/usr/bin/javac script.java " + sum);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}