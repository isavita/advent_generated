
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        int checksum = 0;
        
        try {
            File file = new File("input.txt");
            Scanner scanner = new Scanner(file);
            
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                String[] nums = line.split("\\s+");
                
                int min = Integer.MAX_VALUE;
                int max = Integer.MIN_VALUE;
                
                for (String num : nums) {
                    int curr = Integer.parseInt(num);
                    min = Math.min(min, curr);
                    max = Math.max(max, curr);
                }
                
                checksum += max - min;
            }
            
            System.out.println(checksum);
            
            scanner.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}
