
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String line = reader.readLine();
            String[] lengthsStr = line.split(",");
            int[] lengths = new int[lengthsStr.length];
            for (int i = 0; i < lengthsStr.length; i++) {
                lengths[i] = Integer.parseInt(lengthsStr[i]);
            }
            
            int[] list = new int[256];
            for (int i = 0; i < 256; i++) {
                list[i] = i;
            }
            int currentPosition = 0;
            int skipSize = 0;
            
            for (int length : lengths) {
                for (int i = 0; i < length/2; i++) {
                    int start = (currentPosition + i) % 256;
                    int end = (currentPosition + length - 1 - i) % 256;
                    int temp = list[start];
                    list[start] = list[end];
                    list[end] = temp;
                }
                currentPosition = (currentPosition + length + skipSize) % 256;
                skipSize++;
            }
            
            int result = list[0] * list[1];
            System.out.println(result);
            
            reader.close();
        } catch (IOException e) {
            System.out.println("File reading error " + e);
        }
    }
}
