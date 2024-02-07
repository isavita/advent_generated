
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
	public static void main(String[] args) {
		try {
			BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
			int steps = Integer.parseInt(reader.readLine().trim());
			reader.close();
			
			int currentPos = 0;
			int valueAfterZero = 0;
			
			for (int i = 1; i <= 50000000; i++) {
				currentPos = (currentPos + steps) % i;
				if (currentPos == 0) {
					valueAfterZero = i;
				}
				currentPos++;
			}
			
			System.out.println(valueAfterZero);
		} catch (IOException e) {
			System.out.println("File reading error: " + e.getMessage());
		}
	}
}
