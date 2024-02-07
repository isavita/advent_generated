import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Triangle {

    public static void main(String[] args) {
        List<String> lines = new ArrayList<>();
        try {
            File myObj = new File("input.txt");
            Scanner myReader = new Scanner(myObj);
            while (myReader.hasNextLine()) {
                String data = myReader.nextLine();
                lines.add(data);
            }
            myReader.close();
        } catch (FileNotFoundException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }

        int possibleTriangles = 0;

        for (int i = 0; i < lines.size(); i += 3) {
            String[] row1 = lines.get(i).trim().split("\\s+");
            String[] row2 = lines.get(i + 1).trim().split("\\s+");
            String[] row3 = lines.get(i + 2).trim().split("\\s+");

            for (int j = 0; j < 3; j++) {
                int a = Integer.parseInt(row1[j]);
                int b = Integer.parseInt(row2[j]);
                int c = Integer.parseInt(row3[j]);

                if (a + b > c && a + c > b && b + c > a) {
                    possibleTriangles++;
                }
            }
        }

        System.out.println(possibleTriangles);
    }
}