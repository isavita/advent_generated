
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class Solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            String directions = reader.readLine();
            reader.close();
            
            Map<String, Boolean> visitedHouses = new HashMap<>();
            int x = 0, y = 0; // Santa's starting position
            String position = x + "," + y;
            visitedHouses.put(position, true);
            
            for (int i = 0; i < directions.length(); i++) {
                char dir = directions.charAt(i);
                switch (dir) {
                    case '^':
                        y++; // Move north
                        break;
                    case 'v':
                        y--; // Move south
                        break;
                    case '>':
                        x++; // Move east
                        break;
                    case '<':
                        x--; // Move west
                        break;
                }
                
                position = x + "," + y;
                visitedHouses.put(position, true);
            }
            
            System.out.println(visitedHouses.size());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
