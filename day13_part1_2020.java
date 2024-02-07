
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {
    public static void main(String[] args) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader("input.txt"));
            int earliestDeparture = Integer.parseInt(reader.readLine());
            String[] busIDs = reader.readLine().split(",");

            int earliestBusID = 0;
            int minWaitTime = earliestDeparture;

            for (String id : busIDs) {
                if (id.equals("x")) {
                    continue;
                }
                int busID = Integer.parseInt(id);
                int waitTime = busID - (earliestDeparture % busID);
                if (waitTime < minWaitTime) {
                    minWaitTime = waitTime;
                    earliestBusID = busID;
                }
            }

            System.out.println(earliestBusID * minWaitTime);
            
            reader.close();
        } catch (IOException e) {
            System.err.println("Error reading file: " + e.getMessage());
        }
    }
}
