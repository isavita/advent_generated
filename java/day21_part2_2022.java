
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class MonkeyMath {

    public static void main(String[] args) {
        try {
            Map<String, String> monkeyJobs = readInput("input.txt");

            // Part 1
            long rootNumber = calculateMonkeyValue(monkeyJobs, "root");
            System.out.println("Part 1 - Root yells: " + rootNumber);

            // Part 2
            long humnValue = solveForHumn(monkeyJobs);
            System.out.println("Part 2 - You (humn) yell: " + humnValue);

        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static Map<String, String> readInput(String filename) throws IOException {
        Map<String, String> monkeyJobs = new HashMap<>();
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            while ((line = reader.readLine()) != null) {
                String[] parts = line.split(": ");
                monkeyJobs.put(parts[0], parts[1]);
            }
        }
        return monkeyJobs;
    }

    private static long calculateMonkeyValue(Map<String, String> monkeyJobs, String monkeyName) {
        String job = monkeyJobs.get(monkeyName);
        if (job.matches("\\d+")) {
            return Long.parseLong(job);
        }

        String[] parts = job.split(" ");
        long val1 = calculateMonkeyValue(monkeyJobs, parts[0]);
        long val2 = calculateMonkeyValue(monkeyJobs, parts[2]);

        switch (parts[1]) {
            case "+":
                return val1 + val2;
            case "-":
                return val1 - val2;
            case "*":
                return val1 * val2;
            case "/":
                return val1 / val2;
            default:
                throw new IllegalArgumentException("Invalid operation: " + parts[1]);
        }
    }


    private static long solveForHumn(Map<String, String> monkeyJobs) {
        String rootJob = monkeyJobs.get("root");
        String[] rootParts = rootJob.split(" ");
        String monkey1 = rootParts[0];
        String monkey2 = rootParts[2];

        monkeyJobs.put("humn", "x"); // Use a symbolic representation for humn

        // Determine which side contains 'humn'.  We need to calculate the *other* side's value.
        long targetValue;
        String equationToSolve;

        if (containsHumn(monkeyJobs, monkey1)) {
            targetValue = calculateMonkeyValue(createCopyWithoutHumn(monkeyJobs), monkey2);
             equationToSolve = monkey1;
           
        } else  {
            targetValue = calculateMonkeyValue(createCopyWithoutHumn(monkeyJobs), monkey1);
            equationToSolve = monkey2;

        }
        return solveEquation(monkeyJobs, equationToSolve, targetValue);

    }


    private static boolean containsHumn(Map<String,String> monkeyJobs, String monkeyName){
        if(monkeyName.equals("humn")){
            return true;
        }
         String job = monkeyJobs.get(monkeyName);
          if (job.matches("\\d+") || job.equals("x")) { //base cases no humn
                return false;
           }
            String[] parts = job.split(" ");

            return containsHumn(monkeyJobs, parts[0]) || containsHumn(monkeyJobs, parts[2]);
    }


  private static Map<String, String> createCopyWithoutHumn(Map<String,String> original){
      Map<String,String> copy = new HashMap<>();
       for (Map.Entry<String, String> entry : original.entrySet()) {
           if(!entry.getKey().equals("humn")){
               copy.put(entry.getKey(),entry.getValue());
           } else{
               copy.put(entry.getKey(),original.get(entry.getKey()).replaceAll("[^0-9]",""));
           }
       }
      return copy;
  }



    private static long solveEquation(Map<String, String> monkeyJobs, String monkeyName, long targetValue) {

        if (monkeyName.equals("humn")) {
            return targetValue;
        }

        String job = monkeyJobs.get(monkeyName);
        String[] parts = job.split(" ");
        String monkeyA = parts[0];
        String monkeyB = parts[2];
        String operation = parts[1];

        if (containsHumn(monkeyJobs, monkeyA)) {
            long valueB = calculateMonkeyValue(createCopyWithoutHumn(monkeyJobs), monkeyB);
            switch (operation) {
                case "+":
                    return solveEquation(monkeyJobs, monkeyA, targetValue - valueB);
                case "-":
                    return solveEquation(monkeyJobs, monkeyA, targetValue + valueB);
                case "*":
                    return solveEquation(monkeyJobs, monkeyA, targetValue / valueB);
                case "/":
                    return solveEquation(monkeyJobs, monkeyA, targetValue * valueB);
            }

        } else {
            long valueA = calculateMonkeyValue(createCopyWithoutHumn(monkeyJobs), monkeyA);
              switch (operation) {
                case "+":
                    return solveEquation(monkeyJobs, monkeyB, targetValue - valueA);
                case "-":
                    return solveEquation(monkeyJobs, monkeyB, valueA - targetValue);
                case "*":
                    return solveEquation(monkeyJobs, monkeyB, targetValue / valueA);
                case "/":
                    return solveEquation(monkeyJobs, monkeyB, valueA / targetValue);
            }
        }
          throw new IllegalArgumentException("should not reach here");
    }
}
