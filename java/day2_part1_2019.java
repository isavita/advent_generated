
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class solution {

    public static void main(String[] args) {
        int[] inputData = readInputFromFile("input.txt");

        inputData[1] = 12;
        inputData[2] = 2;

        int result = executeProgram(inputData);

        System.out.println(result);
    }

    private static int[] readInputFromFile(String fileName) {
        try {
            BufferedReader reader = new BufferedReader(new FileReader(fileName));
            String line = reader.readLine();
            String[] values = line.split(",");
            int[] inputData = new int[values.length];
            for (int i = 0; i < values.length; i++) {
                inputData[i] = Integer.parseInt(values[i]);
            }
            reader.close();
            return inputData;
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    private static int executeProgram(int[] data) {
        for (int i = 0; i < data.length - 3; i += 4) {
            int pos1 = data[i + 1];
            int pos2 = data[i + 2];
            int pos3 = data[i + 3];
            switch (data[i]) {
                case 1:
                    int sum = data[pos1] + data[pos2];
                    data[pos3] = sum;
                    break;
                case 2:
                    int product = data[pos1] * data[pos2];
                    data[pos3] = product;
                    break;
                case 99:
                    return data[0];
                default:
                    throw new IllegalArgumentException("Invalid opcode");
            }
        }
        return data[0];
    }
}
