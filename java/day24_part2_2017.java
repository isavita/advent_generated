import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;

public class Main {
    static class Component {
        int a, b;
        Component(int a, int b) {
            this.a = a;
            this.b = b;
        }
    }

    static int maxStrength = 0;
    static int maxLength = 0;

    static void findStrongestLongestBridge(Component[] components, boolean[] used, int port, int strength, int length) {
        if (length > maxLength || (length == maxLength && strength > maxStrength)) {
            maxStrength = strength;
            maxLength = length;
        }

        for (int i = 0; i < components.length; i++) {
            if (used[i]) {
                continue;
            }

            if (components[i].a == port || components[i].b == port) {
                used[i] = true;
                int nextPort = components[i].a;
                if (components[i].a == port) {
                    nextPort = components[i].b;
                }
                findStrongestLongestBridge(components, used, nextPort, strength + components[i].a + components[i].b, length + 1);
                used[i] = false;
            }
        }
    }

    public static void main(String[] args) throws IOException {
        BufferedReader br = new BufferedReader(new FileReader("input.txt"));
        String line;
        Component[] components = new Component[1000]; // assuming max 1000 components
        int i = 0;
        while ((line = br.readLine()) != null) {
            String[] ports = line.split("/");
            components[i++] = new Component(Integer.parseInt(ports[0]), Integer.parseInt(ports[1]));
        }
        br.close();

        boolean[] used = new boolean[i];
        findStrongestLongestBridge(Arrays.copyOf(components, i), used, 0, 0, 0);

        System.out.println(maxStrength);
    }
}