
import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class ElectromagneticMoat {

    static class Component {
        int port1;
        int port2;

        public Component(int port1, int port2) {
            this.port1 = port1;
            this.port2 = port2;
        }
    }

    public static void main(String[] args) {
        List<Component> components = readComponentsFromFile("input.txt");
        int maxStrength = findMaxBridgeStrength(components, 0, 0, new ArrayList<>());
        System.out.println(maxStrength);
    }

    private static List<Component> readComponentsFromFile(String filename) {
        List<Component> components = new ArrayList<>();
        try (Scanner scanner = new Scanner(new File(filename))) {
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                String[] parts = line.split("/");
                int port1 = Integer.parseInt(parts[0]);
                int port2 = Integer.parseInt(parts[1]);
                components.add(new Component(port1, port2));
            }
        } catch (FileNotFoundException e) {
            System.err.println("Error: " + e.getMessage());
        }
        return components;
    }

    private static int findMaxBridgeStrength(List<Component> availableComponents, int currentPort, int currentStrength, List<Component> usedComponents) {
        int maxStrength = currentStrength;
        for (int i = 0; i < availableComponents.size(); i++) {
            Component component = availableComponents.get(i);
            if (!usedComponents.contains(component)) {
                if (component.port1 == currentPort) {
                    List<Component> nextUsedComponents = new ArrayList<>(usedComponents);
                    nextUsedComponents.add(component);
                    int nextStrength = currentStrength + component.port1 + component.port2;
                    maxStrength = Math.max(maxStrength, findMaxBridgeStrength(availableComponents, component.port2, nextStrength, nextUsedComponents));
                } else if (component.port2 == currentPort) {
                    List<Component> nextUsedComponents = new ArrayList<>(usedComponents);
                    nextUsedComponents.add(component);
                    int nextStrength = currentStrength + component.port1 + component.port2;
                    maxStrength = Math.max(maxStrength, findMaxBridgeStrength(availableComponents, component.port1, nextStrength, nextUsedComponents));
                }
            }
        }
        return maxStrength;
    }
}
