
import java.io.File;
import java.io.IOException;
import java.util.*;

public class TicketTranslation {

    public static void main(String[] args) throws IOException {
        Scanner scanner = new Scanner(new File("input.txt"));
        List<Rule> rules = new ArrayList<>();
        List<Integer> myTicket = new ArrayList<>();
        List<List<Integer>> nearbyTickets = new ArrayList<>();

        // Parse rules
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            if (line.isEmpty()) break;
            rules.add(parseRule(line));
        }

        // Skip "your ticket:" line
        scanner.nextLine();

        // Parse my ticket
        String myTicketLine = scanner.nextLine();
        for (String numStr : myTicketLine.split(",")) {
            myTicket.add(Integer.parseInt(numStr));
        }

        // Skip empty line and "nearby tickets:" line
        scanner.nextLine();
        scanner.nextLine();

        // Parse nearby tickets
        while (scanner.hasNextLine()) {
            String line = scanner.nextLine();
            List<Integer> ticket = new ArrayList<>();
            for (String numStr : line.split(",")) {
                ticket.add(Integer.parseInt(numStr));
            }
            nearbyTickets.add(ticket);
        }

        // Part 1: Calculate ticket scanning error rate
        int errorRate = 0;
        List<List<Integer>> validTickets = new ArrayList<>();
        for (List<Integer> ticket : nearbyTickets) {
            boolean validTicket = true;
            for (int num : ticket) {
                if (!isValid(num, rules)) {
                    errorRate += num;
                    validTicket = false;
                }
            }
            if (validTicket) {
                validTickets.add(ticket);
            }
        }
        System.out.println("Part 1 - Ticket scanning error rate: " + errorRate);

        // Part 2: Determine field order and calculate product of departure fields
        validTickets.add(myTicket);
        Map<String, Integer> fieldOrder = determineFieldOrder(rules, validTickets);
        long departureProduct = 1;
        for (Map.Entry<String, Integer> entry : fieldOrder.entrySet()) {
            if (entry.getKey().startsWith("departure")) {
                departureProduct *= myTicket.get(entry.getValue());
            }
        }
        System.out.println("Part 2 - Product of departure fields: " + departureProduct);
    }

    static Rule parseRule(String line) {
        String[] parts = line.split(": ");
        String name = parts[0];
        String[] ranges = parts[1].split(" or ");
        int[][] rangeValues = new int[2][2];
        for (int i = 0; i < 2; i++) {
            String[] range = ranges[i].split("-");
            rangeValues[i][0] = Integer.parseInt(range[0]);
            rangeValues[i][1] = Integer.parseInt(range[1]);
        }
        return new Rule(name, rangeValues);
    }

    static boolean isValid(int num, List<Rule> rules) {
        for (Rule rule : rules) {
            if ((num >= rule.ranges[0][0] && num <= rule.ranges[0][1]) ||
                (num >= rule.ranges[1][0] && num <= rule.ranges[1][1])) {
                return true;
            }
        }
        return false;
    }

    static Map<String, Integer> determineFieldOrder(List<Rule> rules, List<List<Integer>> tickets) {
        int numFields = tickets.get(0).size();
        Map<String, Set<Integer>> possiblePositions = new HashMap<>();
        for (Rule rule : rules) {
            possiblePositions.put(rule.name, new HashSet<>());
            for (int i = 0; i < numFields; i++) {
                boolean validForPosition = true;
                for (List<Integer> ticket : tickets) {
                    int num = ticket.get(i);
                    if (!((num >= rule.ranges[0][0] && num <= rule.ranges[0][1]) ||
                          (num >= rule.ranges[1][0] && num <= rule.ranges[1][1]))) {
                        validForPosition = false;
                        break;
                    }
                }
                if (validForPosition) {
                    possiblePositions.get(rule.name).add(i);
                }
            }
        }

        Map<String, Integer> fieldOrder = new HashMap<>();
        while (fieldOrder.size() < rules.size()) {
            for (Map.Entry<String, Set<Integer>> entry : possiblePositions.entrySet()) {
                if (entry.getValue().size() == 1) {
                    int position = entry.getValue().iterator().next();
                    fieldOrder.put(entry.getKey(), position);
                    for (Set<Integer> positions : possiblePositions.values()) {
                        positions.remove(position);
                    }
                }
            }
        }
        return fieldOrder;
    }

    static class Rule {
        String name;
        int[][] ranges;

        Rule(String name, int[][] ranges) {
            this.name = name;
            this.ranges = ranges;
        }
    }
}
