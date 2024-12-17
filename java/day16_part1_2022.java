
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Main {

    static class Valve {
        String id;
        int flow;
        Map<String, Integer> tunnels = new HashMap<>();
    }

    public static void main(String[] args) throws IOException {
        Map<String, Valve> valves = new HashMap<>();
        String input = Files.readString(Paths.get("input.txt")).trim();
        Pattern pattern = Pattern.compile("Valve (\\w+) has flow rate=(\\d+); tunnels? leads? to valves? (.*)");
        for (String line : input.split("\n")) {
            Matcher matcher = pattern.matcher(line);
            if (matcher.find()) {
                Valve v = new Valve();
                v.id = matcher.group(1);
                v.flow = Integer.parseInt(matcher.group(2));
                v.tunnels.put(v.id, 0);
                for (String t : matcher.group(3).split(", ")) {
                    v.tunnels.put(t, 1);
                }
                valves.put(v.id, v);
            }
        }

        for (String k : valves.keySet()) {
            for (String i : valves.keySet()) {
                for (String j : valves.keySet()) {
                    Integer dik = valves.get(i).tunnels.get(k);
                    Integer dkj = valves.get(k).tunnels.get(j);
                    if (dik != null && dkj != null) {
                        Integer dij = valves.get(i).tunnels.get(j);
                        if (dij == null || dij > dik + dkj) {
                            valves.get(i).tunnels.put(j, dik + dkj);
                        }
                    }
                }
            }
        }

        List<String> open = valves.values().stream()
                .filter(v -> v.flow > 0)
                .map(v -> v.id)
                .collect(Collectors.toList());

        System.out.println(maxPressure(valves, "AA", 30, 0, open, 0));
    }

    static int maxPressure(Map<String, Valve> valves, String curr, int minute, int pressure, List<String> open, int d) {
        int max = pressure;
        for (String next : open) {
            List<String> newOpen = new ArrayList<>(open);
            newOpen.remove(next);
            int timeLeft = minute - valves.get(curr).tunnels.get(next) - 1;
            if (timeLeft > 0) {
                max = Math.max(max, maxPressure(valves, next, timeLeft, timeLeft * valves.get(next).flow + pressure, newOpen, d + 1));
            }
        }
        return max;
    }
}
