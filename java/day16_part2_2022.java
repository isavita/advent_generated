
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Solution {
    static Map<String, Map<String, Integer>> valves = new HashMap<>();
    static int maxPressure = 0;

    static int maxPressure(String curr, int minute, int pressure, Set<String> opened, Set<String> toOpen) {
        maxPressure = Math.max(maxPressure, pressure);
        int maxP = pressure;

        for (String nextValve : toOpen) {
            int timeLeft = minute - valves.get(curr).get(nextValve) - 1;
            if (timeLeft > 0) {
                Set<String> newToOpen = new HashSet<>(toOpen);
                newToOpen.remove(nextValve);
                maxP = Math.max(maxP, maxPressure(nextValve, timeLeft, pressure + timeLeft * valves.get(nextValve).get("flow"), opened, newToOpen));
            }
        }
        return maxP;
    }

    static List<List<List<Integer>>> divide(int l) {
        List<List<List<Integer>>> res = new ArrayList<>();
        if (l == 1) {
            res.add(List.of(List.of(), List.of(0)));
            res.add(List.of(List.of(0), List.of()));
            return res;
        }
        List<List<List<Integer>>> d = divide(l - 1);
        for (List<List<Integer>> i : d) {
            List<Integer> first = new ArrayList<>(i.get(0));
            first.add(l - 1);
            List<Integer> second = new ArrayList<>(i.get(1));
            res.add(List.of(first, second));

            first = new ArrayList<>(i.get(0));
            second = new ArrayList<>(i.get(1));
            second.add(l - 1);
            res.add(List.of(first, second));
        }
        return res;
    }

    public static void main(String[] args) throws IOException {
        List<String> lines = Files.readAllLines(Paths.get("input.txt"));
        for (String line : lines) {
            String[] sp = line.split("; ");
            String id = sp[0].split(" ")[1];
            int flow = Integer.parseInt(sp[0].split("=")[1]);
            Map<String, Integer> tunnels = new HashMap<>();
            String[] tunnelStr = sp[1].replace("tunnel leads to valve ", "").replace("tunnels lead to valves ", "").trim().split(", ");
            for (String t : tunnelStr) tunnels.put(t, 1);
            tunnels.put(id, 0);
            Map<String, Integer> valveData = new HashMap<>();
            valveData.put("flow", flow);
            valves.getOrDefault(id, new HashMap<>()).putAll(tunnels);
            valves.put(id, valveData);
           valves.get(id).putAll(tunnels);

        }

      for(String k : valves.keySet()){
        for(String i : valves.keySet()){
            for(String j : valves.keySet()){
                if(valves.get(i).containsKey(k) && valves.get(k).containsKey(j)){
                    if(!valves.get(i).containsKey(j) || valves.get(i).get(j) > valves.get(i).get(k) + valves.get(k).get(j))
                        valves.get(i).put(j,valves.get(i).get(k) + valves.get(k).get(j) );
                }
            }
        }
      }
        List<String> openValves = new ArrayList<>();
        for (String key : valves.keySet()) {
            if (valves.get(key).get("flow") > 0) {
                openValves.add(key);
                valves.get(key).put("flow", valves.get(key).get("flow"));
            }
        }

        int maxTotalPressure = 0;
        for (List<List<Integer>> d : divide(openValves.size())) {
            if (d.get(0).isEmpty() || d.get(1).isEmpty()) continue;
            Set<String> mine = new HashSet<>();
            Set<String> elephant = new HashSet<>();
            for (int i : d.get(0)) mine.add(openValves.get(i));
            for (int i : d.get(1)) elephant.add(openValves.get(i));

             maxPressure=0;
             int pressure1  = maxPressure("AA", 26, 0, new HashSet<>(), mine);
             maxPressure =0;
             int pressure2 = maxPressure("AA", 26, 0, new HashSet<>(), elephant);


            maxTotalPressure = Math.max(maxTotalPressure, pressure1+pressure2);
        }
        System.out.println(maxTotalPressure);
    }
}
