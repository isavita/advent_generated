import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CrossedWiresSolver {
    public static void main(String[] args) {
        CrossedWiresSolver solver = new CrossedWiresSolver();
        try {
            List<Gate> gates = solver.readInput("input.txt");
            
            // Find the swapped wires
            long startTime = System.currentTimeMillis();
            List<String> swappedWires = solver.findSwappedWires(gates);
            long endTime = System.currentTimeMillis();
            
            // Sort and join with commas
            Collections.sort(swappedWires);
            String result = String.join(",", swappedWires);
            System.out.println(result);
            System.out.println("Time taken: " + (endTime - startTime) + "ms");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    
    public List<Gate> readInput(String filename) throws IOException {
        List<Gate> gates = new ArrayList<>();
        
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            boolean parsingGates = false;
            
            while ((line = reader.readLine()) != null) {
                line = line.trim();
                
                if (line.isEmpty()) {
                    parsingGates = true;
                    continue;
                }
                
                if (parsingGates) {
                    // Parse gate definition
                    Pattern gatePattern = Pattern.compile("([a-z0-9]+) (AND|OR|XOR) ([a-z0-9]+) -> ([a-z0-9]+)");
                    Matcher matcher = gatePattern.matcher(line);
                    
                    if (matcher.matches()) {
                        String input1 = matcher.group(1);
                        String operation = matcher.group(2);
                        String input2 = matcher.group(3);
                        String output = matcher.group(4);
                        
                        Gate gate = new Gate(input1, operation, input2, output);
                        gates.add(gate);
                    }
                }
            }
        }
        
        return gates;
    }
    
    private String getReverseLookupKey(String a, String op, String b) {
        // Sort inputs to handle commutative operations
        if (a.compareTo(b) > 0) {
            String temp = a;
            a = b;
            b = temp;
        }
        
        return a + "_" + op + "_" + b;
    }
    
    private Map<String, Gate> createLookup(List<Gate> gates) {
        Map<String, Gate> lookup = new HashMap<>();
        
        for (Gate gate : gates) {
            lookup.put(gate.output, gate);
        }
        
        return lookup;
    }
    
    private Map<String, String> createReverseLookup(List<Gate> gates) {
        Map<String, String> reverseLookup = new HashMap<>();
        
        for (Gate gate : gates) {
            String a = gate.input1;
            String b = gate.input2;
            String op = gate.operation;
            
            String key = getReverseLookupKey(a, op, b);
            reverseLookup.put(key, gate.output);
        }
        
        return reverseLookup;
    }
    
    private void swap(List<String[]> pairs, List<Gate> gates, String a, String b) {
        // Add to pairs
        pairs.add(new String[]{a, b});
        
        // Swap outputs in gates
        for (Gate gate : gates) {
            if (gate.output.equals(a)) {
                gate.output = b;
            } else if (gate.output.equals(b)) {
                gate.output = a;
            }
        }
    }
    
    public List<String> findSwappedWires(List<Gate> gates) {
        List<String[]> pairs = new ArrayList<>();
        
        // Count number of z wires to determine bit width
        int numZ = 0;
        for (Gate gate : gates) {
            if (gate.output.startsWith("z")) {
                int position = Integer.parseInt(gate.output.substring(1));
                numZ = Math.max(numZ, position + 1);
            }
        }
        
        // Keep finding swaps until we have 4
        while (pairs.size() < 4) {
            // Create lookups for current state of gates
            Map<String, Gate> lookup = createLookup(gates);
            Map<String, String> reverseLookup = createReverseLookup(gates);
            
            String adder = null;
            String carry = null;
            
            // Try to reconstruct adder structure for each bit position
            boolean foundSwap = false;
            
            for (int i = 0; i < numZ; i++) {
                String xi = String.format("x%02d", i);
                String yi = String.format("y%02d", i);
                String zi = String.format("z%02d", i);
                
                if (i == 0) {
                    // First bit: z0 = x0 XOR y0, carry = x0 AND y0
                    String bitKey = getReverseLookupKey(xi, "XOR", yi);
                    adder = reverseLookup.get(bitKey);
                    
                    String carryKey = getReverseLookupKey(xi, "AND", yi);
                    carry = reverseLookup.get(carryKey);
                } else {
                    // Higher bits use previous carry
                    String bitKey = getReverseLookupKey(xi, "XOR", yi);
                    String bit = reverseLookup.get(bitKey);
                    
                    if (bit != null) {
                        String adderKey = getReverseLookupKey(bit, "XOR", carry);
                        adder = reverseLookup.get(adderKey);
                        
                        if (adder != null) {
                            // Calculate next carry
                            String c1Key = getReverseLookupKey(xi, "AND", yi);
                            String c1 = reverseLookup.get(c1Key);
                            
                            String c2Key = getReverseLookupKey(bit, "AND", carry);
                            String c2 = reverseLookup.get(c2Key);
                            
                            String carryKey = getReverseLookupKey(c1, "OR", c2);
                            carry = reverseLookup.get(carryKey);
                        }
                    }
                }
                
                // If no adder found, look for potential swap
                if (adder == null) {
                    Gate gate = lookup.get(zi);
                    if (gate != null) {
                        String bitKey = getReverseLookupKey(xi, "XOR", yi);
                        String bit = reverseLookup.get(bitKey);
                        
                        if (bit != null) {
                            // Check if gate.input1 should be the bit
                            String gateAKey = getReverseLookupKey(gate.input1, "XOR", carry);
                            if (reverseLookup.containsKey(gateAKey)) {
                                swap(pairs, gates, bit, gate.input1);
                                foundSwap = true;
                                break;
                            }
                            
                            // Check if gate.input2 should be the bit
                            String gateBKey = getReverseLookupKey(gate.input2, "XOR", carry);
                            if (reverseLookup.containsKey(gateBKey)) {
                                swap(pairs, gates, bit, gate.input2);
                                foundSwap = true;
                                break;
                            }
                        }
                    }
                } else if (!adder.equals(zi)) {
                    // Found adder but it's not connected to correct z output
                    swap(pairs, gates, adder, zi);
                    foundSwap = true;
                    break;
                }
            }
            
            // If we didn't find a swap, try to find any missing connections
            if (!foundSwap && pairs.size() < 4) {
                // Look for gates that should be connected based on adder structure
                for (int i = 0; i < numZ - 1; i++) {
                    String zi = String.format("z%02d", i);
                    String zNext = String.format("z%02d", i + 1);
                    
                    Gate ziGate = lookup.get(zi);
                    Gate zNextGate = lookup.get(zNext);
                    
                    if (ziGate != null && zNextGate != null) {
                        // In an adder, there should be some connections between consecutive bits
                        // If we find gates that should be connected but aren't, swap them
                        for (Gate g1 : gates) {
                            for (Gate g2 : gates) {
                                if (isAdderConnection(g1, g2) && !isConnected(gates, g1, g2)) {
                                    swap(pairs, gates, g1.output, g2.output);
                                    foundSwap = true;
                                    break;
                                }
                            }
                            if (foundSwap) break;
                        }
                    }
                    if (foundSwap) break;
                }
            }
            
            // If still no swap found and we need more, use a heuristic to find likely swaps
            if (!foundSwap && pairs.size() < 4) {
                // Look for gates with unusual connections that don't match adder structure
                for (int i = 0; i < numZ; i++) {
                    String xi = String.format("x%02d", i);
                    String yi = String.format("y%02d", i);
                    String zi = String.format("z%02d", i);
                    
                    // For example, an XOR gate should connect to a bit position
                    // If we find an XOR gate with unusual inputs, it might be swapped
                    for (Gate gate : gates) {
                        if (gate.operation.equals("XOR") && !gate.output.equals(zi)) {
                            if ((gate.input1.equals(xi) && gate.input2.equals(yi)) ||
                                (gate.input1.equals(yi) && gate.input2.equals(xi))) {
                                swap(pairs, gates, gate.output, zi);
                                foundSwap = true;
                                break;
                            }
                        }
                    }
                    if (foundSwap) break;
                }
            }
            
            // If we really can't find any more swaps, break out
            if (!foundSwap) {
                break;
            }
        }
        
        // Flatten pairs to list of wires
        List<String> result = new ArrayList<>();
        for (String[] pair : pairs) {
            result.add(pair[0]);
            result.add(pair[1]);
        }
        
        return result;
    }
    
    private boolean isAdderConnection(Gate g1, Gate g2) {
        // Check if gates should be connected in an adder structure
        // For example, an XOR gate and an AND gate with same inputs
        if (g1.operation.equals("XOR") && g2.operation.equals("AND")) {
            return (g1.input1.equals(g2.input1) && g1.input2.equals(g2.input2)) ||
                   (g1.input1.equals(g2.input2) && g1.input2.equals(g2.input1));
        }
        return false;
    }
    
    private boolean isConnected(List<Gate> gates, Gate g1, Gate g2) {
        // Check if two gates are connected
        return gates.stream().anyMatch(g -> 
            (g.input1.equals(g1.output) && g.input2.equals(g2.output)) ||
            (g.input1.equals(g2.output) && g.input2.equals(g1.output))
        );
    }
    
    private static class Gate {
        String input1;
        String operation;
        String input2;
        String output;
        
        public Gate(String input1, String operation, String input2, String output) {
            this.input1 = input1;
            this.operation = operation;
            this.input2 = input2;
            this.output = output;
        }
    }
}
