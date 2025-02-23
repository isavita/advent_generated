
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class ImmuneSystemSimulator {

    private static final String INPUT_FILE = "input.txt";

    public static void main(String[] args) {
        try {
            List<Group> immuneSystem = new ArrayList<>();
            List<Group> infection = new ArrayList<>();
            parseInput(immuneSystem, infection);

            while (!immuneSystem.isEmpty() && !infection.isEmpty()) {
                // Target Selection Phase
                Map<Group, Group> targets = new HashMap<>();
                List<Group> allGroups = new ArrayList<>();
                allGroups.addAll(immuneSystem);
                allGroups.addAll(infection);

                allGroups.sort(Comparator.<Group, Integer>comparing(g -> g.getEffectivePower())
                        .thenComparing(g -> g.initiative).reversed());

                Set<Group> targetedGroups = new HashSet<>();
                for (Group attacker : allGroups) {
                    List<Group> enemies = (attacker.type == GroupType.IMMUNE_SYSTEM) ? infection : immuneSystem;
                    Group target = selectTarget(attacker, enemies, targetedGroups);
                    if (target != null) {
                        targets.put(attacker, target);
                        targetedGroups.add(target);
                    }
                }

                // Attacking Phase
                allGroups.sort(Comparator.comparing(g -> g.initiative, Comparator.reverseOrder()));
                for (Group attacker : allGroups) {
                    if (attacker.units <= 0) continue; // Group already dead
                    Group target = targets.get(attacker);
                    if (target != null) {
                        int damage = calculateDamage(attacker, target);
                        int unitsKilled = Math.min(target.units, damage / target.hitPoints);
                        target.units -= unitsKilled;
                    }
                }

                // Remove dead groups
                immuneSystem.removeIf(g -> g.units <= 0);
                infection.removeIf(g -> g.units <= 0);

                // Check if all groups have 0 units.
                if (immuneSystem.stream().allMatch(group -> group.units <= 0) && infection.stream().allMatch(group -> group.units <= 0))
                {
                    //In a situation where neither team can do any damage to the other,
                    // and a situation could occur where the same fight would occur repeatedly with no change.
                    break;
                }
            }

            int remainingUnits = 0;
            if (!immuneSystem.isEmpty()) {
                remainingUnits = immuneSystem.stream().mapToInt(g -> g.units).sum();
            } else if (!infection.isEmpty()) {
                remainingUnits = infection.stream().mapToInt(g -> g.units).sum();
            }
            System.out.println("Remaining units: " + remainingUnits);

        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static Group selectTarget(Group attacker, List<Group> enemies, Set<Group> targetedGroups) {
        return enemies.stream()
                .filter(defender -> !targetedGroups.contains(defender))
                .filter(defender -> calculateDamage(attacker, defender) > 0)
                .max(Comparator.<Group, Integer>comparing(defender -> calculateDamage(attacker, defender))
                        .thenComparing(defender -> defender.getEffectivePower())
                        .thenComparing(defender -> defender.initiative))
                .orElse(null);
    }

    private static int calculateDamage(Group attacker, Group defender) {
        if (defender.immunities.contains(attacker.attackType)) {
            return 0;
        }
        int damage = attacker.getEffectivePower();
        if (defender.weaknesses.contains(attacker.attackType)) {
            damage *= 2;
        }
        return damage;
    }

    private static void parseInput(List<Group> immuneSystem, List<Group> infection) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader(INPUT_FILE))) {
            String line;
            GroupType currentType = null;
            Pattern groupPattern = Pattern.compile(
                    "(\\d+) units each with (\\d+) hit points (\\((.*)\\) )?with an attack that does (\\d+) (\\w+) damage at initiative (\\d+)"
            );

            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) continue;

                if (line.equals("Immune System:")) {
                    currentType = GroupType.IMMUNE_SYSTEM;
                } else if (line.equals("Infection:")) {
                    currentType = GroupType.INFECTION;
                } else {
                    Matcher matcher = groupPattern.matcher(line);
                    if (matcher.matches()) {
                        int units = Integer.parseInt(matcher.group(1));
                        int hitPoints = Integer.parseInt(matcher.group(2));
                        String weaknessesAndImmunities = matcher.group(4);
                        int attackDamage = Integer.parseInt(matcher.group(5));
                        String attackType = matcher.group(6);
                        int initiative = Integer.parseInt(matcher.group(7));

                        Set<String> weaknesses = new HashSet<>();
                        Set<String> immunities = new HashSet<>();

                        if (weaknessesAndImmunities != null) {
                            String[] parts = weaknessesAndImmunities.split("; ");
                            for (String part : parts) {
                                if (part.startsWith("weak to ")) {
                                    String[] weakTypes = part.substring("weak to ".length()).split(", ");
                                    weaknesses.addAll(Arrays.asList(weakTypes));
                                } else if (part.startsWith("immune to ")) {
                                    String[] immuneTypes = part.substring("immune to ".length()).split(", ");
                                    immunities.addAll(Arrays.asList(immuneTypes));
                                }
                            }
                        }
                        Group group = new Group(currentType, units, hitPoints, weaknesses, immunities, attackDamage, attackType, initiative);
                        if (currentType == GroupType.IMMUNE_SYSTEM) {
                            immuneSystem.add(group);
                        } else {
                            infection.add(group);
                        }
                    }
                }
            }
        }
    }

    enum GroupType {
        IMMUNE_SYSTEM,
        INFECTION
    }

    static class Group {
        GroupType type;
        int units;
        int hitPoints;
        Set<String> weaknesses;
        Set<String> immunities;
        int attackDamage;
        String attackType;
        int initiative;

        public Group(GroupType type, int units, int hitPoints, Set<String> weaknesses, Set<String> immunities,
                     int attackDamage, String attackType, int initiative) {
            this.type = type;
            this.units = units;
            this.hitPoints = hitPoints;
            this.weaknesses = weaknesses;
            this.immunities = immunities;
            this.attackDamage = attackDamage;
            this.attackType = attackType;
            this.initiative = initiative;
        }

        public int getEffectivePower() {
            return units * attackDamage;
        }
    }
}
