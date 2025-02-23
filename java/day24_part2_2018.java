
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class ImmuneSystemSimulator {

    private static final String INPUT_FILE = "input.txt";
    private static final Pattern GROUP_PATTERN = Pattern.compile(
            "(\\d+) units each with (\\d+) hit points (?:\\(([^)]*)\\) )?with an attack that does (\\d+) (\\w+) damage at initiative (\\d+)");

    public static void main(String[] args) {
        try {
            List<Group> immuneSystem = new ArrayList<>();
            List<Group> infection = new ArrayList<>();
            readInput(INPUT_FILE, immuneSystem, infection);

            // Part One: Original Combat
            int originalWinningUnits = simulateCombat(cloneGroups(immuneSystem), cloneGroups(infection), 0);
            System.out.println("Part One - Winning army units: " + originalWinningUnits);

            // Part Two: Find minimum boost
            int boost = 0;
            int winningUnits = 0;
            while (true) {
                boost++;
                List<Group> boostedImmuneSystem = cloneGroups(immuneSystem);
                List<Group> boostedInfection = cloneGroups(infection);
                int result = simulateCombat(boostedImmuneSystem, boostedInfection, boost);

                if (result > 0) {
                    boolean immuneSystemWon = boostedImmuneSystem.stream().mapToInt(g -> g.units).sum() > 0 &&
                            boostedInfection.stream().mapToInt(g->g.units).sum() == 0;
                    
                    if (immuneSystemWon) {
                      winningUnits = boostedImmuneSystem.stream().mapToInt(g -> g.units).sum();
                        break; // Immune system won
                    }
                }
                if (boost > 1000) break;
            }
                System.out.println("Part Two - Immune system units with boost: " + winningUnits);


        } catch (IOException e) {
            System.err.println("Error reading input file: " + e.getMessage());
        }
    }

    private static void readInput(String filename, List<Group> immuneSystem, List<Group> infection) throws IOException {
        try (BufferedReader reader = new BufferedReader(new FileReader(filename))) {
            String line;
            List<Group> currentArmy = null;

            while ((line = reader.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty()) {
                    continue;
                }

                if (line.equals("Immune System:")) {
                    currentArmy = immuneSystem;
                } else if (line.equals("Infection:")) {
                    currentArmy = infection;
                } else {
                    Matcher matcher = GROUP_PATTERN.matcher(line);
                    if (matcher.matches()) {
                        int units = Integer.parseInt(matcher.group(1));
                        int hp = Integer.parseInt(matcher.group(2));
                        String weaknessesImmunities = matcher.group(3);
                        int damage = Integer.parseInt(matcher.group(4));
                        String attackType = matcher.group(5);
                        int initiative = Integer.parseInt(matcher.group(6));

                        Set<String> weaknesses = new HashSet<>();
                        Set<String> immunities = new HashSet<>();

                        if (weaknessesImmunities != null) {
                            String[] parts = weaknessesImmunities.split("; ");
                            for (String part : parts) {
                                if (part.startsWith("weak to ")) {
                                    weaknesses.addAll(Arrays.asList(part.substring(8).split(", ")));
                                } else if (part.startsWith("immune to ")) {
                                    immunities.addAll(Arrays.asList(part.substring(10).split(", ")));
                                }
                            }
                        }
                        boolean isImmune = currentArmy == immuneSystem;
                        currentArmy.add(new Group(units, hp, weaknesses, immunities, damage, attackType, initiative, isImmune));
                    }
                }
            }
        }
    }


    private static int simulateCombat(List<Group> immuneSystem, List<Group> infection, int boost) {
       for (Group group : immuneSystem){
           group.attackDamage += boost;
       }


        while (!immuneSystem.isEmpty() && !infection.isEmpty()) {
            // Target Selection
            Map<Group, Group> targets = new HashMap<>();
            List<Group> allGroups = new ArrayList<>(immuneSystem);
            allGroups.addAll(infection);
            allGroups.sort(Comparator.<Group, Integer>comparing(g -> g.getEffectivePower()).reversed()
                    .thenComparing(Comparator.<Group, Integer>comparing(g -> g.initiative).reversed()));

            Set<Group> targeted = new HashSet<>();
            for (Group attacker : allGroups) {
                List<Group> potentialTargets = attacker.isImmune ? infection : immuneSystem;
                Group target = selectTarget(attacker, potentialTargets, targeted);
                if (target != null) {
                    targets.put(attacker, target);
                    targeted.add(target);
                }
            }

            // Attacking
            allGroups.sort(Comparator.<Group, Integer>comparing(g -> g.initiative).reversed());
            boolean damageDealtThisRound = false;
            for (Group attacker : allGroups) {
                if (attacker.units <= 0) {
                    continue; // Group is already dead
                }
                Group defender = targets.get(attacker);
                if (defender != null) {
                    int damage = calculateDamage(attacker, defender);
                    int unitsKilled = Math.min(defender.units, damage / defender.hp);
                    defender.units -= unitsKilled;
                    if (unitsKilled > 0){
                        damageDealtThisRound = true;
                    }
                }
            }
            if (!damageDealtThisRound){
                return -1;
            }

            immuneSystem.removeIf(group -> group.units <= 0);
            infection.removeIf(group -> group.units <= 0);
        }
        return immuneSystem.isEmpty() ? -infection.stream().mapToInt(g->g.units).sum() : immuneSystem.stream().mapToInt(g->g.units).sum();
    }



    private static Group selectTarget(Group attacker, List<Group> potentialTargets, Set<Group> alreadyTargeted) {
        return potentialTargets.stream()
                .filter(defender -> !alreadyTargeted.contains(defender))
                .max(Comparator.<Group, Integer>comparing(defender -> calculateDamage(attacker, defender))
                        .thenComparing(defender -> defender.getEffectivePower())
                        .thenComparing(defender -> defender.initiative))
                .filter(defender -> calculateDamage(attacker, defender) > 0)
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


    private static List<Group> cloneGroups(List<Group> groups) {
        return groups.stream().map(Group::new).collect(Collectors.toList());
    }



    static class Group {
        int units;
        int hp;
        Set<String> weaknesses;
        Set<String> immunities;
        int attackDamage;
        String attackType;
        int initiative;
        boolean isImmune;


        public Group(int units, int hp, Set<String> weaknesses, Set<String> immunities, int attackDamage,
                     String attackType, int initiative, boolean isImmune) {
            this.units = units;
            this.hp = hp;
            this.weaknesses = weaknesses;
            this.immunities = immunities;
            this.attackDamage = attackDamage;
            this.attackType = attackType;
            this.initiative = initiative;
            this.isImmune = isImmune;
        }

        // Copy constructor for cloning
        public Group(Group other) {
            this.units = other.units;
            this.hp = other.hp;
            this.weaknesses = new HashSet<>(other.weaknesses);
            this.immunities = new HashSet<>(other.immunities);
            this.attackDamage = other.attackDamage;
            this.attackType = other.attackType;
            this.initiative = other.initiative;
            this.isImmune = other.isImmune;

        }

        public int getEffectivePower() {
            return units * attackDamage;
        }
    }
}
