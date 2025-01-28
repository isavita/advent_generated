
<?php

class Group {
    public int $Units;
    public int $HitPoints;
    public int $AttackDamage;
    public string $AttackType;
    public int $Initiative;
    public array $Immunities;
    public array $Weaknesses;

    public ?Group $Attacker = null;
    public ?Group $Target = null;

    public function __construct(int $Units, int $HitPoints, int $AttackDamage, string $AttackType, int $Initiative, array $Immunities, array $Weaknesses) {
        $this->Units = $Units;
        $this->HitPoints = $HitPoints;
        $this->AttackDamage = $AttackDamage;
        $this->AttackType = $AttackType;
        $this->Initiative = $Initiative;
        $this->Immunities = $Immunities;
        $this->Weaknesses = $Weaknesses;
    }

    public function EffectivePower(): int {
        return $this->Units * $this->AttackDamage;
    }

    public function DamageDealt(Group $e): int {
        if (in_array($this->AttackType, $e->Immunities)) {
            return 0;
        }
        if (in_array($this->AttackType, $e->Weaknesses)) {
            return $this->EffectivePower() * 2;
        }
        return $this->EffectivePower();
    }

    public function __toString(): string {
        $out = "{$this->Units} units each with {$this->HitPoints} hit points";
        if (count($this->Immunities) > 0 || count($this->Weaknesses) > 0) {
            $out .= " (";
            if (count($this->Immunities) > 0) {
                $out .= "immune to " . implode(" and ", $this->Immunities);
                if (count($this->Weaknesses) > 0) {
                    $out .= "; ";
                }
            }
            if (count($this->Weaknesses) > 0) {
                $out .= "weak to " . implode(" and ", $this->Weaknesses);
            }
            $out .= ")";
        }
        $out .= " with an attack that does {$this->AttackDamage} {$this->AttackType} damage at initiative {$this->Initiative}";
        return $out;
    }
}

class Initiative implements \Countable, \Iterator {
    private array $groups = [];
    private int $position = 0;

    public function addGroup(Group $group): void {
        $this->groups[] = $group;
    }

    public function count(): int {
        return count($this->groups);
    }

    public function rewind(): void {
        $this->position = 0;
        usort($this->groups, function ($a, $b) {
            return $b->Initiative <=> $a->Initiative;
        });
    }

    public function current(): Group {
        return $this->groups[$this->position];
    }

    public function key(): int {
        return $this->position;
    }

    public function next(): void {
        ++$this->position;
    }

    public function valid(): bool {
        return isset($this->groups[$this->position]);
    }

    public function attack(): void {
        $this->rewind();
        foreach ($this as $group) {
            if ($group->Units > 0 && $group->Target !== null && $group->Target->Units > 0) {
                $group->Target->Units -= intdiv($group->DamageDealt($group->Target), $group->Target->HitPoints);
            }
            if ($group->Target !== null) {
                $group->Target->Attacker = null;
                $group->Target = null;
            }
        }
    }

    public function clean(): void {
        $this->groups = array_values(array_filter($this->groups, function ($g) {
            return $g->Units > 0;
        }));
        usort($this->groups, function ($a, $b) {
            return $b->Initiative <=> $a->Initiative;
        });
    }

    public function getGroups(): array {
        return $this->groups;
    }
}

class Army implements \Countable, \Iterator {
    private array $groups = [];
    private int $position = 0;

    public function addGroup(Group $group): void {
        $this->groups[] = $group;
    }

    public function count(): int {
        return count($this->groups);
    }

    public function rewind(): void {
        $this->position = 0;
        usort($this->groups, function ($a, $b) {
            if ($a->EffectivePower() !== $b->EffectivePower()) {
                return $b->EffectivePower() <=> $a->EffectivePower();
            }
            return $b->Initiative <=> $a->Initiative;
        });
    }

    public function current(): Group {
        return $this->groups[$this->position];
    }

    public function key(): int {
        return $this->position;
    }

    public function next(): void {
        ++$this->position;
    }

    public function valid(): bool {
        return isset($this->groups[$this->position]);
    }

    public function alive(): bool {
        foreach ($this->groups as $group) {
            if ($group->Units > 0) {
                return true;
            }
        }
        return false;
    }

    public function boost(int $amount): void {
        foreach ($this->groups as $group) {
            $group->AttackDamage += $amount;
        }
    }

    public function getGroups(): array {
        return $this->groups;
    }
}

class Battlefield {
    private array $armies = [];

    public function addArmy(int $armyId, Army $army): void {
        $this->armies[$armyId] = $army;
    }

    public function getArmy(int $armyId): Army {
        return $this->armies[$armyId];
    }

    public function findTargets(): void {
        foreach ($this->armies as $armyId => $army) {
            $army->rewind();
            foreach ($army as $group) {
                $mostDamage = 0;
                $targetGroup = null;

                foreach ($this->armies as $enemyArmyId => $enemyArmy) {
                    if ($armyId === $enemyArmyId || $group->Units <= 0) {
                        continue;
                    }
                    foreach ($enemyArmy as $enemyGroup) {
                        if ($enemyGroup->Units <= 0 || $enemyGroup->Attacker !== null || $group->DamageDealt($enemyGroup) === 0 || $group->DamageDealt($enemyGroup) < $mostDamage) {
                            continue;
                        }
                        if ($group->DamageDealt($enemyGroup) === $mostDamage && $targetGroup !== null) {
                            if ($enemyGroup->EffectivePower() < $targetGroup->EffectivePower()) {
                                continue;
                            }
                            if ($enemyGroup->EffectivePower() === $targetGroup->EffectivePower() && $enemyGroup->Initiative < $targetGroup->Initiative) {
                                continue;
                            }
                        }
                        $mostDamage = $group->DamageDealt($enemyGroup);
                        $targetGroup = $enemyGroup;
                    }
                }
                if ($targetGroup !== null) {
                    $group->Target = $targetGroup;
                    $targetGroup->Attacker = $group;
                }
            }
        }
    }

    public function clean(): void {
        foreach ($this->armies as $armyId => $army) {
            $currentGroups = $army->getGroups();
            $cleanedGroups = [];
            foreach($currentGroups as $group) {
                if ($group->Units > 0) {
                    $cleanedGroups[] = $group;
                }
            }
            $army = new Army();
            foreach($cleanedGroups as $g) {
                $army->addGroup($g);
            }
            $this->armies[$armyId] = $army;
        }
    }

    public function active(): bool {
        foreach ($this->armies as $army) {
            if (!$army->alive()) {
                return false;
            }
        }
        return true;
    }

    public function result(): array {
        $winner = 0;
        $units = 0;

        foreach ($this->armies as $armyId => $army) {
            if ($army->alive()) {
                $winner = $armyId;
                foreach ($army as $group) {
                    if ($group->Units > 0) {
                        $units += $group->Units;
                    }
                }
            }
        }

        return [$winner, $units];
    }

    public function totalUnits(): int {
        $sum = 0;
        foreach ($this->armies as $army) {
            foreach ($army as $group) {
                if ($group->Units > 0) {
                    $sum += $group->Units;
                }
            }
        }
        return $sum;
    }
}

function prepareForBattle(array $input): array {
    $initiative = new Initiative();
    $battle = new Battlefield();
    $currentArmy = 0;

    $armyNameRegex = '/^(.*):$/';
    $groupImmunitiesRegex = '/immune to (.*?)[;)]/';
    $groupWeaknessesRegex = '/weak to (.*?)[;)]/';
    $groupDescriptionRegex = '/^(\d+) units each with (\d+) hit points.*with an attack that does (\d+) (\w+) damage at initiative (\d+)$/';

    $stringArmies = [
        "Immune System" => 1,
        "Infection" => 2,
    ];
    $armyImmuneSystem = 1;
    $armyInfection = 2;
    $armyCount = 3;

    foreach ($input as $line) {
        if (preg_match($armyNameRegex, $line, $matches)) {
            if (isset($stringArmies[$matches[1]])) {
                $currentArmy = $stringArmies[$matches[1]];
                $battle->addArmy($currentArmy, new Army());
            } else {
                throw new Exception("unknown army: " . $matches[1]);
            }
        } else {
            if ($currentArmy <= 0 || $currentArmy >= $armyCount) {
                throw new Exception("tried to assign group to invalid army: " . $currentArmy);
            }
            if (preg_match($groupDescriptionRegex, $line, $description)) {
                $immunities = [];
                if (preg_match($groupImmunitiesRegex, $line, $immuneMatches)) {
                    $immunities = array_map('trim', explode(', ', $immuneMatches[1]));
                }

                $weaknesses = [];
                if (preg_match($groupWeaknessesRegex, $line, $weakMatches)) {
                    $weaknesses = array_map('trim', explode(', ', $weakMatches[1]));
                }

                $group = new Group(
                    intval($description[1]),
                    intval($description[2]),
                    intval($description[3]),
                    $description[4],
                    intval($description[5]),
                    $immunities,
                    $weaknesses
                );

                $battle->getArmy($currentArmy)->addGroup($group);
                $initiative->addGroup($group);
            }
        }
    }

    return [$battle, $initiative];
}

function conditionFight(array $input): int {
    list($battle, $initiative) = prepareForBattle($input);

    while ($battle->active()) {
        $battle->findTargets();
        $initiative->attack();
        $battle->clean();
        $initiative->clean();
        if (!$battle->active()) break;
        $prev_units = $battle->totalUnits();
        $battle->findTargets();
        $initiative->attack();
        $battle->clean();
        $initiative->clean();
        if ($battle->totalUnits() == $prev_units && $battle->active()) return 0; // Stale mate check
    }

    list($winner, $units) = $battle->result();
    return $units;
}

$input = file("input.txt", FILE_IGNORE_NEW_LINES);
echo conditionFight($input) . "\n";

?>
