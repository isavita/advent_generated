
<?php

class Group {
    public $Units;
    public $HitPoints;
    public $AttackDamage;
    public $AttackType;
    public $Initiative;
    public $Immunities;
    public $Weaknesses;
    public $Attacker;
    public $Target;

    public function __construct($units, $hitPoints, $attackDamage, $attackType, $initiative, $immunities = [], $weaknesses = []) {
        $this->Units = $units;
        $this->HitPoints = $hitPoints;
        $this->AttackDamage = $attackDamage;
        $this->AttackType = $attackType;
        $this->Initiative = $initiative;
        $this->Immunities = $immunities;
        $this->Weaknesses = $weaknesses;
    }

    public function EffectivePower() {
        return $this->Units * $this->AttackDamage;
    }

    public function DamageDealt($e) {
        if (in_array($this->AttackType, $e->Immunities)) {
            return 0;
        }
        if (in_array($this->AttackType, $e->Weaknesses)) {
            return $this->EffectivePower() * 2;
        }
        return $this->EffectivePower();
    }
}

function PrepareForBattle($input) {
    $initiative = [];
    $battle = [
        "Immune System" => [],
        "Infection" => []
    ];
    $currentArmy = null;

    foreach ($input as $line) {
        if (preg_match('/^(.*):$/', $line, $matches)) {
            $currentArmy = $matches[1];
        } else if (preg_match('/^(\d+) units each with (\d+) hit points.*with an attack that does (\d+) (\w+) damage at initiative (\d+)$/', $line, $matches)) {
            $group = new Group(
                (int)$matches[1],
                (int)$matches[2],
                (int)$matches[3],
                $matches[4],
                (int)$matches[5]
            );

            if (preg_match('/immune to (.*?)[;)]/', $line, $immuneMatches)) {
                $group->Immunities = explode(', ', $immuneMatches[1]);
            }
            if (preg_match('/weak to (.*?)[;)]/', $line, $weakMatches)) {
                $group->Weaknesses = explode(', ', $weakMatches[1]);
            }

            $battle[$currentArmy][] = $group;
            $initiative[] = $group;
        }
    }

    return [$battle, $initiative];
}

function FindTargets(&$battle) {
    foreach ($battle as $armyName => &$groups) {
      usort($groups, function ($a, $b) {
          if ($a->EffectivePower() != $b->EffectivePower()) {
                return $b->EffectivePower() - $a->EffectivePower();
            }
            return $b->Initiative - $a->Initiative;
      });
        foreach ($groups as &$group) {
            $mostDamage = 0;
            $targetGroup = null;

            foreach ($battle as $enemyArmyName => &$enemyGroups) {
                if ($armyName == $enemyArmyName || $group->Units <= 0) {
                    continue;
                }

                foreach ($enemyGroups as &$enemyGroup) {
                    if ($enemyGroup->Units <= 0 || $enemyGroup->Attacker !== null || $group->DamageDealt($enemyGroup) == 0 || $group->DamageDealt($enemyGroup) < $mostDamage) {
                        continue;
                    }
                    if ($group->DamageDealt($enemyGroup) == $mostDamage && $targetGroup !== null) {
                        if ($enemyGroup->EffectivePower() < $targetGroup->EffectivePower()) {
                            continue;
                        }
                        if ($enemyGroup->EffectivePower() == $targetGroup->EffectivePower() && $enemyGroup->Initiative < $targetGroup->Initiative) {
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

function Attack(&$initiative) {
    usort($initiative, function ($a, $b) {
        return $b->Initiative - $a->Initiative;
    });

    foreach ($initiative as $group) {
        if ($group->Units > 0 && $group->Target !== null && $group->Target->Units > 0) {
            $group->Target->Units -= floor($group->DamageDealt($group->Target) / $group->Target->HitPoints);
        }
        if ($group->Target !== null) {
            $group->Target->Attacker = null;
            $group->Target = null;
        }
    }
}

function Clean(&$battlefieldOrInitiative) {
  foreach ($battlefieldOrInitiative as &$armyOrGroup) {
      if (is_array($armyOrGroup))
      {
          $armyOrGroup = array_filter($armyOrGroup, function ($g) {
            return $g->Units > 0;
          });
      }
  }
    if (is_array(reset($battlefieldOrInitiative)))
    {
        foreach ($battlefieldOrInitiative as &$army)
        {
            usort($army, function ($a, $b) {
                return $b->Initiative - $a->Initiative;
            });
        }
    } else {
        usort($battlefieldOrInitiative, function ($a, $b) {
            return $b->Initiative - $a->Initiative;
        });
    }
}

function Active($battle) {
    foreach ($battle as $army) {
        $alive = false;
        foreach ($army as $g) {
          if ($g->Units > 0)
          {
            $alive = true;
            break;
          }
        }
        if (!$alive)
        {
          return false;
        }
    }
    return true;
}

function Result($battle) {
    $winner = null;
    $units = 0;

    foreach ($battle as $armyName => $groups) {
      $alive = false;
      $armyUnits = 0;
        foreach ($groups as $g)
        {
          if ($g->Units > 0)
          {
            $alive = true;
            $armyUnits += $g->Units;
          }
        }
        if ($alive) {
            $winner = $armyName;
            $units = $armyUnits;
        }
    }

    return [$winner, $units];
}

function TotalUnits($battle) {
    $sum = 0;
    foreach ($battle as $groups) {
        foreach ($groups as $group) {
            if ($group->Units > 0) {
                $sum += $group->Units;
            }
        }
    }
    return $sum;
}

function ImmuneSystemBoost($input) {
    $boost = 0;

    while (true) {
        $stalemate = false;
        list($battle, $initiative) = PrepareForBattle($input);

        foreach ($battle["Immune System"] as $group) {
            $group->AttackDamage += $boost;
        }

        while (Active($battle)) {
            $before = TotalUnits($battle);

            FindTargets($battle);
            Attack($initiative);

            if (TotalUnits($battle) == $before) {
                $stalemate = true;
                break;
            }
            Clean($battle);
            Clean($initiative);
        }

        if (!$stalemate) {
            list($winner, $units) = Result($battle);
            if ($winner == "Immune System") {
                return $units;
            }
        }

        $boost++;
    }
}

$input = file("input.txt", FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);
echo ImmuneSystemBoost($input);

?>
