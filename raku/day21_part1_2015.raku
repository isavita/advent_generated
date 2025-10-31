
sub MAIN() {
    my $input = "input.txt".IO.slurp;
    my @lines = $input.lines;
    
    my $boss-hp = @lines[0].split(':')[1].trim.Int;
    my $boss-damage = @lines[1].split(':')[1].trim.Int;
    my $boss-armor = @lines[2].split(':')[1].trim.Int;
    
    my @weapons = (
        { cost => 8,  damage => 4, armor => 0 },
        { cost => 10, damage => 5, armor => 0 },
        { cost => 25, damage => 6, armor => 0 },
        { cost => 40, damage => 7, armor => 0 },
        { cost => 74, damage => 8, armor => 0 }
    );
    
    my @armor = (
        { cost => 0,  damage => 0, armor => 0 },
        { cost => 13, damage => 0, armor => 1 },
        { cost => 31, damage => 0, armor => 2 },
        { cost => 53, damage => 0, armor => 3 },
        { cost => 75, damage => 0, armor => 4 },
        { cost => 102, damage => 0, armor => 5 }
    );
    
    my @rings = (
        { cost => 0,   damage => 0, armor => 0 },
        { cost => 25,  damage => 1, armor => 0 },
        { cost => 50,  damage => 2, armor => 0 },
        { cost => 100, damage => 3, armor => 0 },
        { cost => 20,  damage => 0, armor => 1 },
        { cost => 40,  damage => 0, armor => 2 },
        { cost => 80,  damage => 0, armor => 3 }
    );
    
    my $player-hp = 100;
    my $min-gold = Inf;
    
    for @weapons -> $w {
        for @armor -> $a {
            for ^@rings -> $i {
                for $i+1 ..^ @rings -> $j {
                    my $r1 = @rings[$i];
                    my $r2 = @rings[$j];
                    
                    my $current-cost = $w<cost> + $a<cost> + $r1<cost> + $r2<cost>;
                    my $player-damage = $w<damage> + $a<damage> + $r1<damage> + $r2<damage>;
                    my $player-armor = $w<armor> + $a<armor> + $r1<armor> + $r2<armor>;
                    
                    my $player-effective-damage = max(1, $player-damage - $boss-armor);
                    my $player-turns = ($boss-hp + $player-effective-damage - 1) div $player-effective-damage;
                    
                    my $boss-effective-damage = max(1, $boss-damage - $player-armor);
                    my $boss-turns = ($player-hp + $boss-effective-damage - 1) div $boss-effective-damage;
                    
                    if $player-turns <= $boss-turns {
                        $min-gold = min($min-gold, $current-cost);
                    }
                }
            }
        }
    }
    
    say $min-gold;
}
