
my $min = ∞;
my %start;

sub simulate($state, Bool $player-turn) {
    return if $state<spent> ≥ $min;
    if $state<boss> ≤ 0 {
        $min = $state<spent>;
        return;
    }
    return if $state<hp> ≤ 0;

    $state<shield>-- if $state<shield>;
    if $state<poison> {
        $state<boss> -= 3;
        $state<poison>--;
    }
    if $state<recharge> {
        $state<mana> += 101;
        $state<recharge>--;
    }

    unless $player-turn {
        my $dmg = max 1, $state<boss-dmg> - ($state<shield> ?? 7 !! 0);
        $state<hp> -= $dmg;
        simulate $state, True;
        return;
    }

    if $state<mana> ≥ 53 {
        my %s = $state;
        %s<mana> -= 53;
        %s<spent> += 53;
        %s<boss> -= 4;
        simulate %s, False;
    }
    if $state<mana> ≥ 73 {
        my %s = $state;
        %s<mana> -= 73;
        %s<spent> += 73;
        %s<boss> -= 2;
        %s<hp> += 2;
        simulate %s, False;
    }
    if $state<mana> ≥ 113 && !$state<shield> {
        my %s = $state;
        %s<mana> -= 113;
        %s<spent> += 113;
        %s<shield> = 6;
        simulate %s, False;
    }
    if $state<mana> ≥ 173 && !$state<poison> {
        my %s = $state;
        %s<mana> -= 173;
        %s<spent> += 173;
        %s<poison> = 6;
        simulate %s, False;
    }
    if $state<mana> ≥ 229 && !$state<recharge> {
        my %s = $state;
        %s<mana> -= 229;
        %s<spent> += 229;
        %s<recharge> = 5;
        simulate %s, False;
    }
}

sub MAIN() {
    for 'input.txt'.IO.lines {
        if /Hit \s Points\: \s (\d+)/ { %start<boss> = +$0 }
        if /Damage\: \s (\d+)/ { %start<boss-dmg> = +$0 }
    }
    %start<hp> = 50;
    %start<mana> = 500;
    %start<spent> = 0;
    simulate %start, True;
    say $min;
}
