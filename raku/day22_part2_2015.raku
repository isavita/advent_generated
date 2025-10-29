
#!/usr/bin/env raku
use v6;

my $min-mana = Inf;

sub apply-effects(%s) {
    %s<shieldTimer>   = %s<shieldTimer>   - 1 if %s<shieldTimer>   > 0;
    if %s<poisonTimer> > 0 {
        %s<bossHP>      -= 3;
        %s<poisonTimer> = %s<poisonTimer> - 1;
    }
    if %s<rechargeTimer> > 0 {
        %s<playerMana> += 101;
        %s<rechargeTimer> = %s<rechargeTimer> - 1;
    }
    %s;
}

sub simulate(%state, Bool $player-turn) {
    return if %state<manaSpent> >= $min-mana;
    return if %state<bossHP> <= 0 && ( $min-mana = %state<manaSpent> ) && True;
    return if %state<playerHP> <= 0;

    # hard mode: player loses 1 HP at start of his turn
    if $player-turn {
        %state<playerHP>--;
        return if %state<playerHP> <= 0;
    }

    %state = apply-effects(%state);

    # boss turn
    unless $player-turn {
        my $damage = %state<bossDamage>;
        $damage -= 7 if %state<shieldTimer> > 0;
        $damage = 1 if $damage < 1;
        %state<playerHP> -= $damage;
        simulate(%state, True);
        return;
    }

    # player spells
    if %state<playerMana> >= 53 {                     # Magic Missile
        my %n = %state;
        %n<playerMana> -= 53;
        %n<manaSpent>  += 53;
        %n<bossHP>     -= 4;
        simulate(%n, False);
    }
    if %state<playerMana> >= 73 {                     # Drain
        my %n = %state;
        %n<playerMana> -= 73;
        %n<manaSpent>  += 73;
        %n<bossHP>     -= 2;
        %n<playerHP>   += 2;
        simulate(%n, False);
    }
    if %state<playerMana> >= 113 && %state<shieldTimer> == 0 {  # Shield
        my %n = %state;
        %n<playerMana>  -= 113;
        %n<manaSpent>   += 113;
        %n<shieldTimer> = 6;
        simulate(%n, False);
    }
    if %state<playerMana> >= 173 && %state<poisonTimer> == 0 {  # Poison
        my %n = %state;
        %n<playerMana>  -= 173;
        %n<manaSpent>   += 173;
        %n<poisonTimer> = 6;
        simulate(%n, False);
    }
    if %state<playerMana> >= 229 && %state<rechargeTimer> == 0 {# Recharge
        my %n = %state;
        %n<playerMana>    -= 229;
        %n<manaSpent>     += 229;
        %n<rechargeTimer> = 5;
        simulate(%n, False);
    }
}

sub MAIN() {
    my $file = "input.txt".IO;
    die "Cannot read input.txt" unless $file.e;

    my ($bossHP, $bossDamage) = $file.lines.map({
        .match(/'Hit Points:' \s* (\d+)/) ?? $0.Int !!
        .match(/'Damage:' \s* (\d+)/) ?? $0.Int !!
        Nil
    }).grep(*.defined);

    my %initial = (
        playerHP      => 50,
        playerMana    => 500,
        bossHP        => $bossHP,
        bossDamage    => $bossDamage,
        shieldTimer   => 0,
        poisonTimer   => 0,
        rechargeTimer => 0,
        manaSpent     => 0,
    );

    simulate(%initial, True);
    say $min-mana;
}
