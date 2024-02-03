
use strict;
use warnings;

my $input_file = "input.txt";
open(my $fh, "<", $input_file) or die "Could not open file '$input_file' $!";

my $bossHP;
my $bossDamage;
my $line_num = 0;
while (my $row = <$fh>) {
    chomp $row;
    if ($line_num == 0) {
        $bossHP = (split /: /, $row)[1];
    } elsif ($line_num == 1) {
        $bossDamage = (split /: /, $row)[1];
    }
    $line_num++;
}

my %initialState = (
    playerHP => 50,
    playerMana => 500,
    bossHP => $bossHP,
    bossDamage => $bossDamage,
    shieldTimer => 0,
    poisonTimer => 0,
    rechargeTimer => 0,
    manaSpent => 0
);

my $minMana = ~0 >> 1;

sub simulate {
    my ($state, $playerTurn) = @_;
    if ($state->{manaSpent} >= $minMana) {
        return;
    }
    if ($state->{bossHP} <= 0) {
        $minMana = $state->{manaSpent};
        return;
    }
    if ($state->{playerHP} <= 0) {
        return;
    }

    if ($state->{shieldTimer} > 0) {
        $state->{shieldTimer}--;
    }
    if ($state->{poisonTimer} > 0) {
        $state->{bossHP} -= 3;
        $state->{poisonTimer}--;
    }
    if ($state->{rechargeTimer} > 0) {
        $state->{playerMana} += 101;
        $state->{rechargeTimer}--;
    }

    if (!$playerTurn) {
        my $damage = $state->{bossDamage};
        if ($state->{shieldTimer} > 0) {
            $damage -= 7;
        }
        $damage = 1 if $damage < 1;
        $state->{playerHP} -= $damage;
        simulate($state, 1);
        return;
    }

    if ($state->{playerMana} >= 53) {
        my %newState = %$state;
        $newState{playerMana} -= 53;
        $newState{manaSpent} += 53;
        $newState{bossHP} -= 4;
        simulate(\%newState, 0);
    }
    if ($state->{playerMana} >= 73) {
        my %newState = %$state;
        $newState{playerMana} -= 73;
        $newState{manaSpent} += 73;
        $newState{bossHP} -= 2;
        $newState{playerHP} += 2;
        simulate(\%newState, 0);
    }
    if ($state->{playerMana} >= 113 && $state->{shieldTimer} == 0) {
        my %newState = %$state;
        $newState{playerMana} -= 113;
        $newState{manaSpent} += 113;
        $newState{shieldTimer} = 6;
        simulate(\%newState, 0);
    }
    if ($state->{playerMana} >= 173 && $state->{poisonTimer} == 0) {
        my %newState = %$state;
        $newState{playerMana} -= 173;
        $newState{manaSpent} += 173;
        $newState{poisonTimer} = 6;
        simulate(\%newState, 0);
    }
    if ($state->{playerMana} >= 229 && $state->{rechargeTimer} == 0) {
        my %newState = %$state;
        $newState{playerMana} -= 229;
        $newState{manaSpent} += 229;
        $newState{rechargeTimer} = 5;
        simulate(\%newState, 0);
    }
}

simulate(\%initialState, 1);
print "$minMana\n";
