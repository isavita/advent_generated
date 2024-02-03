
use strict;
use warnings;

my $input_file = "input.txt";
open(my $fh, '<', $input_file) or die "Could not open file '$input_file' $!";
my @lines = <$fh>;
close $fh;

my $bossHP = (split /: /, $lines[0])[1];
my $bossDamage = (split /: /, $lines[1])[1];

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

sub minManaToWin {
    my %state = %{$_[0]};
    my $minMana = ~0 >> 1;
    
    my $simulate;
    $simulate = sub {
        my %state = %{$_[0]};
        my $playerTurn = $_[1];
        
        if ($state{manaSpent} >= $minMana) {
            return;
        }
        if ($state{bossHP} <= 0) {
            $minMana = $state{manaSpent};
            return;
        }
        if ($state{playerHP} <= 0) {
            return;
        }
        
        if ($playerTurn) {
            $state{playerHP}--;
            if ($state{playerHP} <= 0) {
                return;
            }
        }
        
        if ($state{shieldTimer} > 0) {
            $state{shieldTimer}--;
        }
        if ($state{poisonTimer} > 0) {
            $state{bossHP} -= 3;
            $state{poisonTimer}--;
        }
        if ($state{rechargeTimer} > 0) {
            $state{playerMana} += 101;
            $state{rechargeTimer}--;
        }
        
        if (!$playerTurn) {
            my $damage = $state{bossDamage};
            if ($state{shieldTimer} > 0) {
                $damage -= 7;
            }
            $damage = 1 if $damage < 1;
            $state{playerHP} -= $damage;
            $simulate->(\%state, 1);
            return;
        }
        
        if ($state{playerMana} >= 53) {
            my %newState = %state;
            $newState{playerMana} -= 53;
            $newState{manaSpent} += 53;
            $newState{bossHP} -= 4;
            $simulate->(\%newState, 0);
        }
        if ($state{playerMana} >= 73) {
            my %newState = %state;
            $newState{playerMana} -= 73;
            $newState{manaSpent} += 73;
            $newState{bossHP} -= 2;
            $newState{playerHP} += 2;
            $simulate->(\%newState, 0);
        }
        if ($state{playerMana} >= 113 && $state{shieldTimer} == 0) {
            my %newState = %state;
            $newState{playerMana} -= 113;
            $newState{manaSpent} += 113;
            $newState{shieldTimer} = 6;
            $simulate->(\%newState, 0);
        }
        if ($state{playerMana} >= 173 && $state{poisonTimer} == 0) {
            my %newState = %state;
            $newState{playerMana} -= 173;
            $newState{manaSpent} += 173;
            $newState{poisonTimer} = 6;
            $simulate->(\%newState, 0);
        }
        if ($state{playerMana} >= 229 && $state{rechargeTimer} == 0) {
            my %newState = %state;
            $newState{playerMana} -= 229;
            $newState{manaSpent} += 229;
            $newState{rechargeTimer} = 5;
            $simulate->(\%newState, 0);
        }
    };

    $initialState{playerHP} = 50;
    $initialState{playerMana} = 500;
    $simulate->(\%initialState, 1);
    return $minMana;
}

print minManaToWin(\%initialState);
