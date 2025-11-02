#!/usr/bin/env raku

sub MAIN {
    my $file = 'input.txt'.IO;
    my $a;
    my $b;
    my $c;
    my @program;

    for $file.lines -> $line {
        my ($key, $value) = $line.split(':').map(*.trim);
        next unless $value;
        given $key {
            when 'Register A' { $a = +$value }
            when 'Register B' { $b = +$value }
            when 'Register C' { $c = +$value }
            when 'Program'    { @program = $value.split(',').map(*.trim)>>.Int }
        }
    }

    sub first-output($A, $B, $C) {
        my $aa = $A;
        my $bb = $B;
        my $cc = $C;
        my $ip = 0;
        while $ip < @program {
            my $op = @program[$ip];
            my $operand = @program[$ip+1];
            my $combo = do given $operand {
                when 0|1|2|3 { $_ }
                when 4 { $aa }
                when 5 { $bb }
                when 6 { $cc }
                default { die "Invalid combo operand $operand" }
            };
            given $op {
                when 0 { $aa = $aa +> $combo; }
                when 1 { $bb = $bb +^ $operand; }
                when 2 { $bb = $combo % 8; }
                when 3 { if $aa != 0 { $ip = $operand - 2; } }
                when 4 { $bb = $bb +^ $cc; }
                when 5 { return $combo % 8; }
                when 6 { $bb = $aa +> $combo; }
                when 7 { $cc = $aa +> $combo; }
                default { die "Invalid opcode $op"; }
            }
            $ip += 2;
        }
        return Nil;
    }

    my @answers;
    my %seen;

    sub dfs($depth, $score) {
        my $k = "$depth,$score";
        return if %seen{$k}++;
        if $depth == @program.elems {
            @answers.push($score);
            return;
        }
        my $expected = @program[@program.elems - 1 - $depth];
        for ^8 -> $i {
            my $newScore = $i + 8 * $score;
            my $out = first-output($newScore, $b, $c);
            if defined($out) && $out == $expected {
                dfs($depth+1, $newScore);
            }
        }
    }

    dfs(0, 0);

    if @answers {
        say @answers.min;
    } else {
        say "No valid values found";
    }
}