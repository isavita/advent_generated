
use v6;

my %monkeys;
my %cache;

sub evaluate(Str $name) {
    return %cache{$name} if %cache{$name}:exists;
    my $job = %monkeys{$name};
    if $job ~~ /^\d+$/ {
        %cache{$name} = $job.Int;
    } else {
        my ($a, $op, $b) = $job.split(' ', 3);
        my $v1 = evaluate($a);
        my $v2 = evaluate($b);
        %cache{$name} = do given $op {
            when '+' { $v1 + $v2 }
            when '-' { $v1 - $v2 }
            when '*' { $v1 * $v2 }
            when '/' { $v1 div $v2 }
        };
    }
    %cache{$name}
}

sub MAIN() {
    for "input.txt".IO.lines -> $line {
        my ($monkey, $job) = $line.split(': ', 2);
        %monkeys{$monkey} = $job;
    }
    my $root = evaluate('root');
    say "The monkey named root will yell: $root";
}
