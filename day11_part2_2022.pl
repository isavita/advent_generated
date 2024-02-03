
use strict;
use warnings;
use List::Util qw(reduce);

sub parse {
    my $s = shift;
    my $m = {};
    my @lines = split("\n", $s);
    foreach my $item (split(", ", (split(": ", $lines[1]))[1])) {
        push @{$m->{items}}, int(toInt($item));
    }
    my @f = split(" ", (split("= ", $lines[2]))[1]);
    if ($f[1] eq "+") {
        if ($f[2] eq "old") {
            $m->{operation} = sub { return $_[0] + $_[0] };
        } else {
            $m->{operation} = sub { return $_[0] + toInt($f[2]) };
        }
    } elsif ($f[1] eq "*") {
        if ($f[2] eq "old") {
            $m->{operation} = sub { return $_[0] * $_[0] };
        } else {
            $m->{operation} = sub { return $_[0] * toInt($f[2]) };
        }
    }
    $lines[3] =~ /Test: divisible by (\d+)/;
    $m->{div} = $1;
    $lines[4] =~ /If true: throw to monkey (\d+)/;
    $m->{next}[0] = $1;
    $lines[5] =~ /If false: throw to monkey (\d+)/;
    $m->{next}[1] = $1;
    return $m;
}

sub monkeyBusiness {
    my ($monkeys, $rounds, $worry) = @_;
    my $div = reduce { $a * $b } 1, map { $_->{div} } @{$monkeys};

    for (my $i = 0; $i < $rounds; $i++) {
        foreach my $m (@{$monkeys}) {
            while (scalar @{$m->{items}} > 0) {
                $m->{inspections}++;
                my $item = $m->{operation}->($m->{items}[0]);
                if ($worry) {
                    $item %= $div;
                } else {
                    $item /= 3;
                }
                if ($item % $m->{div} == 0) {
                    push @{$monkeys->[$m->{next}[0]]{items}}, $item;
                } else {
                    push @{$monkeys->[$m->{next}[1]]{items}}, $item;
                }
                shift @{$m->{items}};
            }
        }
    }
    my @inspections = ();
    foreach my $m (@{$monkeys}) {
        push @inspections, $m->{inspections};
    }
    @inspections = sort { $b <=> $a } @inspections;
    return $inspections[0] * $inspections[1];
}

sub readAll {
    my $path = shift;
    open my $fh, '<', $path or die "Cannot open file: $!";
    local $/;
    my $content = <$fh>;
    close $fh;
    return $content;
}

sub toInt {
    my $s = shift;
    return int($s);
}

my @monkeys;
my $s = readAll("input.txt");
foreach my $m (split("\n\n", $s)) {
    push @monkeys, parse($m);
}

for (my $i = 0; $i < scalar @monkeys; $i++) {
    $monkeys[$i] = parse((split("\n\n", $s))[$i]);
}
print monkeyBusiness(\@monkeys, 10000, 1) . "\n";
