
#!/usr/bin/perl
use strict;
use warnings;

my $offset = 10000000000000;
my @machines = read_input("input.txt");
foreach my $m (@machines) {
    $m->{px} += $offset;
    $m->{py} += $offset;
}

my @results;
foreach my $m (@machines) {
    my $cost = solve_machine($m);
    push @results, $cost if $cost >= 0;
}

if (@results == 0) {
    print "0 0\n";
} else {
    my $count = @results;
    my $sum = 0;
    $sum += $_ for @results;
    print "$count $sum\n";
}

sub read_input {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Could not open file '$filename': $!";
    my @machines;
    my @lines;
    while (my $line = <$fh>) {
        chomp $line;
        $line =~ s/^\s+|\s+$//g;
        if ($line eq "") {
            if (@lines) {
                push @machines, parse_machine(@lines);
                @lines = ();
            }
        } else {
            push @lines, $line;
        }
    }
    if (@lines) {
        push @machines, parse_machine(@lines);
    }
    close $fh;
    return @machines;
}

sub parse_machine {
    my @lines = @_;
    my %m;
    foreach my $l (@lines) {
        $l =~ s/Button A:/A:/;
        $l =~ s/Button B:/B:/;
        $l =~ s/Prize:/P:/;
        if ($l =~ /^A:(.*)/) {
            ($m{ax}, $m{ay}) = parse_line($1);
        } elsif ($l =~ /^B:(.*)/) {
            ($m{bx}, $m{by}) = parse_line($1);
        } elsif ($l =~ /^P:(.*)/) {
            ($m{px}, $m{py}) = parse_prize($1);
        }
    }
    return \%m;
}

sub parse_line {
    my ($s) = @_;
    $s =~ s/^\s+|\s+$//g;
    my ($x, $y) = split ',', $s;
    return (parse_val($x), parse_val($y));
}

sub parse_prize {
    my ($s) = @_;
    $s =~ s/^\s+|\s+$//g;
    my ($x, $y) = split ',', $s;
    return (parse_val_prize($x), parse_val_prize($y));
}

sub parse_val {
    my ($s) = @_;
    $s =~ s/^\s+|\s+$//g;
    $s =~ s/^(X\+|Y\+|X=|Y=)//;
    return $s;
}

sub parse_val_prize {
    my ($s) = @_;
    $s =~ s/^\s+|\s+$//g;
    $s =~ s/^(X=|Y=)//;
    return $s;
}

sub solve_machine {
    my ($m) = @_;
    my $D = $m->{ax} * $m->{by} - $m->{ay} * $m->{bx};
    return -1 if $D == 0;
    my $numA = $m->{px} * $m->{by} - $m->{py} * $m->{bx};
    my $numB = -$m->{px} * $m->{ay} + $m->{py} * $m->{ax};
    return -1 if $numA % $D != 0 || $numB % $D != 0;
    my $a = $numA / $D;
    my $b = $numB / $D;
    return -1 if $a < 0 || $b < 0;
    return 3 * $a + $b;
}
