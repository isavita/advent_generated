
open(my $fh, '<', 'input.txt') or die "Error reading file: $!";
my $moves = <$fh>;
close($fh);

my @moves = split(',', $moves);
my @programs = ('a'..'p');

foreach my $move (@moves) {
    if ($move =~ /^s(\d+)$/) {
        my $x = $1;
        spin(\@programs, $x);
    } elsif ($move =~ /^x(\d+)\/(\d+)$/) {
        my ($A, $B) = ($1, $2);
        exchange(\@programs, $A, $B);
    } elsif ($move =~ /^p(\w)\/(\w)$/) {
        my ($A, $B) = ($1, $2);
        partner(\@programs, $A, $B);
    }
}

print join('', @programs) . "\n";

sub spin {
    my ($programs, $x) = @_;
    my @temp = @$programs;

    for (my $i = 0; $i < scalar(@$programs); $i++) {
        $programs->[($i+$x)%@$programs] = $temp[$i];
    }
}

sub exchange {
    my ($programs, $A, $B) = @_;
    ($programs->[$A], $programs->[$B]) = ($programs->[$B], $programs->[$A]);
}

sub partner {
    my ($programs, $A, $B) = @_;
    my ($indexA, $indexB);

    foreach my $i (0..$#{$programs}) {
        $indexA = $i if $programs->[$i] eq $A;
        $indexB = $i if $programs->[$i] eq $B;
    }

    exchange($programs, $indexA, $indexB);
}
