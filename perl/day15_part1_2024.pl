
#!/usr/bin/perl
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die "Can't open input.txt: $!";
my @grid;
my $moves = "";
my $reading_map = 1;

while (my $line = <$fh>) {
    chomp $line;
    if ($reading_map) {
        if ($line =~ /#/) {
            push @grid, $line;
        } else {
            $reading_map = 0;
            $moves .= $line;
        }
    } else {
        $moves .= $line;
    }
}
close $fh;

my @runes;
for my $row (@grid) {
    push @runes, [ split //, $row ];
}

my ($robot_r, $robot_c);
for my $r (0..$#runes) {
    for my $c (0..$#{$runes[$r]}) {
        if ($runes[$r][$c] eq '@') {
            ($robot_r, $robot_c) = ($r, $c);
            last;
        }
    }
}

my %dirs = (
    '^' => [-1, 0],
    'v' => [1, 0],
    '<' => [0, -1],
    '>' => [0, 1],
);

sub push_boxes {
    my ($runes, $r, $c, $dr, $dc) = @_;
    my ($nr, $nc) = ($r + $dr, $c + $dc);
    return 0 if $runes->[$nr][$nc] eq '#';
    if ($runes->[$nr][$nc] eq 'O') {
        return 0 unless push_boxes($runes, $nr, $nc, $dr, $dc);
    }
    if ($runes->[$nr][$nc] eq '.') {
        $runes->[$nr][$nc] = 'O';
        $runes->[$r][$c] = '.';
        return 1;
    }
    return 0;
}

for my $move (split //, $moves) {
    my ($dr, $dc) = @{$dirs{$move}};
    my ($nr, $nc) = ($robot_r + $dr, $robot_c + $dc);
    next if $runes[$nr][$nc] eq '#';
    if ($runes[$nr][$nc] eq 'O') {
        next unless push_boxes(\@runes, $nr, $nc, $dr, $dc);
    }
    if ($runes[$nr][$nc] eq '.' || $runes[$nr][$nc] eq 'O') {
        ($runes[$robot_r][$robot_c], $runes[$nr][$nc]) = ('.', '@');
        ($robot_r, $robot_c) = ($nr, $nc);
    }
}

my $sum = 0;
for my $r (0..$#runes) {
    for my $c (0..$#{$runes[$r]}) {
        if ($runes[$r][$c] eq 'O') {
            $sum += $r * 100 + $c;
        }
    }
}

print "$sum\n";
