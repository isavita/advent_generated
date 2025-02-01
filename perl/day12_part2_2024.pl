
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(sum);

my @move = (
    { label => "left", x => -1, y => 0 },
    { label => "up", x => 0, y => -1 },
    { label => "right", x => 1, y => 0 },
    { label => "down", x => 0, y => 1 },
);

my @graph;
open(my $fh, "<", "input.txt") or die "Cannot open input.txt: $!";
while (my $line = <$fh>) {
    chomp $line;
    next if $line eq '';
    push @graph, [ split //, $line ];
}
close $fh;

my $H = @graph;
my $W = @{$graph[0]};

my $sum = 0;

for my $y (0 .. $H - 1) {
    for my $x (0 .. $W - 1) {
        next if $graph[$y][$x] eq ".";
        my $area = 0;
        my $target = $graph[$y][$x];
        my %visited;
        my %side;
        my $search;

        $search = sub {
            my ($cx, $cy, $label) = @_;
            if ($graph[$cy][$cx] ne $target) {
                if ($label ne "" && !$visited{"$cx,$cy"}) {
                  saveOuter($label, \%side, $cx, $cy);
                }
              return;
            }
            $visited{"$cx,$cy"} = 1;
            $area++;
            $graph[$cy][$cx] = ".";

             for my $m (@move) {
                my $nx = $cx + $m->{x};
                my $ny = $cy + $m->{y};

                if ($nx < 0 || $nx >= $W || $ny < 0 || $ny >= $H) {
                   saveOuter($m->{label}, \%side, $nx, $ny);
                   next;
                }
                $search->($nx, $ny, $m->{label});
            }
        };
        $search->($x, $y, "");
        my $outer = countOuter(\%side);
        $sum += $area * $outer;
    }
}
print "$sum\n";

sub saveOuter {
    my ($label, $side, $x, $y) = @_;
    my $key;
    if ($label eq "up" || $label eq "down") {
        $key = "$y:$x";
    } else {
        $key = "$x:$y";
    }
    $side->{$label} //= {};
    $side->{$label}{$key} = 1;
}

sub countOuter {
    my ($side) = @_;
    my $outer = 0;
     for my $label (keys %$side) {
        my @array = sort {
          my ($a1, $a2) = split(":", $a);
          my ($b1, $b2) = split(":", $b);
          $a1 <=> $b1 or $a2 <=> $b2;
        } keys %{$side->{$label}};

       my @temp;
        for my $current (@array) {
          my ($i, $j) = split(":", $current);
          unless(check(\@temp, $i, $j)){
             $outer++;
          }
          push @temp, $current;
        }
     }
     return $outer;
}

sub check {
  my ($ary, $i, $j) = @_;
  my @search = (
    "$i:" . ($j-1),
    "$i:" . ($j+1),
  );
  for my $s (@search) {
    for my $a (@$ary) {
      return 1 if $a eq $s;
    }
  }
  return 0;
}
