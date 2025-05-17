
use v6;

sub MAIN {
    my @containers = 'input.txt'.IO.lines.map: *.Int;
    my $target = 150;
    my $count = @containers.combinations.grep(*.sum == $target).elems;
    say $count;
}
