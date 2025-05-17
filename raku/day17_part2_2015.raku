
use v6;

sub MAIN {
    my @containers = 'input.txt'.IO.lines.map(*.Int);
    my $total-liters = 150;

    my $total-valid-combinations = 0;
    my $min-containers-count = Inf;
    my %valid-counts-by-size;

    for 1 .. @containers.elems -> $i {
        my @valid-combos = @containers.combinations($i).grep(*.sum == $total-liters);
        my $current-valid-count = @valid-combos.elems;

        if $current-valid-count > 0 {
            $total-valid-combinations += $current-valid-count;
            %valid-counts-by-size{$i} = $current-valid-count;

            if $i < $min-containers-count {
                $min-containers-count = $i;
            }
        }
    }

    say $total-valid-combinations;
    say %valid-counts-by-size{$min-containers-count};
}
