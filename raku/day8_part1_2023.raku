
sub MAIN() {
    my $input-text = slurp 'input.txt';
    my @lines = $input-text.lines.map: *.trim;

    my $instructions = @lines[0];
    my %desert-map;

    for @lines[2 .. *].grep(*.chars) -> $line {
        my @matches = $line.comb(/\w\w\w/);
        %desert-map{@matches[0]} = (@matches[1], @matches[2]);
    }

    my $current = "AAA";
    my $steps = 0;
    my $target = "ZZZ";

    my @instruction-chars = $instructions.comb('');

    while $current ne $target {
        my $direction = @instruction-chars[$steps % @instruction-chars.elems];

        if $direction eq 'R' {
            $current = %desert-map{$current}[1];
        } else {
            $current = %desert-map{$current}[0];
        }
        $steps++;
    }

    say $steps;
}
