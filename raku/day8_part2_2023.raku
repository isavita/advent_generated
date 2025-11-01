
sub gcd(Int \a, Int \b) { b ?? gcd(b, a % b) !! a }

sub lcm(Int \a, Int \b) { (a / gcd(a, b) * b).Int }

sub parse-line(Str \line) {
    my ($head, $rest) = line.split(' = ');
    my ($left, $right) = $rest.trans('()' => '').split(', ');
    $head => ($left, $right)
}

sub MAIN {
    my @input = 'input.txt'.IO.lines;
    my $instr = @input.shift;
    @input.shift;                 # drop blank line
    my %nodes = @input.map: { parse-line($_) };

    my @curr = %nodes.keys.grep: *.ends-with('A');

    my @steps;
    for @curr -> $start {
        my $pos = $start;
        my $cnt = 0;
        my $len = $instr.chars;
        until $pos.ends-with('Z') {
            my $d = $instr.substr($cnt % $len, 1);
            $pos = $d eq 'L' ?? %nodes{$pos}[0] !! %nodes{$pos}[1];
            $cnt++;
        }
        @steps.push: $cnt
    }

    say [*] @steps.reduce: &lcm
}
