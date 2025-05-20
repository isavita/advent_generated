
use JSON::Fast;

sub sum-numbers(Any $data) {
    given $data {
        when Int { $data }
        when Positional { $data.map(&sum-numbers).sum }
        when Associative {
            if $data.values.any eq 'red' {
                0
            } else {
                $data.values.map(&sum-numbers).sum
            }
        }
        default { 0 }
    }
}

sub MAIN {
    my $json-string = 'input.txt'.IO.slurp;
    my $data = from-json($json-string);
    say sum-numbers($data);
}
