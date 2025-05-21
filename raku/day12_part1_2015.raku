use JSON::Tiny;

sub MAIN() {
    my $json-string = slurp 'input.txt';
    my $document = from-json($json-string);
    say sum-numbers($document);
}

multi sum-numbers(Int $data) { $data }

multi sum-numbers(List $data) { $data.map(&sum-numbers).sum }

multi sum-numbers(Hash $data) { $data.values.map(&sum-numbers).sum }

multi sum-numbers($data) { 0 }