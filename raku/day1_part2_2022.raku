
sub MAIN() {
    my $content = 'input.txt'.IO.slurp;

    my @elf-calories = $content.split(/\n\n+/).map({
        .lines.map(*.Int).sum
    });

    my $result = @elf-calories.sort({ $^b <=> $^a })[^3].sum;

    say $result;
}
