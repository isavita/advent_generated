
sub MAIN() {
    my $fh = open 'input.txt', :r or die "Cannot open input.txt: $!";
    my @holders;
    my %held;
    
    for $fh.lines -> $line {
        my @tokens = $line.split(/<[ ,]>+/);
        next unless @tokens;
        @holders.push(@tokens[0]);
        %held{@tokens[1..*]} = True if @tokens > 1;
    }
    
    for @holders -> $holder {
        say $holder unless %held{$holder};
    }
}
