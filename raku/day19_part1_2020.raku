
my %rules;
my %cache;

sub rule-pattern($n) {
    return %cache{$n} if %cache{$n}:exists;
    my $rule = %rules{$n};
    die "Rule $n not found" unless $rule.defined;
    my $pat = $rule ~~ /^ '"' (<-["]>+) '"' $/ ?? $0.Str
               !! '(' ~ (do for $rule.split('|') -> $or {
                   [~] $or.words.map: {rule-pattern($_)}
               }).join('|') ~ ')';
    %cache{$n} = $pat;
}

sub MAIN() {
    my $fh = 'input.txt'.IO.open orelse die $!;
    my @messages;
    my $rules = True;
    for $fh.lines {
        when .chars == 0 { $rules = False; next }
        if $rules {
            my ($k,$v) = .split(': ');
            %rules{$k} = $v;
        } else {
            @messages.push: $_;
        }
    }
    my $rx = / ^ <{ rule-pattern('0') }> $ /;
    say +@messages.grep: { $_ ~~ $rx }
}
