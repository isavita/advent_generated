
sub can-reach(Int $target, @nums) {
    my $num-ops = @nums.elems - 1;
    return Nil if $num-ops < 0;

    for 0 .. 2**$num-ops - 1 -> $ops_code_base {
        my $current-ops_code = $ops_code_base;
        my $result = @nums[0];

        for 0 .. $num-ops - 1 -> $i {
            my $op = ($current-ops_code % 2 == 0) ?? '+' !! '*';
            $result = do given $op {
                when '+' { $result + @nums[$i + 1] }
                when '*' { $result * @nums[$i + 1] }
            };
            $current-ops_code div= 2;
        }

        return $target if $result == $target;
    }

    return Nil;
}

sub MAIN() {
    my @test-cases = 'input.txt'.IO.slurp.lines.map({
        my ($target_str, $nums_str) = $_.split(': ');
        ($target_str.Int, $nums_str.split(' ').map({ $_.Int }));
    });

    my $total-sum = @test-cases.map({
        can-reach($_[0], $_[1]);
    }).grep(*.defined).sum;

    say $total-sum;
}
