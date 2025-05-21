sub get_value(@data, $mode, $param) { $mode == 0 ?? @data[$param] !! $param }

sub run_program(@data, $input-val) {
    my $i = 0;
    my $diagnostic-code;

    while $i < @data.elems {
        my $instruction = @data[$i];
        my $opcode = $instruction % 100;
        my $mode1 = ($instruction div 100) % 10;
        my $mode2 = ($instruction div 1000) % 10;
        my $mode3 = ($instruction div 10000) % 10;

        given $opcode {
            when 1 {
                my $val1 = get_value(@data, $mode1, @data[$i+1]);
                my $val2 = get_value(@data, $mode2, @data[$i+2]);
                @data[@data[$i+3]] = $val1 + $val2;
                $i += 4;
            }
            when 2 {
                my $val1 = get_value(@data, $mode1, @data[$i+1]);
                my $val2 = get_value(@data, $mode2, @data[$i+2]);
                @data[@data[$i+3]] = $val1 * $val2;
                $i += 4;
            }
            when 3 {
                @data[@data[$i+1]] = $input-val;
                $i += 2;
            }
            when 4 {
                $diagnostic-code = get_value(@data, $mode1, @data[$i+1]);
                $i += 2;
            }
            when 99 {
                last;
            }
            default {
                $i += 1;
            }
        }
    }
    return $diagnostic-code;
}

sub MAIN() {
    my @data = 'input.txt'.IO.slurp.trim.split(',').map(*.Int);
    my $result = run_program(@data, 1);
    say $result;
}