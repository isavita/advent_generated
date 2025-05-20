
sub rect(@screen, $width, $height) {
  for 0 .. $height - 1 -> $i {
    for 0 .. $width - 1 -> $j {
      @screen[$i][$j] = '#';
    }
  }
}

sub rotate_row(@screen, $row, $by) {
  @screen[$row] = @screen[$row].rotate(-$by);
}

sub rotate_column(@screen, $col, $by) {
  my @temp_col = @screen.map: *.[$col];
  for 0 .. 5 -> $i {
    @screen[($i + $by) % 6][$col] = @temp_col[$i];
  }
}

sub apply_instruction(@screen, $instruction) {
  my @parts = $instruction.split(/\s+/);
  given @parts[0] {
    when 'rect' {
      my ($width, $height) = @parts[1].split('x').map(*.Int);
      rect(@screen, $width, $height);
    }
    when 'rotate' {
      my $idx = @parts[2].split('=')[1].Int;
      my $by = @parts[4].Int;
      given @parts[1] {
        when 'row' {
          rotate_row(@screen, $idx, $by);
        }
        when 'column' {
          rotate_column(@screen, $idx, $by);
        }
      }
    }
  }
}

sub print_screen(@screen) {
  for @screen -> @row {
    say @row.join('');
  }
}

sub MAIN() {
  my @screen = [ [ '.' xx 50 ] xx 6 ];
  for "input.txt".IO.lines() -> $instruction {
    apply_instruction(@screen, $instruction);
  }
  print_screen(@screen);
}
