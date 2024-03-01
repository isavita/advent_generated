program Day12;

var
    input: Text;
    action: char;
    value: integer;
    direction: char;
    x, y: integer;

begin
    assign(input, 'input.txt');
    reset(input);

    direction := 'E';
    x := 0;
    y := 0;

    while not eof(input) do
    begin
        readln(input, action, value);

        case action of
            'N': y := y + value;
            'S': y := y - value;
            'E': x := x + value;
            'W': x := x - value;
            'L': begin
                    value := value div 90;
                    case value of
                        1: begin
                                case direction of
                                    'N': direction := 'W';
                                    'S': direction := 'E';
                                    'E': direction := 'N';
                                    'W': direction := 'S';
                                end;
                           end;
                        2: begin
                                case direction of
                                    'N': direction := 'S';
                                    'S': direction := 'N';
                                    'E': direction := 'W';
                                    'W': direction := 'E';
                                end;
                           end;
                        3: begin
                                case direction of
                                    'N': direction := 'E';
                                    'S': direction := 'W';
                                    'E': direction := 'S';
                                    'W': direction := 'N';
                                end;
                           end;
                    end;
                end;
            'R': begin
                    value := value div 90;
                    case value of
                        1: begin
                                case direction of
                                    'N': direction := 'E';
                                    'S': direction := 'W';
                                    'E': direction := 'S';
                                    'W': direction := 'N';
                                end;
                           end;
                        2: begin
                                case direction of
                                    'N': direction := 'S';
                                    'S': direction := 'N';
                                    'E': direction := 'W';
                                    'W': direction := 'E';
                                end;
                           end;
                        3: begin
                                case direction of
                                    'N': direction := 'W';
                                    'S': direction := 'E';
                                    'E': direction := 'N';
                                    'W': direction := 'S';
                                end;
                           end;
                    end;
                end;
            'F': begin
                    case direction of
                        'N': y := y + value;
                        'S': y := y - value;
                        'E': x := x + value;
                        'W': x := x - value;
                    end;
                end;
        end;
    end;

    writeln(abs(x) + abs(y));

    close(input);
end.