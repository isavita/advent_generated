program solution;

var
    inputFile: text;
    line: string;
    groupAnswers: set of char;
    totalCount: integer;
    question: char;

begin
    assign(inputFile, 'input.txt');
    reset(inputFile);

    totalCount := 0;
    groupAnswers := [];

    while not eof(inputFile) do
    begin
        readln(inputFile, line);
        if line = '' then
        begin
            totalCount := totalCount + Length(groupAnswers);
            groupAnswers := [];
        end
        else
        begin
            for question in line do
            begin
                groupAnswers := groupAnswers + [question];
            end;
        end;
    end;

    totalCount := totalCount + Length(groupAnswers);
    writeln(totalCount);

    close(inputFile);
end.