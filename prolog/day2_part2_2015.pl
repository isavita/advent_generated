:- initialization(main).

main :-
    open('input.txt', read, Stream),
    read_dimensions(Stream, TotalPaper, TotalRibbon),
    close(Stream),
    format('Total wrapping paper needed: ~d~n', [TotalPaper]),
    format('Total ribbon needed: ~d~n', [TotalRibbon]).

read_dimensions(Stream, TotalPaper, TotalRibbon) :-
    read_line_to_string(Stream, Line),
    ( Line \= end_of_file ->
        split_string(Line, "x", "", [LStr, WStr, HStr]),
        number_string(L, LStr),
        number_string(W, WStr),
        number_string(H, HStr),
        SurfaceArea is 2 * (L * W + W * H + H * L),
        Slack is min(L * W, min(W * H, H * L)),
        Paper is SurfaceArea + Slack,
        Perimeter is 2 * (min(L + W, min(W + H, H + L))),
        Volume is L * W * H,
        Ribbon is Perimeter + Volume,
        read_dimensions(Stream, PaperAcc, RibbonAcc),
        TotalPaper is Paper + PaperAcc,
        TotalRibbon is Ribbon + RibbonAcc
    ; 
        TotalPaper = 0,
        TotalRibbon = 0
    ).