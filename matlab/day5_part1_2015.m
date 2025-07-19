
function main
    str = fileread('input.txt');
    lines = regexp(str,'\r?\n','split');
    lines = lines(~cellfun('isempty',lines));
    niceStrings = 0;
    for k = 1:numel(lines)
        s = lines{k};
        vowels = sum(ismember(s,'aeiou'));
        doubleLetter = any(s(1:end-1) == s(2:end));
        bad = any([ ...
            strfind(s,'ab'); strfind(s,'cd'); ...
            strfind(s,'pq'); strfind(s,'xy')]);
        niceStrings = niceStrings + (vowels>=3 && doubleLetter && ~bad);
    end
    disp(niceStrings)
end
