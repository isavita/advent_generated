
function main()
    fid = fopen('input.txt', 'r');
    if fid == -1
        error('Could not open input.txt');
    end
    initialSequence = fgetl(fid);
    fclose(fid);

    initialSequence = strtrim(initialSequence);

    if isstring(initialSequence)
        initialSequence = char(initialSequence);
    end

    iterations = 50;
    currentSequence = initialSequence;

    for i = 1:iterations
        currentSequence = nextSequence(currentSequence);
    end

    fprintf('%d\n', length(currentSequence));
end

function nextSeq = nextSequence(seq)
    if isempty(seq)
        nextSeq = '';
        return;
    end

    nextSeqParts = cell(1, length(seq));
    partIdx = 1;
    idx = 1;
    len = length(seq);

    while idx <= len
        currentChar = seq(idx);
        count = 0;
        while idx <= len && seq(idx) == currentChar
            count = count + 1;
            idx = idx + 1;
        end
        nextSeqParts{partIdx} = sprintf('%d%c', count, currentChar);
        partIdx = partIdx + 1;
    end
    nextSeq = [nextSeqParts{1:partIdx-1}];
end
