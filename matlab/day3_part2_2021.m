
function main()
    fid = fopen('input.txt', 'r');
    values = textscan(fid, '%s');
    fclose(fid);
    values = values{1};

    oxygenGeneratorRating = filterValues(values, @(zeros, ones) '0'*(zeros > ones) + '1'*(zeros <= ones));
    co2ScrubberRating = filterValues(values, @(zeros, ones) '0'*(zeros <= ones) + '1'*(zeros > ones));

    oxygenGeneratorRatingInt = bin2dec(oxygenGeneratorRating);
    co2ScrubberRatingInt = bin2dec(co2ScrubberRating);

    fprintf('%d\n', oxygenGeneratorRatingInt * co2ScrubberRatingInt);
end

function result = filterValues(values, criteria)
    numBits = length(values{1});
    for i = 1:numBits
        zeros = sum(cellfun(@(x) x(i) == '0', values));
        ones = length(values) - zeros;
        keep = criteria(zeros, ones);
        values = filterByBit(values, i, keep);
        if length(values) == 1
            break;
        end
    end
    result = values{1};
end

function filtered = filterByBit(values, bitIndex, keep)
    filtered = values(cellfun(@(x) x(bitIndex) == keep, values));
end

main();
