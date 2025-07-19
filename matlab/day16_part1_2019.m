
function main
% Read the raw input string
str = fileread('input.txt');
str = str(~isspace(str));          % strip newlines / spaces
sig = str - '0';                  % char -> numeric vector

% Run 100 FFT phases
sig = fftPhases(sig, 100);

% Output the first eight digits
fprintf('%s\n', sprintf('%d', sig(1:8)));
end

function out = fftPhases(sig, nPhases)
len = numel(sig);
out = sig;
pat = [0 1 0 -1];                 % base pattern

for p = 1:nPhases
    new = zeros(size(out));
    for k = 1:len
        % Build the repeating pattern for this output position
        reps = k;
        % The pattern is (pat repeated reps times) repeated to length len+1,
        % then skip the first element.
        patIdx = mod(0:len, 4*reps) + 1;          % 1-based indexing
        mask   = pat(ceil(patIdx / reps));
        mask   = mask(2:end);                     % skip first element
        
        % Element-wise multiply and sum
        new(k) = mod(abs(sum(out .* mask)), 10);
    end
    out = new;
end
end
