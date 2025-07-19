
function day15()
%----------------------------------------------------------
% Read input ------------------------------------------------
txt   = fileread('input.txt');
nums  = regexp(txt,'-?\d+','match');
nums  = str2double(nums);
S     = reshape(nums,4,[])';          % [sx sy bx by]

%----------------------------------------------------------
% Part 1 ---------------------------------------------------
yRow  = 2e6;
fprintf('Part 1: %d\n', countImpossible(S,yRow));

%----------------------------------------------------------
% Part 2 ---------------------------------------------------
lim   = 4e6;
[x,y] = findGap(S,lim);
fprintf('Part 2: %.0f\n', x*4e6 + y);
end
%----------------------------------------------------------
function n = countImpossible(S,yRow)
sx = S(:,1); sy = S(:,2); bx = S(:,3); by = S(:,4);
d  = abs(sx-bx) + abs(sy-by);          % Manhattan radius
dy = abs(sy - yRow);
mask = dy <= d;                        % sensors that reach yRow
if ~any(mask), n = 0; return, end

sx   = sx(mask);  d = d(mask);  dy = dy(mask);
L    = sx - (d - dy);                % leftmost  covered x
R    = sx + (d - dy);                % rightmost covered x
ints = [L(:) R(:)];
ints = sortrows(ints,1);

% merge overlapping intervals
merged = ints(1,:);
for k = 2:size(ints,1)
    if ints(k,1) <= merged(end,2)+1
        merged(end,2) = max(merged(end,2), ints(k,2));
    else
        merged(end+1,:) = ints(k,:);
    end
end
n = sum(diff(merged))+numel(merged);   % total covered positions

% subtract beacons that sit exactly on the row
n = n - sum(by == yRow & bx >= min(merged(:,1)) & bx <= max(merged(:,2)));
end
%----------------------------------------------------------
function [x,y] = findGap(S,lim)
sx = S(:,1); sy = S(:,2); bx = S(:,3); by = S(:,4);
d  = abs(sx-bx) + abs(sy-by);

% For every sensor build the four lines that form the diamond border
% (x ± (d+1), y) and (x, y ± (d+1))
% We only need to check points that are one unit outside these borders.
% Collect all candidate lines and intersect them.
lines = [];
for k = 1:numel(d)
    r = d(k)+1;
    lines = [lines; ...
             sy(k)-sx(k)-r;   % slope -1, c = y+x
             sy(k)-sx(k)+r;   % slope -1
             sy(k)+sx(k)-r;   % slope +1, c = y-x
             sy(k)+sx(k)+r];  % slope +1
end
lines = unique(lines);

% Build every intersection of lines with slopes +1 and -1
% (x,y) = ((c2-c1)/2 , (c1+c2)/2)
c1 = lines; c2 = lines.';
X  = (c2 - c1)/2;
Y  = (c1 + c2)/2;
keep = (X==floor(X)) & (Y==floor(Y)) & ...
       X>=0 & X<=lim & Y>=0 & Y<=lim;
X = X(keep);  Y = Y(keep);

% Vectorised test: for every candidate (x,y) check that
% |x-sx|+|y-sy| > d for every sensor
xy = [X(:) Y(:)];
inside = false(size(xy,1),1);
for k = 1:numel(d)
    inside = inside | (abs(xy(:,1)-sx(k)) + abs(xy(:,2)-sy(k))) <= d(k);
end
xy = xy(~inside,:);
x  = xy(1,1);
y  = xy(1,2);
end
