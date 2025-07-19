
function main
    % Read input
    fid = fopen('input.txt','r');
    if fid<0, error('Cannot open input.txt'); end
    
    workflows = containers.Map;
    while true
        line = fgetl(fid);
        if isempty(line) || line==-1, break; end
        
        brace = strfind(line,'{');
        name = line(1:brace-1);
        rulesStr = line(brace+1:end-1);
        
        rules = {};
        parts = strsplit(rulesStr,',');
        for k = 1:numel(parts)
            p = parts{k};
            colon = strfind(p,':');
            if isempty(colon)
                rule.var = 0;
                rule.op = 0;
                rule.val = 0;
                rule.dest = p;
            else
                rule.var = p(1);
                rule.op = p(2);
                rule.val = str2double(p(3:colon-1));
                rule.dest = p(colon+1:end);
            end
            rules{end+1} = rule;
        end
        workflows(name) = rules;
    end
    fclose(fid);
    
    % Memoization
    persistent memo
    if isempty(memo)
        memo = containers.Map('KeyType','char','ValueType','int64');
    end
    
    % Solve
    initial = repmat(struct('min',1,'max',4000),4,1);
    total = process('in',initial,workflows,memo);
    fprintf('%d\n',total);
end

function res = process(wf,cons,workflows,memo)
    key = sprintf('%s_%d_%d_%d_%d_%d_%d_%d_%d',wf,...
        cons(1).min,cons(1).max,...
        cons(2).min,cons(2).max,...
        cons(3).min,cons(3).max,...
        cons(4).min,cons(4).max);
    if isKey(memo,key)
        res = memo(key);
        return
    end
    
    if any([cons.min]>[cons.max])
        res = 0;
        return
    end
    
    if strcmp(wf,'R')
        res = 0;
        return
    end
    if strcmp(wf,'A')
        res = int64(1);
        for k=1:4
            res = res*int64(cons(k).max-cons(k).min+1);
        end
        return
    end
    
    rules = workflows(wf);
    current = cons;
    res = int64(0);
    
    for k=1:numel(rules)
        rule = rules{k};
        if rule.var==0
            res = res + process(rule.dest,current,workflows,memo);
            break
        end
        
        idx = find(['xmas']==rule.var);
        R = current(idx);
        
        if rule.op=='<'
            trueR = struct('min',R.min,'max',min(rule.val-1,R.max));
            falseR = struct('min',max(rule.val,R.min),'max',R.max);
        else % '>'
            trueR = struct('min',max(rule.val+1,R.min),'max',R.max);
            falseR = struct('min',R.min,'max',min(rule.val,R.max));
        end
        
        if trueR.min<=trueR.max
            next = current;
            next(idx) = trueR;
            res = res + process(rule.dest,next,workflows,memo);
        end
        
        if falseR.min<=falseR.max
            current(idx) = falseR;
        else
            break
        end
    end
    
    memo(key) = res;
end
