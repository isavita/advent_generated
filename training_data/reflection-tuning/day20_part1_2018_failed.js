function findFurthestRoom(regex) {
    let x = 0, y = 0;
    let maxDist = 0;
    let stack = [];
    
    for (let char of regex) {
        switch(char) {
            case 'N':
                y++; break;
            case 'S':
                y--; break;
            case 'E':
                x++; break;
            case 'W':
                x--; break;
            case '(':
                stack.push([x, y]);
                break;
            case ')':
                if (stack.length > 0) {
                    [x, y] = stack.pop();
                }
                break;
            case '|':
                if (stack.length > 0) {
                    [x, y] = stack[stack.length - 1];
                }
                break;
        }
        maxDist = Math.max(maxDist, Math.abs(x) + Math.abs(y));
    }
    
    return maxDist;
}

// Test cases
console.log(findFurthestRoom('^WNE$')); // Should output 3
console.log(findFurthestRoom('^ENWWW(NEEE|SSE(EE|N))$')); // Should output 10
console.log(findFurthestRoom('^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$')); // Should output 18
console.log(findFurthestRoom('^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$')); // Should output 23
console.log(findFurthestRoom('^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$')); // Should output 31
