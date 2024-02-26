
{
    split($0, dims, "x");
    l = dims[1];
    w = dims[2];
    h = dims[3];
    
    area = 2*l*w + 2*w*h + 2*h*l;
    
    slack = l*w;
    if (w*h < slack) {
        slack = w*h;
    }
    if (h*l < slack) {
        slack = h*l;
    }
    
    total_area += area + slack;
    
    perimeter = 2*l + 2*w + 2*h - 2*max(l, w, h);
    
    volume = l*w*h;
    
    total_ribbon += perimeter + volume;
}

END {
    print total_area;
    print total_ribbon;
}

function max(a, b, c) {
    if (a >= b && a >= c) {
        return a;
    } else if (b >= a && b >= c) {
        return b;
    } else {
        return c;
    }
}
