
BEGIN {
    ARGV[1] = "input.txt"
    ARGC = 2
}
{
    gsub(/[^-0-9]+/, " ")
    if (NF < 4) next
    n++
    X[n]=$1; Y[n]=$2; Z[n]=$3; R[n]=$4
}
END {
    maxR = -1
    for (i = 1; i <= n; i++) {
        if (R[i] > maxR) {
            maxR = R[i]; mX = X[i]; mY = Y[i]; mZ = Z[i]
        }
    }
    c1 = 0
    for (i = 1; i <= n; i++) {
        if (abs(X[i]-mX) + abs(Y[i]-mY) + abs(Z[i]-mZ) <= maxR) c1++
    }
    print c1

    minX = X[1]-R[1]; maxX = X[1]+R[1]
    minY = Y[1]-R[1]; maxY = Y[1]+R[1]
    minZ = Z[1]-R[1]; maxZ = Z[1]+R[1]
    for (i = 2; i <= n; i++) {
        if (X[i]-R[i] < minX) minX = X[i]-R[i]
        if (X[i]+R[i] > maxX) maxX = X[i]+R[i]
        if (Y[i]-R[i] < minY) minY = Y[i]-R[i]
        if (Y[i]+R[i] > maxY) maxY = Y[i]+R[i]
        if (Z[i]-R[i] < minZ) minZ = Z[i]-R[i]
        if (Z[i]+R[i] > maxZ) maxZ = Z[i]+R[i]
    }
    side = maxX - minX
    if (maxY - minY > side) side = maxY - minY
    if (maxZ - minZ > side) side = maxZ - minZ
    sz = 1; while (sz < side) sz *= 2

    c_cnt = 0
    for (k = 1; k <= n; k++) if (dist_to_cube(minX, minY, minZ, sz, k) <= R[k]) c_cnt++
    push(c_cnt, min_origin_dist(minX, minY, minZ, sz), sz, minX, minY, minZ)

    while (hl > 0) {
        pop()
        if (rs == 1) {
            print rd
            break
        }
        nsz = rs / 2
        for (nx = rx; nx <= rx + nsz; nx += nsz) {
            for (ny = ry; ny <= ry + nsz; ny += nsz) {
                for (nz = rz; nz <= rz + nsz; nz += nsz) {
                    c_cnt = 0
                    for (k = 1; k <= n; k++) {
                        if (dist_to_cube(nx, ny, nz, nsz, k) <= R[k]) c_cnt++
                    }
                    push(c_cnt, min_origin_dist(nx, ny, nz, nsz), nsz, nx, ny, nz)
                }
            }
        }
    }
}
function abs(v) { return v < 0 ? -v : v }
function min_origin_dist(nx, ny, nz, nsz,    dx, dy, dz) {
    dx = nx > 0 ? nx : (nx + nsz - 1 < 0 ? -(nx + nsz - 1) : 0)
    dy = ny > 0 ? ny : (ny + nsz - 1 < 0 ? -(ny + nsz - 1) : 0)
    dz = nz > 0 ? nz : (nz + nsz - 1 < 0 ? -(nz + nsz - 1) : 0)
    return dx + dy + dz
}
function dist_to_cube(nx, ny, nz, nsz, k,    d, dx, dy, dz) {
    dx = 0; if (X[k] < nx) dx = nx - X[k]; else if (X[k] > nx + nsz - 1) dx = X[k] - (nx + nsz - 1)
    dy = 0; if (Y[k] < ny) dy = ny - Y[k]; else if (Y[k] > ny + nsz - 1) dy = Y[k] - (ny + nsz - 1)
    dz = 0; if (Z[k] < nz) dz = nz - Z[k]; else if (Z[k] > nz + nsz - 1) dz = Z[k] - (nz + nsz - 1)
    return dx + dy + dz
}
function push(c, d, s, x, y, z,   i, p) {
    hl++; hc[hl]=c; hd[hl]=d; hs[hl]=s; hx[hl]=x; hy[hl]=y; hz[hl]=z
    i = hl
    while (i > 1) {
        p = int(i/2)
        if (better(i, p)) { swap(i, p); i = p } else break
    }
}
function pop(   p, l, r, s) {
    rx=hx[1]; ry=hy[1]; rz=hz[1]; rs=hs[1]; rc=hc[1]; rd=hd[1]
    hx[1]=hx[hl]; hy[1]=hy[hl]; hz[1]=hz[hl]; hs[1]=hs[hl]; hc[1]=hc[hl]; hd[1]=hd[hl]
    hl--; p = 1
    while (1) {
        l = 2 * p; r = 2 * p + 1; s = p
        if (l <= hl && better(l, s)) s = l
        if (r <= hl && better(r, s)) s = r
        if (s != p) { swap(p, s); p = s } else break
    }
}
function better(a, b) {
    if (hc[a] != hc[b]) return hc[a] > hc[b]
    if (hd[a] != hd[b]) return hd[a] < hd[b]
    return hs[a] > hs[b]
}
function swap(a, b,   t) {
    t=hc[a]; hc[a]=hc[b]; hc[b]=t; t=hd[a]; hd[a]=hd[b]; hd[b]=t
    t=hs[a]; hs[a]=hs[b]; hs[b]=t; t=hx[a]; hx[a]=hx[b]; hx[b]=t
    t=hy[a]; hy[a]=hy[b]; hy[b]=t; t=hz[a]; hz[a]=hz[b]; hz[b]=t
}
