#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <cctype>

using namespace std;

string add_big(const string& a, const string& b) {
    int i = (int)a.size() - 1;
    int j = (int)b.size() - 1;
    int carry = 0;
    string res;
    res.reserve(max(a.size(), b.size()) + 1);
    while (i >= 0 || j >= 0 || carry) {
        int da = (i >= 0) ? a[i] - '0' : 0;
        int db = (j >= 0) ? b[j] - '0' : 0;
        int s = da + db + carry;
        res.push_back(char('0' + (s % 10)));
        carry = s / 10;
        --i; --j;
    }
    reverse(res.begin(), res.end());
    return res;
}

string mul_big(const string& a, const string& b) {
    if (a == "0" || b == "0") return "0";
    int al = (int)a.size(), bl = (int)b.size();
    vector<int> prod(al + bl, 0);
    for (int i = al - 1; i >= 0; --i) {
        int ai = a[i] - '0';
        int carry = 0;
        for (int j = bl - 1; j >= 0; --j) {
            int bj = b[j] - '0';
            int sum = prod[i + j + 1] + ai * bj + carry;
            prod[i + j + 1] = sum % 10;
            carry = sum / 10;
        }
        prod[i] += carry;
    }
    int k = 0;
    int n = al + bl;
    while (k < n && prod[k] == 0) ++k;
    string res;
    res.reserve(n - k);
    for (; k < n; ++k) res.push_back(char('0' + prod[k]));
    if (res.empty()) return "0";
    return res;
}

void process_block(int sc, int ec, const vector<string>& lines, string& grand) {
    vector<string> nums;
    int op = 0;
    for (const string& line : lines) {
        int e = ec + 1;
        int line_len = (int)line.size();
        if (e > line_len) e = line_len;
        if (sc >= line_len) continue;
        string seg = line.substr(sc, e - sc);
        int l = 0;
        int r = (int)seg.size() - 1;
        while (l <= r && isspace((unsigned char)seg[l])) ++l;
        while (r >= l && isspace((unsigned char)seg[r])) --r;
        if (l > r) continue;
        string tmp = seg.substr(l, r - l + 1);
        if (tmp == "+") op = 1;
        else if (tmp == "*") op = 2;
        else nums.push_back(tmp);
    }
    if (nums.empty()) return;
    string acc = "0";
    if (op == 1) {
        for (const string& s : nums) acc = add_big(acc, s);
    } else if (op == 2) {
        acc = "1";
        for (const string& s : nums) acc = mul_big(acc, s);
    } else if ((int)nums.size() == 1) {
        acc = nums[0];
    }
    grand = add_big(grand, acc);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    ifstream fin("input.txt");
    vector<string> lines;
    string line;
    size_t maxw = 0;
    while (getline(fin, line)) {
        lines.push_back(line);
        if (line.size() > maxw) maxw = line.size();
    }
    fin.close();

    string grand = "0";
    bool inb = false;
    int sc = 0;
    for (size_t x = 0; x < maxw; ++x) {
        bool sep = true;
        for (const string& L : lines) {
            if (x < L.size() && !isspace((unsigned char)L[x])) { sep = false; break; }
        }
        if (!sep) {
            if (!inb) { inb = true; sc = (int)x; }
        } else {
            if (inb) {
                process_block(sc, (int)x - 1, lines, grand);
                inb = false;
            }
        }
    }
    if (inb) process_block(sc, (int)maxw - 1, lines, grand);

    cout << "Grand total: " << grand << "\n";
    return 0;
}