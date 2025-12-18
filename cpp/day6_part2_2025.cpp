#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>
#include <cctype>

static std::string add_str(const std::string& a, const std::string& b) {
    int i = (int)a.size() - 1;
    int j = (int)b.size() - 1;
    int carry = 0;
    std::string res;
    res.reserve(std::max(a.size(), b.size()) + 1);
    while (i >= 0 || j >= 0 || carry) {
        int da = (i >= 0) ? (a[i] - '0') : 0;
        int db = (j >= 0) ? (b[j] - '0') : 0;
        int sum = da + db + carry;
        res.push_back(char('0' + (sum % 10)));
        carry = sum / 10;
        --i; --j;
    }
    std::reverse(res.begin(), res.end());
    // remove leading zeros if any (protective)
    size_t pos = 0;
    while (pos + 1 < res.size() && res[pos] == '0') ++pos;
    if (pos) res.erase(0, pos);
    return res;
}

static std::string mul_str(const std::string& a, const std::string& b) {
    if ((a.size() == 1 && a[0] == '0') || (b.size() == 1 && b[0] == '0')) return "0";
    std::vector<int> tmp(a.size() + b.size(), 0);
    int la = (int)a.size();
    int lb = (int)b.size();
    for (int i = la - 1; i >= 0; --i) {
        int da = a[i] - '0';
        for (int j = lb - 1; j >= 0; --j) {
            int db = b[j] - '0';
            int pos_i = la - 1 - i;
            int pos_j = lb - 1 - j;
            tmp[pos_i + pos_j] += da * db;
        }
    }
    int carry = 0;
    for (size_t k = 0; k < tmp.size(); ++k) {
        int sum = tmp[k] + carry;
        tmp[k] = sum % 10;
        carry = sum / 10;
    }
    size_t len = tmp.size();
    while (len > 1 && tmp[len - 1] == 0) --len;
    std::string res;
    res.reserve(len);
    for (size_t i = 0; i < len; ++i) res.push_back(char('0' + tmp[len - 1 - i]));
    // remove leading zeros if any
    size_t pos = 0;
    while (pos + 1 < res.size() && res[pos] == '0') ++pos;
    if (pos) res.erase(0, pos);
    return res;
}

static void process_block(const std::vector<std::string>& lines, size_t linecnt, int start, int end, std::string& grand_total) {
    std::vector<std::string> nums;
    char op = '+';
    for (int c = start; c <= end; ++c) {
        std::string buf;
        buf.reserve(linecnt + 1);
        for (size_t r = 0; r < linecnt; ++r) {
            const std::string& ln = lines[r];
            if ((size_t)ln.size() > (size_t)c) {
                char ch = ln[c];
                if (std::isdigit((unsigned char)ch)) {
                    buf.push_back(ch);
                } else if (ch == '+' || ch == '*') {
                    op = ch;
                }
            }
        }
        if (!buf.empty()) nums.push_back(std::move(buf));
    }
    if (nums.empty()) return;
    std::string block_res;
    if (op == '*') {
        block_res = "1";
        for (const auto& num : nums) {
            block_res = mul_str(block_res, num);
        }
    } else {
        block_res = "0";
        for (const auto& num : nums) {
            block_res = add_str(block_res, num);
        }
    }
    grand_total = add_str(grand_total, block_res);
}

int main() {
    std::ifstream fin("input.txt");
    if (!fin) return 1;
    std::vector<std::string> lines;
    std::string s;
    size_t maxw = 0;
    while (std::getline(fin, s)) {
        if (!s.empty() && s.back() == '\r') s.pop_back();
        lines.push_back(s);
        if (s.size() > maxw) maxw = s.size();
    }
    fin.close();

    size_t linecnt = lines.size();
    if (linecnt == 0) {
        std::cout << "Grand total: 0\n";
        return 0;
    }

    std::vector<char> is_sep(maxw, 0);
    for (size_t x = 0; x < maxw; ++x) {
        bool allspace = true;
        for (size_t r = 0; r < linecnt; ++r) {
            if (lines[r].size() > x) {
                char ch = lines[r][x];
                if (!std::isspace((unsigned char)ch)) {
                    allspace = false;
                    break;
                }
            }
        }
        is_sep[x] = allspace ? 1 : 0;
    }

    std::string grand_total = "0";
    bool in_block = false;
    int start = 0;
    for (size_t x = 0; x < maxw; ++x) {
        if (!is_sep[x]) {
            if (!in_block) {
                in_block = true;
                start = (int)x;
            }
        } else {
            if (in_block) {
                process_block(lines, linecnt, start, (int)x - 1, grand_total);
                in_block = false;
            }
        }
    }
    if (in_block) process_block(lines, linecnt, start, (int)maxw - 1, grand_total);

    std::cout << "Grand total: " << grand_total << "\n";
    return 0;
}