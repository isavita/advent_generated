
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>

using namespace std;

inline unsigned int next_secret(unsigned int s) {
    unsigned int mod = 1 << 24;
    unsigned int x = s * 64;
    s ^= x;
    s &= mod - 1;
    x = s >> 5;
    s ^= x;
    s &= mod - 1;
    x = s * 2048;
    s ^= x;
    s &= mod - 1;
    return s;
}

inline int encode_change4(int c1, int c2, int c3, int c4) {
    return (c1 + 9) + (c2 + 9) * 19 + (c3 + 9) * 19 * 19 + (c4 + 9) * 19 * 19 * 19;
}

int main() {
    const int num_steps = 2000;
    const int pattern_count = 19 * 19 * 19 * 19;

    ifstream file("input.txt");
    if (!file.is_open()) {
        cerr << "Error opening input.txt" << endl;
        return 1;
    }

    vector<unsigned int> initials;
    unsigned int init_val;
    while (file >> init_val) {
        initials.push_back(init_val);
    }
    file.close();

    vector<int> global_sum(pattern_count, 0);

    for (unsigned int init_val : initials) {
        vector<int> prices(num_steps + 1);
        unsigned int s = init_val;
        for (int i = 0; i <= num_steps; ++i) {
            prices[i] = s % 10;
            s = next_secret(s);
        }

        vector<int> local_price(pattern_count, -1);
        for (int i = 0; i < num_steps - 3; ++i) {
            int c1 = prices[i + 1] - prices[i];
            int c2 = prices[i + 2] - prices[i + 1];
            int c3 = prices[i + 3] - prices[i + 2];
            int c4 = prices[i + 4] - prices[i + 3];

            if (c1 >= -9 && c1 <= 9 && c2 >= -9 && c2 <= 9 && c3 >= -9 && c3 <= 9 && c4 >= -9 && c4 <= 9) {
                int idx = encode_change4(c1, c2, c3, c4);
                if (local_price[idx] < 0) {
                    local_price[idx] = prices[i + 4];
                }
            }
        }

        for (int idx = 0; idx < pattern_count; ++idx) {
            if (local_price[idx] >= 0) {
                global_sum[idx] += local_price[idx];
            }
        }
    }

    cout << *max_element(global_sum.begin(), global_sum.end()) << endl;

    return 0;
}
