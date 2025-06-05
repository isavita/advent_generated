
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>

struct FileSegment {
    int id;
    int start;
    int end;
};

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(NULL);

    std::ifstream inputFile("input.txt");
    std::string line;
    std::getline(inputFile, line);
    inputFile.close();

    std::vector<int> disk;
    int file_id_counter = 0;
    bool is_file_segment = true;

    for (char char_len : line) {
        int length = char_len - '0';
        for (int i = 0; i < length; ++i) {
            disk.push_back(is_file_segment ? file_id_counter : -1); // -1 for empty space
        }
        if (is_file_segment) {
            file_id_counter++;
        }
        is_file_segment = !is_file_segment;
    }

    std::vector<FileSegment> files;
    int current_segment_id = -2; // Sentinel for no current segment
    int segment_start_idx = 0;

    for (int i = 0; i < disk.size(); ++i) {
        int value = disk[i];
        if (value == -1) {
            current_segment_id = -2;
            continue;
        }

        if (value != current_segment_id) {
            current_segment_id = value;
            segment_start_idx = i;
        }

        if (i == disk.size() - 1 || disk[i+1] != value) {
            files.push_back({value, segment_start_idx, i});
        }
    }

    std::sort(files.begin(), files.end(), [](const FileSegment& a, const FileSegment& b) {
        return a.id > b.id; // Sort by ID descending
    });

    for (const auto& file : files) {
        int file_len = file.end - file.start + 1;
        int leftmost_free_span_start = -1;
        int current_free_span_len = 0;
        
        for (int i = 0; i < file.start; ++i) {
            if (disk[i] == -1) {
                if (current_free_span_len == 0) {
                    leftmost_free_span_start = i;
                }
                current_free_span_len++;
                if (current_free_span_len == file_len) {
                    break;
                }
            } else {
                current_free_span_len = 0;
                leftmost_free_span_start = -1;
            }
        }

        if (leftmost_free_span_start != -1 && current_free_span_len == file_len) {
            for (int i = file.start; i <= file.end; ++i) {
                disk[i] = -1;
            }
            for (int i = 0; i < file_len; ++i) {
                disk[leftmost_free_span_start + i] = file.id;
            }
        }
    }

    long long checksum = 0;
    for (int i = 0; i < disk.size(); ++i) {
        if (disk[i] != -1) {
            checksum += (long long)i * disk[i];
        }
    }

    std::cout << checksum << std::endl;

    return 0;
}
