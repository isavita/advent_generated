#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_map>

using namespace std;

struct File {
    string name;
    int size;
};

struct Directory {
    string name;
    vector<File> files;
    vector<Directory*> dirs;
    Directory* parent;
};

Directory* parseInput(ifstream& file) {
    Directory* root = new Directory();
    root->name = "/";
    Directory* currentDir = root;

    string line;
    while (getline(file, line)) {
        if (line[0] == '$') {
            // Command
            if (line.find("cd") != string::npos) {
                string dirName = line.substr(5);
                if (dirName == "..") {
                    currentDir = currentDir->parent;
                } else if (dirName == "/") {
                    currentDir = root;
                } else {
                    for (Directory* dir : currentDir->dirs) {
                        if (dir->name == dirName) {
                            currentDir = dir;
                            break;
                        }
                    }
                }
            } else if (line.find("ls") != string::npos) {
                // Do nothing
            }
        } else {
            // Output
            if (line.find("dir") != string::npos) {
                string dirName = line.substr(4);
                Directory* dir = new Directory();
                dir->name = dirName;
                dir->parent = currentDir;
                currentDir->dirs.push_back(dir);
            } else {
                size_t spacePos = line.find(' ');
                int fileSize = stoi(line.substr(0, spacePos));
                string fileName = line.substr(spacePos + 1);
                File file;
                file.name = fileName;
                file.size = fileSize;
                currentDir->files.push_back(file);
            }
        }
    }

    return root;
}

int calculateDirSize(Directory* dir) {
    int size = 0;
    for (File file : dir->files) {
        size += file.size;
    }
    for (Directory* subdir : dir->dirs) {
        size += calculateDirSize(subdir);
    }
    return size;
}

int main() {
    ifstream file("input.txt");
    Directory* root = parseInput(file);
    file.close();

    int sum = 0;
    unordered_map<string, int> dirSizes;
    vector<Directory*> dirs;
    dirs.push_back(root);
    while (!dirs.empty()) {
        Directory* dir = dirs.back();
        dirs.pop_back();
        int size = calculateDirSize(dir);
        dirSizes[dir->name] = size;
        if (size <= 100000) {
            sum += size;
        }
        for (Directory* subdir : dir->dirs) {
            dirs.push_back(subdir);
        }
    }

    cout << "Sum of total sizes of directories with size at most 100000: " << sum << endl;

    return 0;
}