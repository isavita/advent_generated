
#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>

struct Particle {
    int p[3];
    int v[3];
    int a[3];
};

int abs(int x) {
    if (x < 0) {
        return -x;
    }
    return x;
}

int manhattan(int x[3]) {
    return abs(x[0]) + abs(x[1]) + abs(x[2]);
}

int main() {
    std::ifstream file("input.txt");
    if (!file.is_open()) {
        std::cerr << "Error opening file" << std::endl;
        return 1;
    }

    std::vector<Particle> particles;
    std::string line;
    while (std::getline(file, line)) {
        std::vector<std::string> parts;
        size_t pos = 0;
        while ((pos = line.find(", ")) != std::string::npos) {
            parts.push_back(line.substr(0, pos));
            line.erase(0, pos + 2);
        }
        parts.push_back(line);

        Particle p;
        for (int i = 0; i < parts.size(); i++) {
            std::vector<std::string> coords;
            size_t start = parts[i].find("<") + 1;
            size_t end = parts[i].find(">");
            std::string coordStr = parts[i].substr(start, end - start);
            pos = 0;
            while ((pos = coordStr.find(",")) != std::string::npos) {
                coords.push_back(coordStr.substr(0, pos));
                coordStr.erase(0, pos + 1);
            }
            coords.push_back(coordStr);

            for (int j = 0; j < coords.size(); j++) {
                int num = std::stoi(coords[j]);
                switch (i) {
                    case 0:
                        p.p[j] = num;
                        break;
                    case 1:
                        p.v[j] = num;
                        break;
                    case 2:
                        p.a[j] = num;
                        break;
                }
            }
        }
        particles.push_back(p);
    }

    int closestParticle = 0;
    int minAccel = INT_MAX;
    int minVelocity = INT_MAX;
    int minPosition = INT_MAX;

    for (int i = 0; i < particles.size(); i++) {
        int accel = manhattan(particles[i].a);
        int velocity = manhattan(particles[i].v);
        int position = manhattan(particles[i].p);

        if (accel < minAccel || (accel == minAccel && velocity < minVelocity) ||
            (accel == minAccel && velocity == minVelocity && position < minPosition)) {
            minAccel = accel;
            minVelocity = velocity;
            minPosition = position;
            closestParticle = i;
        }
    }

    std::cout << closestParticle << std::endl;

    return 0;
}
