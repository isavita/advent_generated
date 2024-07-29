#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <sstream>

struct Particle {
    int p[3], v[3], a[3];
};

int main() {
    std::ifstream file("input.txt");
    std::vector<Particle> particles;
    std::string line;

    while (std::getline(file, line)) {
        Particle p;
        std::istringstream ss(line);
        for (int i = 0; i < 3; ++i) {
            std::string part;
            std::getline(ss, part, ',');
            p.p[i] = std::stoi(part.substr(part.find('<') + 1, part.find('>') - part.find('<') - 1));
        }
        for (int i = 0; i < 3; ++i) {
            std::string part;
            std::getline(ss, part, ',');
            p.v[i] = std::stoi(part.substr(part.find('<') + 1, part.find('>') - part.find('<') - 1));
        }
        for (int i = 0; i < 3; ++i) {
            std::string part;
            std::getline(ss, part, ',');
            p.a[i] = std::stoi(part.substr(part.find('<') + 1, part.find('>') - part.find('<') - 1));
        }
        particles.push_back(p);
    }

    for (int tick = 0; tick < 1000; ++tick) {
        std::map<std::string, int> positions;
        for (auto& particle : particles) {
            for (int j = 0; j < 3; ++j) {
                particle.v[j] += particle.a[j];
                particle.p[j] += particle.v[j];
            }
            std::string posStr = std::to_string(particle.p[0]) + "," + std::to_string(particle.p[1]) + "," + std::to_string(particle.p[2]);
            positions[posStr]++;
        }

        std::vector<Particle> newParticles;
        for (const auto& particle : particles) {
            std::string posStr = std::to_string(particle.p[0]) + "," + std::to_string(particle.p[1]) + "," + std::to_string(particle.p[2]);
            if (positions[posStr] == 1) {
                newParticles.push_back(particle);
            }
        }
        particles = std::move(newParticles);
    }

    std::cout << particles.size() << std::endl;
}