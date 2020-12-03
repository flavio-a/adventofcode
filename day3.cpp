#include <iostream>
#include <fstream>
#include <algorithm>
#include <vector>
#include <cassert>

using namespace std;

typedef unsigned int uint;
typedef std::pair<uint, uint> slope;

uint check_slope(slope slope, vector<string> field) {
    uint i = 0, j = 0, trees = 0;
    const auto w = field[0].size();
    while(i < field.size()) {
        if (field[i][j] == '#') {
            ++trees;
        }
        i += slope.first;
        j = (j + slope.second) % w;
    }
    return trees;
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        cout << "Missing filename" << endl;
        return 1;
    }
    fstream infile(argv[1], fstream::in);

    string line;
    vector<string> field;
    field.reserve(330);
    while(getline(infile, line)) {
        field.push_back(string(line));
    }

    auto trees1 = check_slope(slope(1, 3), field);
    cout << trees1 << endl;
    cout << check_slope(slope(1, 1), field)
            * trees1
            * check_slope(slope(1, 5), field)
            * check_slope(slope(1, 7), field)
            * check_slope(slope(2, 1), field)
            << endl;

    infile.close();

    return 0;
}
