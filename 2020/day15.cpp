#include <iostream>
#include <fstream>
#include <vector>
#include <unordered_map>
#include <algorithm>
#include <sstream>

using namespace std;

typedef unsigned int uint;
typedef unordered_map<uint, uint> store;

uint nextVal(const store& s, uint pos, uint v) {
    auto idx = s.find(v);
    if (idx == s.end()) {
        return 0;
    }
    else {
        return pos - idx->second;
    }
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        cout << "Missing filename" << endl;
        return 1;
    }
    fstream infile(argv[1], fstream::in);
    const uint targetidx = 30000000;

    string line;
    getline(infile, line);
    infile.close();
    vector<uint> numbers;
    numbers.reserve(targetidx);

    replace(line.begin(), line.end(), ',', ' ');
    stringstream ss(line);
    int temp;
    store s;
    s.reserve(targetidx);
    while (ss >> temp) {
        numbers.push_back(temp);
        s.insert_or_assign(temp, numbers.size());
    }

    uint i = numbers.size();
    while (i < targetidx) {
        const uint val = numbers.back();
        const uint newval = nextVal(s, i, val);
        numbers.push_back(newval);
        s.insert_or_assign(val, i);
        ++i;
        #ifdef DEBUG
        if (i % 100000 == 0) {
            cout << "Iteration " << i << endl;
        }
        #endif
    }

    cout << numbers[2020 - 1] << endl;
    cout << numbers[30000000 - 1] << endl;


    return 0;
}
