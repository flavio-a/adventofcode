#include <iostream>
#include <fstream>
#include <algorithm>

using namespace std;

int main(int argc, char* argv[]) {
    if (argc < 2) {
        cout << "Missing filename" << endl;
        return 1;
    }
    fstream infile(argv[1], fstream::in);

    unsigned int n_valid1 = 0, n_valid2 = 0;
    string line;
    while(getline(infile, line)) {
        size_t sz;
        int l = stoi(line, &sz);
        line = line.substr(sz + 1);
        int u = stoi(line, &sz);
        char c = line[sz + 1];
        // line = line.substr(sz + 4);

        auto occ = count(line.begin() + sz + 4, line.end(), c);
        if (l <= occ && occ <= u) {
            ++n_valid1;
        }

        if ((line[l + sz - 1 + 4] == c) != (line[u + sz - 1 + 4] == c)) {
            ++n_valid2;
        }
    }
    cout << n_valid1 << endl;
    cout << n_valid2 << endl;

    infile.close();

    return 0;
}
