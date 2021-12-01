#include <iostream>
#include <fstream>
#include <climits>
#include <vector>

using namespace std;

int main(int argc, char* argv[]) {
    if (argc < 2) {
        cout << "Missing filename" << endl;
        return 1;
    }
    fstream infile(argv[1], fstream::in);

    vector<int> inputs;
    string line;
    int prev = INT_MAX, increased = 0;
    while(getline(infile, line)) {
        int l = stoi(line);
        inputs.push_back(l);
        if (l > prev) {
            ++increased;
        }
        prev = l;
    }
    cout << increased << endl;

    increased = 0;
    for (int i = 3; i < inputs.size(); ++i) {
        if (inputs[i] > inputs[i - 3]) {
            ++increased;
        }
    }
    cout << increased << endl;

    infile.close();

    return 0;
}