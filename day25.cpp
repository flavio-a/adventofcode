#include <iostream>
#include <fstream>

using namespace std;

typedef unsigned int uint;
typedef unsigned long long int ull;

uint inline transformStep(uint value, uint sub_num) {
    return ((ull)value * (ull)sub_num) % 20201227;
}

uint transform(uint loop_size, uint sub_num) {
    uint value = 1
    // TODO: fast exponentiation here
    for (uint i = 0; i < loop_size; ++i) {
        value = transformStep(value, sub_num);
    }
    return value;
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        cout << "Missing filename" << endl;
        return 1;
    }
    fstream infile(argv[1], fstream::in);

    string line;
    getline(infile, line);
    const uint cardpc = stoi(line);
    getline(infile, line);
    const uint doorpc = stoi(line);
    infile.close();

    // Part 1
    const uint startin_sub_num = 7;
    uint cardls = 0;
    uint value = 1;
    while (value != cardpc) {
        value = transformStep(value, startin_sub_num);
        ++cardls;
    }
    cout << "Card loop size: " << cardls << endl;
    cout << transform(cardls, doorpc) << endl;

    // Part 2
    // lolnope

    return 0;
}
