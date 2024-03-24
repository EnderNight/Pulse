#include <fstream>
#include <iostream>

int main(int argc, char *argv[])
{
    if (argc >= 2) {
        std::ifstream file(argv[1]);

        if (!file.is_open())
            std::cerr << "File not found" << std::endl;
        else
            std::cout << file.rdbuf() << std::endl;
    }

    return 0;
}
