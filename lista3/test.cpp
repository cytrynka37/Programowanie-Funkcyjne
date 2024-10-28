#include <iostream>
#include <cmath>
#include <iomanip>

// Funkcja licząca wyrażenie bezpośrednio (standardowa metoda)
double standardMethod(double x) {
    return pow(10, 8) * (pow(M_Ef64, x) - pow(M_Ef64, 2 * x));
}

// Lepsza metoda przekształcona dla poprawy stabilności numerycznej
double improvedMethod(double x) {
    double exp_x = pow(M_Ef64, x);
    return pow(10, 8) * exp_x * (1 - exp_x);
}

// Przybliżenie analityczne przy małych x: e^x - e^(2x) ≈ -x
double taylorApproximation(double x) {
    return pow(10, 8) * -x;
}

// Funkcja testująca oba podejścia dla różnych, bardzo małych wartości x
void runTests() {
    std::cout << std::fixed << std::setprecision(20);
    double testValues[] = {1e-8, 1e-12, 1e-15, 1e-16};

    std::cout << " x \t\t\t Standard \t\t\t Improved \t\t\t Taylor Approx. \t Difference (Standard-Improved)\n";
    std::cout << "---------------------------------------------------------------------------------------------------------\n";
    
    for (double x : testValues) {
        double standardResult = standardMethod(x);
        double improvedResult = improvedMethod(x);
        double taylorResult = taylorApproximation(x);


        std::cout << x << "\t" 
                  << standardResult << "\t" 
                  << improvedResult << "\t" 
                  << taylorResult << "\t"
                  <<  "\n";
    }
}

int main() {
    runTests();
    return 0;
}
