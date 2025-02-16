/** \file Control.cpp
    \author Naveen Ganesh Muralidharan
    \brief Controls the flow of the program
*/
#include <string>
#include <vector>

#include "Calculations.hpp"
#include "InputParameters.hpp"
#include "OutputFormat.hpp"

using std::string;
using std::vector;

/** \brief Controls the flow of the program
    \param argc Number of command-line arguments
    \param argv List of command-line arguments
    \return exit code
*/
int main(int argc, const char *argv[]) {
    string filename = argv[1];
    double r_t;
    double K_d;
    double K_p;
    double t_step;
    double t_sim;
    get_input(filename, r_t, K_d, K_p, t_step, t_sim);
    input_constraints(r_t, K_d, K_p, t_step, t_sim);
    vector<double> y_t = func_y_t(K_d, K_p, r_t, t_sim, t_step);
    write_output(y_t);
    
    return 0;
}
