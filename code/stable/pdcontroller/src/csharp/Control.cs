/** \file Control.cs
    \author Naveen Ganesh Muralidharan
    \brief Controls the flow of the program
*/
using System.Collections.Generic;

public class Control {
    
    /** \brief Controls the flow of the program
        \param args List of command-line arguments
    */
    public static void Main(string[] args) {
        string filename = args[0];
        double r_t;
        double K_d;
        double K_p;
        double t_step;
        double t_sim;
        InputParameters.get_input(filename, out r_t, out K_d, out K_p, out t_step, out t_sim);
        InputParameters.input_constraints(r_t, K_d, K_p, t_step, t_sim);
        List<double> y_t = Calculations.func_y_t(K_d, K_p, r_t, t_sim, t_step);
        OutputFormat.write_output(y_t);
    }
}
