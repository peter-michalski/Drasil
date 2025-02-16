package PD_Controller;

/** \file InputParameters.java
    \author Naveen Ganesh Muralidharan
    \brief Provides the function for reading inputs and the function for checking the physical constraints on the input
*/
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class InputParameters {
    
    /** \brief Reads input from a file with the given file name
        \param filename name of the input file
        \return array containing the following values:
        \return Set-Point: The desired value that the control system must reach. This also knows as the reference variable
        \return Derivative Gain: Gain constant of the derivative controller
        \return Proportional Gain: Gain constant of the proportional controller
        \return Step Time: Simulation step time (s)
        \return Simulation Time: Total execution time of the PD simulation (s)
    */
    public static Object[] get_input(String filename) throws FileNotFoundException {
        double r_t;
        double K_d;
        double K_p;
        double t_step;
        double t_sim;
        
        Scanner infile;
        infile = new Scanner(new File(filename));
        infile.nextLine();
        r_t = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        K_d = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        K_p = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        t_step = Double.parseDouble(infile.nextLine());
        infile.nextLine();
        t_sim = Double.parseDouble(infile.nextLine());
        infile.close();
        
        Object[] outputs = new Object[5];
        outputs[0] = r_t;
        outputs[1] = K_d;
        outputs[2] = K_p;
        outputs[3] = t_step;
        outputs[4] = t_sim;
        return outputs;
    }
    
    /** \brief Verifies that input values satisfy the physical constraints
        \param r_t Set-Point: The desired value that the control system must reach. This also knows as the reference variable
        \param K_d Derivative Gain: Gain constant of the derivative controller
        \param K_p Proportional Gain: Gain constant of the proportional controller
        \param t_step Step Time: Simulation step time (s)
        \param t_sim Simulation Time: Total execution time of the PD simulation (s)
    */
    public static void input_constraints(double r_t, double K_d, double K_p, double t_step, double t_sim) throws Exception {
        if (!(r_t > 0.0)) {
            System.out.print("r_t has value ");
            System.out.print(r_t);
            System.out.print(", but is expected to be ");
            System.out.print("above ");
            System.out.print(0.0);
            System.out.println(".");
            throw new Exception("InputError");
        }
        if (!(K_d >= 0.0)) {
            System.out.print("K_d has value ");
            System.out.print(K_d);
            System.out.print(", but is expected to be ");
            System.out.print("above ");
            System.out.print(0.0);
            System.out.println(".");
            throw new Exception("InputError");
        }
        if (!(K_p > 0.0)) {
            System.out.print("K_p has value ");
            System.out.print(K_p);
            System.out.print(", but is expected to be ");
            System.out.print("above ");
            System.out.print(0.0);
            System.out.println(".");
            throw new Exception("InputError");
        }
        if (!(1.0 / 1000.0 <= t_step && t_step < t_sim)) {
            System.out.print("t_step has value ");
            System.out.print(t_step);
            System.out.print(", but is expected to be ");
            System.out.print("between ");
            System.out.print(1.0 / 1000.0);
            System.out.print(" ((1)/(1000))");
            System.out.print(" and ");
            System.out.print(t_sim);
            System.out.print(" (t_sim)");
            System.out.println(".");
            throw new Exception("InputError");
        }
        if (!(1.0 <= t_sim && t_sim <= 60.0)) {
            System.out.print("t_sim has value ");
            System.out.print(t_sim);
            System.out.print(", but is expected to be ");
            System.out.print("between ");
            System.out.print(1.0);
            System.out.print(" and ");
            System.out.print(60.0);
            System.out.println(".");
            throw new Exception("InputError");
        }
    }
}
