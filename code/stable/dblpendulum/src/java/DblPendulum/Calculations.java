package DblPendulum;

/** \file Calculations.java
    \author Dong Chen
    \brief Provides functions for calculating the outputs
*/
import java.util.ArrayList;
import org.apache.commons.math3.ode.FirstOrderIntegrator;
import org.apache.commons.math3.ode.nonstiff.DormandPrince54Integrator;

public class Calculations {
    
    /** \brief Calculates dependent variables (rad)
        \param m_1 the mass of the first object (kg)
        \param m_2 the mass of the second object (kg)
        \param L_2 the length of the second rod (m)
        \param L_1 the length of the first rod (m)
        \return dependent variables (rad)
    */
    public static ArrayList<Double> func_theta(double m_1, double m_2, double L_2, double L_1) {
        ArrayList<Double> theta;
        ODEStepHandler stepHandler = new ODEStepHandler();
        ODE ode = new ODE(m_1, m_2, L_1, L_2);
        double[] curr_vals = {1.3463968515384828, 0.0, 2.356194490192345, 0.0};
        
        FirstOrderIntegrator it = new DormandPrince54Integrator(1.0e-3, 1.0e-3, 1.0e-6, 1.0e-6);
        it.addStepHandler(stepHandler);
        it.integrate(ode, 0.0, curr_vals, 20.0, curr_vals);
        theta = stepHandler.theta;
        
        return theta;
    }
}
