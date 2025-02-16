package PD_Controller;

/** \file ODE.java
    \author Naveen Ganesh Muralidharan
    \brief Class representing an ODE system
*/
import org.apache.commons.math3.ode.FirstOrderDifferentialEquations;

/** \brief Class representing an ODE system
*/
public class ODE implements FirstOrderDifferentialEquations {
    private double K_p;
    private double K_d;
    private double r_t;
    
    /** \brief Constructor for ODE objects
        \param K_p Proportional Gain: Gain constant of the proportional controller
        \param K_d Derivative Gain: Gain constant of the derivative controller
        \param r_t Set-Point: The desired value that the control system must reach. This also knows as the reference variable
    */
    public ODE(double K_p, double K_d, double r_t) {
        this.K_p = K_p;
        this.K_d = K_d;
        this.r_t = r_t;
    }
    
    /** \brief returns the ODE system dimension
        \return dimension of the ODE system
    */
    public int getDimension() {
        return 2;
    }
    
    /** \brief function representation of an ODE system
        \param t current independent variable value in ODE solution
        \param y_t Process Variable
        \param dy_t change in Process Variable
    */
    public void computeDerivatives(double t, double[] y_t, double[] dy_t) {
        dy_t[0] = y_t[1];
        dy_t[1] = -(1.0 + K_d) * y_t[1] + -(20.0 + K_p) * y_t[0] + r_t * K_p;
    }
}
