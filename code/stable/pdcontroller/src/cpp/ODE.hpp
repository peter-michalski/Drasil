/** \file ODE.hpp
    \author Naveen Ganesh Muralidharan
    \brief Class representing an ODE system
*/
#ifndef ODE_h
#define ODE_h

#include <vector>

using std::vector;

/** \brief Class representing an ODE system
*/
class ODE {
    public:
        /** \brief Constructor for ODE objects
            \param K_p Proportional Gain: Gain constant of the proportional controller
            \param K_d Derivative Gain: Gain constant of the derivative controller
            \param r_t Set-Point: The desired value that the control system must reach. This also knows as the reference variable
        */
        ODE(double K_p, double K_d, double r_t);
        /** \brief function representation of ODE system
            \param y_t Process Variable: The output value from the power plant
            \param dy_t change in Process Variable
            \param t current independent variable value in ODE solution
        */
        void operator()(vector<double> y_t, vector<double> &dy_t, double t);
    
    private:
        double K_p;
        double K_d;
        double r_t;
        
};

#endif
