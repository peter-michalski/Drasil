## \file Calculations.py
# \author Thulasi Jegatheesan
# \brief Provides functions for calculating the outputs
import scipy.integrate

## \brief Calculates volume of water: the amount of space occupied by a given quantity of water (m^3)
# \param V_tank volume of the cylindrical tank: the amount of space encompassed by a tank (m^3)
# \return volume of water: the amount of space occupied by a given quantity of water (m^3)
def func_V_W(V_tank):
    return V_tank

## \brief Calculates mass of water: the quantity of matter within the water (kg)
# \param rho_W density of water: mass per unit volume of water (kg/m^3)
# \param V_W volume of water: the amount of space occupied by a given quantity of water (m^3)
# \return mass of water: the quantity of matter within the water (kg)
def func_m_W(rho_W, V_W):
    return V_W * rho_W

## \brief Calculates ODE parameter for water related to decay time: derived parameter based on rate of change of temperature of water (s)
# \param C_W specific heat capacity of water: the amount of energy required to raise the temperature of a given unit mass of water by a given amount (J/(kg degreeC))
# \param h_C convective heat transfer coefficient between coil and water: the convective heat transfer coefficient that models the thermal flux from the coil to the surrounding water (W/(m^2 degreeC))
# \param A_C heating coil surface area: area covered by the outermost layer of the coil (m^2)
# \param m_W mass of water: the quantity of matter within the water (kg)
# \return ODE parameter for water related to decay time: derived parameter based on rate of change of temperature of water (s)
def func_tau_W(C_W, h_C, A_C, m_W):
    return m_W * C_W / (h_C * A_C)

## \brief Calculates temperature of the water: the average kinetic energy of the particles within the water (degreeC)
# \param T_C temperature of the heating coil: the average kinetic energy of the particles within the coil (degreeC)
# \param T_init initial temperature: the temperature at the beginning of the simulation (degreeC)
# \param t_final final time: the amount of time elapsed from the beginning of the simulation to its conclusion (s)
# \param A_tol absolute tolerance
# \param R_tol relative tolerance
# \param t_step time step for simulation: the finite discretization of time used in the numerical method for solving the computational model (s)
# \param tau_W ODE parameter for water related to decay time: derived parameter based on rate of change of temperature of water (s)
# \return temperature of the water: the average kinetic energy of the particles within the water (degreeC)
def func_T_W(T_C, T_init, t_final, A_tol, R_tol, t_step, tau_W):
    def f(t, T_W):
        return [1.0 / tau_W * (T_C - T_W[0])]
    
    r = scipy.integrate.ode(f)
    r.set_integrator("dopri5", atol=A_tol, rtol=R_tol)
    r.set_initial_value([T_init], 0.0)
    T_W = [[T_init][0]]
    while r.successful() and r.t < t_final:
        r.integrate(r.t + t_step)
        T_W.append(r.y[0])
    
    return T_W
