## \file Calculations.py
# \author Dong Chen
# \brief Provides functions for calculating the outputs
import math

import scipy.integrate

## \brief Calculates dependent variables (rad)
# \param m_1 the mass of the first object (kg)
# \param m_2 the mass of the second object (kg)
# \param L_2 the length of the second rod (m)
# \param L_1 the length of the first rod (m)
# \return dependent variables (rad)
def func_theta(m_1, m_2, L_2, L_1):
    def f(t, theta):
        return [theta[1], (-9.8 * (2.0 * m_1 + m_2) * math.sin(theta[0]) - m_2 * 9.8 * math.sin(theta[0] - 2.0 * theta[2]) - 2.0 * math.sin(theta[0] - theta[2]) * m_2 * (theta[3] ** 2.0 * L_2 + theta[1] ** 2.0 * L_1 * math.cos(theta[0] - theta[2]))) / (L_1 * (2.0 * m_1 + m_2 - m_2 * math.cos(2.0 * theta[0] - 2.0 * theta[2]))), theta[3], 2.0 * math.sin(theta[0] - theta[2]) * (theta[1] ** 2.0 * L_1 * (m_1 + m_2) + 9.8 * (m_1 + m_2) * math.cos(theta[0]) + theta[3] ** 2.0 * L_2 * m_2 * math.cos(theta[0] - theta[2])) / (L_2 * (2.0 * m_1 + m_2 - m_2 * math.cos(2.0 * theta[0] - 2.0 * theta[2])))]
    
    r = scipy.integrate.ode(f)
    r.set_integrator("dopri5", atol=1.0e-6, rtol=1.0e-6)
    r.set_initial_value([1.3463968515384828, 0.0, 2.356194490192345, 0.0], 0.0)
    theta = [[1.3463968515384828, 0.0, 2.356194490192345, 0.0][0]]
    while r.successful() and r.t < 20.0:
        r.integrate(r.t + 1.0e-3)
        theta.append(r.y[0])
    
    return theta
