## \file Control.py
# \author Dong Chen
# \brief Controls the flow of the program
import sys

import Calculations
import InputParameters
import OutputFormat

filename = sys.argv[1]
L_1, L_2, m_1, m_2 = InputParameters.get_input(filename)
InputParameters.input_constraints(L_1, L_2, m_1, m_2)
theta = Calculations.func_theta(m_1, m_2, L_2, L_1)
OutputFormat.write_output(theta)
