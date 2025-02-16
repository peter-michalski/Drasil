/** \file InputParameters.cs
    \author Dong Chen
    \brief Provides the function for reading inputs and the function for checking the physical constraints on the input
*/
using System;
using System.IO;

public class InputParameters {
    
    /** \brief Reads input from a file with the given file name
        \param filename name of the input file
        \param L_1 the length of the first rod (m)
        \param L_2 the length of the second rod (m)
        \param m_1 the mass of the first object (kg)
        \param m_2 the mass of the second object (kg)
    */
    public static void get_input(string filename, out double L_1, out double L_2, out double m_1, out double m_2) {
        StreamReader infile;
        infile = new StreamReader(filename);
        infile.ReadLine();
        L_1 = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        L_2 = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        m_1 = Double.Parse(infile.ReadLine());
        infile.ReadLine();
        m_2 = Double.Parse(infile.ReadLine());
        infile.Close();
    }
    
    /** \brief Verifies that input values satisfy the physical constraints
        \param L_1 the length of the first rod (m)
        \param L_2 the length of the second rod (m)
        \param m_1 the mass of the first object (kg)
        \param m_2 the mass of the second object (kg)
    */
    public static void input_constraints(double L_1, double L_2, double m_1, double m_2) {
        if (!(L_1 > 0.0)) {
            Console.Write("Warning: ");
            Console.Write("L_1 has value ");
            Console.Write(L_1);
            Console.Write(", but is suggested to be ");
            Console.Write("above ");
            Console.Write(0.0);
            Console.WriteLine(".");
        }
        if (!(L_2 > 0.0)) {
            Console.Write("Warning: ");
            Console.Write("L_2 has value ");
            Console.Write(L_2);
            Console.Write(", but is suggested to be ");
            Console.Write("above ");
            Console.Write(0.0);
            Console.WriteLine(".");
        }
        if (!(m_1 > 0.0)) {
            Console.Write("Warning: ");
            Console.Write("m_1 has value ");
            Console.Write(m_1);
            Console.Write(", but is suggested to be ");
            Console.Write("above ");
            Console.Write(0.0);
            Console.WriteLine(".");
        }
        if (!(m_2 > 0.0)) {
            Console.Write("Warning: ");
            Console.Write("m_2 has value ");
            Console.Write(m_2);
            Console.Write(", but is suggested to be ");
            Console.Write("above ");
            Console.Write(0.0);
            Console.WriteLine(".");
        }
    }
}
