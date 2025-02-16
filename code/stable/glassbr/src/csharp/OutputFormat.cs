/** \file OutputFormat.cs
    \author Nikitha Krithnan and W. Spencer Smith
    \brief Provides the function for writing outputs
*/
using System;
using System.IO;

public class OutputFormat {
    
    /** \brief Writes the output values to output.txt
        \param isSafePb probability of glass breakage safety requirement
        \param isSafeLR 3 second load equivalent resistance safety requirement
        \param P_b probability of breakage: the fraction of glass lites or plies that would break at the first occurrence of a specified load and duration, typically expressed in lites per 1000 (Ref: astm2016)
        \param J stress distribution factor (Function)
    */
    public static void write_output(Boolean isSafePb, Boolean isSafeLR, double P_b, double J) {
        StreamWriter outfile;
        outfile = new StreamWriter("log.txt", true);
        outfile.WriteLine("function write_output called with inputs: {");
        outfile.Write("  isSafePb = ");
        outfile.Write(isSafePb);
        outfile.WriteLine(", ");
        outfile.Write("  isSafeLR = ");
        outfile.Write(isSafeLR);
        outfile.WriteLine(", ");
        outfile.Write("  P_b = ");
        outfile.Write(P_b);
        outfile.WriteLine(", ");
        outfile.Write("  J = ");
        outfile.WriteLine(J);
        outfile.WriteLine("  }");
        outfile.Close();
        
        StreamWriter outputfile;
        outputfile = new StreamWriter("output.txt", false);
        outputfile.Write("isSafePb = ");
        outputfile.WriteLine(isSafePb);
        outputfile.Write("isSafeLR = ");
        outputfile.WriteLine(isSafeLR);
        outputfile.Write("P_b = ");
        outputfile.WriteLine(P_b);
        outputfile.Write("J = ");
        outputfile.WriteLine(J);
        outputfile.Close();
    }
}
