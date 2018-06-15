import java.io.BufferedReader;
import java.io.InputStreamReader;

public class fractions {

    public static void main(String[] args) {
        String command = args[0];
        switch (command.toLowerCase())
        {
            case "verify":
                verify(args);
                break;
        }

    }

    ////////////////////////////////////////////////////////////////////////
    /// verify
    private static void verify(String[] args) {
        int N = Integer.parseInt(args[1]);
        int[] digits = new int[N * 3];
        for (int i = 0; i < digits.length; i++)
        {
            digits[i] = Integer.parseInt(args[i + 2]);
        }

        try
        {
            StringBuilder sb = new StringBuilder();
            sb.append("verify(");
            sb.append(args[1]);
            sb = concatDigitArgs(sb, N, args);
            String predicate = sb.toString();
//            String command = "prolog -G10g -L10g -T10g -s ./bonus.pl -t bonus:crtRepresentationToDecimal(" + this.toPrologRepresentation() + ",X),writeln(X).\n";
            String command = "prolog -G10g -L10g -T10g -s ./bonus.pl -t bonus:" + predicate;
            Process pro = Runtime.getRuntime().exec(command);
            String line = null;
            StringBuilder output = new StringBuilder();
            int exitCode = pro.waitFor();
            BufferedReader in = new BufferedReader(
                    new InputStreamReader(pro.getInputStream()));
            while ((line = in.readLine()) != null) {
                output.append(line);
            }
            sb = new StringBuilder();
            sb.append("fractions(verify, ");
            sb.append(N);
            sb = concatDigitArgs(sb, N, args);
            if (exitCode == 0)
            {
                sb.append(", solution).");
            }
            else if (exitCode == 1)
            {
                sb.append(", not_solution).");
            }
            else
            {
                throw new RuntimeException("bad exit code " + exitCode);
            }
            System.out.println(sb.toString());
        }
        catch (Exception e)
        {
            e.printStackTrace();
            //TODO implement
        }
    }

    private static StringBuilder concatDigitArgs(StringBuilder sb, int N, String[] args)
    {
        sb.append(",[");
        for (int i = 2; i < N * 3 + 2; i++)
        {
            sb.append(args[i]);
            sb.append(",");
        }
        sb.append("])");
        int idx = sb.lastIndexOf(",");
        sb.replace(idx, idx+1,"").toString();
        return sb;
    }
}
