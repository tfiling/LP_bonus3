import java.io.BufferedReader;
import java.io.InputStreamReader;

public class CrtNumber {

    public static int[] BASE = {2, 3, 5, 7, 11, 13, 17};
    public static int REPRESENTATION_SIZE = BASE.length;

    private int[] _crtRepresentation;
    private Long _decimalValue = null;

    public CrtNumber(int[] crtRepresentation)
    {
        this._crtRepresentation = crtRepresentation.clone();
    }

    public CrtNumber(long decimal)
    {
        _crtRepresentation = new int[REPRESENTATION_SIZE];
        for (int i = 0; i < REPRESENTATION_SIZE; i++)
        {
            _crtRepresentation[i] = (int)decimal % BASE[i];//must have 32 bit representaion, the result of modulo is small eanough
        }
    }

    public long getDecimalValue()
    {
        if (_decimalValue == null)
        {
            _decimalValue = convertToDecimal();
        }
        return _decimalValue;
    }

    private Long convertToDecimal() {
        try
        {
            String command = "prolog -G10g -L10g -T10g -s ./bonus.pl -t bonus:crtRepresentationToDecimal(" + this.toPrologRepresentation() + ",X),writeln(X).\n";
            Process pro = Runtime.getRuntime().exec(command);
            String line = null;
            StringBuilder output = new StringBuilder();
            int exitCode = pro.waitFor();
            BufferedReader in = new BufferedReader(
                    new InputStreamReader(pro.getInputStream()));
            while ((line = in.readLine()) != null) {
                output.append(line);
            }
            if (exitCode == 0)
            {
                String resultString = output.toString().split("sat")[1];
                return Long.parseLong(resultString);

            }
            else
            {
                System.out.println("prolog failed with output:\n " + output.toString());
            }
        }
        catch (Exception e)
        {
            e.printStackTrace();
            //TODO implement
        }
        return null;
    }

    public CrtNumber multiply(CrtNumber... others)
    {
        CrtNumber currentResult = new CrtNumber(_crtRepresentation);
        for (CrtNumber other : others)
        {
            int[] resultRepresentation = new int[REPRESENTATION_SIZE];
            for (int i = 0; i < REPRESENTATION_SIZE; i++)
            {

                resultRepresentation[i] = (currentResult._crtRepresentation[i] * other._crtRepresentation[i]) % BASE[i];//must have 32 bit representaion, the result of modulo is small eanough
            }
            currentResult = new CrtNumber(resultRepresentation);
        }
        return currentResult;
    }

    public CrtNumber sum(CrtNumber... others)
    {
        CrtNumber currentResult = new CrtNumber(_crtRepresentation);
        for (CrtNumber other : others)
        {
            int[] resultRepresentation = new int[REPRESENTATION_SIZE];
            for (int i = 0; i < REPRESENTATION_SIZE; i++)
            {

                resultRepresentation[i] = (currentResult._crtRepresentation[i] + other._crtRepresentation[i]) % BASE[i];//must have 32 bit representaion, the result of modulo is small eanough
            }
            currentResult = new CrtNumber(resultRepresentation);
        }
        return currentResult;
    }

    public String toPrologRepresentation()
    {
        StringBuilder sb = new StringBuilder("[");
        for (int i = 0; i < REPRESENTATION_SIZE; i++)
        {
            sb.append(_crtRepresentation[i]);
            sb.append(",");
        }
        sb.append("]");
        String result = sb.toString();
        int idx = result.lastIndexOf(",");
        if( idx >=0 )
            result = new StringBuilder(result).replace(idx, idx+1,"").toString();
        return result;

    }

    public static void main(String[] args) {
        CrtNumber num = new CrtNumber(2);
        System.out.println(num.multiply(new CrtNumber(2), new CrtNumber(3)).convertToDecimal());


    }
}
