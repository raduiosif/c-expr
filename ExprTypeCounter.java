public class ExprTypeCounter {
    private static int lines = 0;
    private static int arith = 0;
    private static int point = 0;
    private static int array = 0;
    private static int nonlinear = 0;

    public static int getLines() { return lines; }
    public static int getArithExpr() { return arith; }
    public static int getPointerExpr() { return point; }
    public static int getArrayExpr() { return array; }
    public static int getNonLinearExpr() { return nonlinear; }

    public static void incLines(int last) { lines += last; }

    public static void incType(ExprType t, int l) {
	if (t == null || t.isVoid() || t.isConst() || t.isVar())
	    return;

	if (CMain.verbosityLevel >= CMain.MEDIUM)
	    System.out.println("\tline " + l + ": " + t + " expression found");

	if (t.isArith())
	    arith ++;

	if (t.isPointer())
	    point ++;

	if (t.isArray())
	    array ++;

	if (t.isNonLinear())
	    nonlinear ++;
    }
}

