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

    public static void incType(ExprType t) {
	if (t == null || t.isConst())
	    return;

	if (t.isArith())
	    arith ++;

	if (t.isPointer())
	    point ++;

	if (t.isArray())
	    array ++;

	if (t.isNonLinear())
	    nonlinear ++;
    }

    public static void printType(int l, ExprType t) {
	if (t.isVoid() || t.isConst())
	    return;

	System.out.print("\t\tline " + l + ": ");

	if (t.isConst())
	    System.out.print("constant ");

	if (t.isVar())
	    System.out.print("variable ");

	if (t.isPointer())
	    System.out.print("pointer ");
	
	if (t.isArray())
	    System.out.print("array ");

	if (t.isNonLinear())
	    System.out.print("nonlinear ");

	if (t.isArith())
	    System.out.print("arithmetic ");

	System.out.println("expression found");
    }
}
