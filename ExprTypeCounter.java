public class ExprTypeCounter {
    private static int lines = 0;
    private static int arith = 0;
    private static int point = 0;
    private static int array = 0;

    public static int getLines() { return lines; }
    public static int getArithExpr() { return arith; }
    public static int getPointerExpr() { return point; }
    public static int getArrayExpr() { return array; }

    public static void incLines(int last) { lines += last; }

    public static void incType(ExprType t) {
	if (t == null)
	    return;

	if (t.isArith())
	    arith ++;

	if (t.isPointer())
	    point ++;

	if (t.isArray())
	    array ++;
    }

    public static void printType(int l, ExprType t) {
	if (t.isVoid())
	    return;

	System.out.print("\t\tline " + l + ": ");
	
	if (t.isPointer())
	    System.out.print("pointer ");
	
	if (t.isArray())
	    System.out.print("array ");
	
	if (t.isArith())
	    System.out.print("arithmetic ");

	System.out.println("expression found");
    }
}
