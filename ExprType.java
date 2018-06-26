public class ExprType {
    public static boolean expression = false; 

    public static int VOID  = 0; 
    public static int ARITH = 1;
    public static int POINT = 2;
    public static int ARRAY = 4;
    public static int VAR = 8;
    public  static int CONST = 16;
    public static int NONLIN = 32;

    private int type;

    public int getType() { return type; }

    public void setArith() { type |= ARITH; }
    public void setNonLinear() { type |= NONLIN; }
    public void setArray() { type |= ARRAY; }
    public void setPointer() { type |= POINT; }

    public boolean isVoid() { return type == VOID; }
    public boolean isConst() { return type == CONST; }

    public boolean isVar() { return (type & VAR) != 0; }
    public boolean isArith() { return (type & ARITH) != 0; }
    public boolean isNonLinear() { return (type & NONLIN) != 0; }
    public boolean isPointer() { return (type & POINT) != 0; }
    public boolean isArray() { return (type & ARRAY) != 0; }

    public ExprType(int t) {
	type = t;
    }

    public ExprType(ExprType t1, ExprType t2) {
	int type1 = (t1 == null) ? VOID : t1.getType();
	int type2 = (t2 == null) ? VOID : t2.getType();

	type = (type1 & ~(CONST | VAR)) | (type2 & ~(CONST | VAR)); 
    }

    public ExprType(ExprType t1, ExprType t2, String op) {
	int type1 = (t1 == null) ? VOID : t1.getType();
	int type2 = (t2 == null) ? VOID : t2.getType();

	if (type1 == VOID) {
	    type = type2; 
	    return;
	}

	if (type2 == VOID) {
	    type = type1; 
	    return;
	}

	if (t1.isConst() && t2.isConst()) {
	    type = CONST;
	    return;
	}	

	type = (type1 & ~(CONST | VAR)) | (type2 & ~(CONST | VAR));

	if (op.equals("+") || op.equals("+=") || 
	    op.equals("-") || op.equals("-=") ||
	    op.equals("*") || op.equals("*=") || 
	    op.equals("/") || op.equals("/=") || 
	    op.equals("%") || op.equals("%="))
	    type |= ARITH; 

	if (!(expression && op.equals("*")) && 
	    (t1.isVar() || t1.isPointer() || t1.isArray()) && 
	    (t2.isVar() || t2.isPointer() || t2.isArray()) &&	    
	    op.equals("*") || op.equals("*=") || 
	    op.equals("/") || op.equals("/=") || 
	    op.equals("%") || op.equals("%="))
	    type |= NONLIN; 
    }

    public String toString() {
	String res = "";

	if (type == VOID)
	    res = " void("+type+")";
	
	if (isConst())
	    res = " const("+type+")";

	if (isVar())
	    res = " var("+type+")";

	if (isNonLinear())
	    res += " nonlinear";
	
	if (isArith())
	    res += " arithmetic";

	if (isPointer())
	    res += " pointer";
	
	if (isArray())
	    res += " array";

	return res; 
    }
}
