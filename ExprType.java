public class ExprType {
    private static int _void  = 0; 
    private static int _arith = 1;
    private static int _point = 2;
    private static int _array = 4;
    private static int _var = 8;
    private static int _const = 16;
    private static int _nonlin = 32;

    private int type;

    public int getType() { return type; }

    public void setArith() { type |= _arith; }
    public void setPointer() { type |= _point; }
    public void setArray() { type |= _array; }
    public void setVar() { type |= _var; }
    public void setConst() { type |= _const; }
    public void resetConst() { type &= ~_const; }
    public void setNonLinear() { type |= _nonlin; }

    public boolean isVoid() { return type == _void; }
    public boolean isArith() { return (type & _arith) != 0; }
    public boolean isPointer() { return (type & _point) != 0; }
    public boolean isArray() { return (type & _array) != 0; }
    public boolean isVar() { return (type & _var) != 0; }
    public boolean isConst() { return (type & _const) != 0; }
    public boolean isNonLinear() { return (type & _nonlin) != 0; }

    public ExprType() {
	type = _void; 
    }

    public ExprType(ExprType t) {
	type = (t == null) ? _void : t.getType(); 
    }

    public ExprType(ExprType t1, ExprType t2) {
	type = ((t1 == null) ? _void : t1.getType()) | 
	    ((t2 == null) ? _void : t2.getType()); 

	if ((t1 != null && t1.isVar()) || 
	    (t2 != null && t2.isVar()))
	    resetConst();
    }

    public ExprType(ExprType t1, ExprType t2, ExprType t3) {	
	type = ((t1 == null) ? _void : t1.getType()) | 
	    ((t2 == null) ? _void : t2.getType()) | 
	    ((t3 == null) ? _void : t3.getType());

	if ((t1 != null && t1.isVar()) || 
	    (t2 != null && t2.isVar()) ||
	    (t3 != null && t3.isVar()))
	    resetConst();
    }
}
