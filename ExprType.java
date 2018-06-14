public class ExprType {
    private static int _void  = 0; 
    private static int _arith = 1;
    private static int _point = 2;
    private static int _array = 4;

    private int type;

    public int getType() { return type; }

    public void setArith() { type |= _arith; }
    public void setPointer() { type |= _point; }
    public void setArray() { type |= _array; }

    public boolean isVoid() { return type == _void; }
    public boolean isArith() { return (type & _arith) != 0; }
    public boolean isPointer() { return (type & _point) != 0; }
    public boolean isArray() { return (type & _array) != 0; }

    public ExprType() {
	type = _void; 
    }

    public ExprType(ExprType t) {
	type = (t == null) ? _void : t.getType(); 
    }

    public ExprType(ExprType t1, ExprType t2) {
	type = ((t1 == null) ? _void : t1.getType()) | 
	    ((t2 == null) ? _void : t2.getType()); 
    }

    public ExprType(ExprType t1, ExprType t2, ExprType t3) {	
	type = ((t1 == null) ? _void : t1.getType()) | 
	    ((t2 == null) ? _void : t2.getType()) | 
	    ((t3 == null) ? _void : t3.getType());
    }
}
