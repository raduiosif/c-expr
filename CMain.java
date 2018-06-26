import java.io.*;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;

public class CMain {

    static public int LOW = 1;
    static public int MEDIUM = 2;
    static public int HIGH = 3;

    static private boolean isPre = false;
    static public int verbosityLevel = 0;

    public static void driver(String path) throws Exception {      
	File file = new File(path);

	if (file.getName().startsWith("._"))
	    return;

	if (file.isFile()) {
	    if (isPre) {    
		if (!(path.endsWith(".c4")))
		    return;
	    }
	    else if (!(path.endsWith(".c")))
		return;

	    FileInputStream stream = new FileInputStream(path);       
	    ANTLRInputStream input = new ANTLRInputStream(stream);
	    CLexer lexer = new CLexer(input);
	    CommonTokenStream tokens = new CommonTokenStream(lexer);
	    CParser parser = new CParser(tokens);
	
	    System.out.println("parsing " + path + " ...");
	    parser.compilationUnit();
	} 
	else if (file.isDirectory()) {
	    File[] fileList = file.listFiles();
	    
	    for (int i = 0; i < fileList.length; i ++)
		driver(path + "/" + fileList[i].getName());
	} 
    }

    public static void usage() {
	System.out.println("usage: java option CMain <path>");
	System.out.println("options:");
	System.out.println("\t-p: parses preprocessed code .c4 files");
	System.out.println("\t-v N: set verbosity level to N (default 0)");
    }

    public static void output() {
	System.out.println("\nLines:\t\t" + ExprTypeCounter.getLines());
	System.out.println("Arithmetic:\t" + ExprTypeCounter.getArithExpr() + " (" + ExprTypeCounter.getNonLinearExpr() + " nonlinear)");
	System.out.println("Pointers:\t" + ExprTypeCounter.getPointerExpr());
	System.out.println("Arrays:\t\t" + ExprTypeCounter.getArrayExpr());
    }

    public static void parseOptions(String[] args) throws Exception {
	String path = null; 
	boolean setPath = false;
       
	for (int i = 0; i < args.length; i ++) {
	    if (!isPre && args[i].equals("-p"))
		isPre = true;
	    else if (args[i].equals("-v")) {
		if (args.length == ++ i) {
		    usage();
		    return;
		}
		
		verbosityLevel = Integer.parseInt(args[i]);
	    }
	    else if (!setPath) {
		path = args[i];
		setPath = true;
	    }
	    else System.out.println("ignored: " + args[i]);
	}

	driver(path);
	output();
    }

    public static void main(String[] args) throws Exception {
	if (args.length < 1) {
	    usage();
	    return;
	} 

	parseOptions(args);
    }
}
