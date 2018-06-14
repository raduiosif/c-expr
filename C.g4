/*
 [The "BSD licence"]
 Copyright (c) 2013 Sam Harwell
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/** C 2011 grammar built from the C11 Spec */
grammar C;

primaryExpression returns[ExprType type]
    :   Identifier { 
    	$type = new ExprType();
	if ($Identifier.text.endsWith("alloc") || $Identifier.text.equals("free"))
	   $type.setPointer();
    }
    |   Constant { $type = new ExprType(); }
    |   StringLiteral+  { $type = new ExprType(); }
    |   '(' e=expression ')' { $type = $e.type; }
    |   g=genericSelection     { $type = $g.type; }
    |   '__extension__'? '(' compoundStatement ')' { $type = new ExprType(); } // Blocks (GCC extension)
    |   '__builtin_va_arg' '(' e1=unaryExpression ',' typeName ')' { $type = $e1.type; }
    |   '__builtin_offsetof' '(' typeName ',' e1=unaryExpression ')' { $type = $e1.type; }
    ;

genericSelection returns[ExprType type]
    :   '_Generic' '(' e=assignmentExpression ',' genericAssocList ')' { $type = $e.type; }
    ;

genericAssocList
    :   genericAssociation
    |   genericAssocList ',' genericAssociation
    ;

genericAssociation
    :   typeName ':' assignmentExpression
    |   'default' ':' assignmentExpression
    ;

postfixExpression returns [ExprType type]
    :   e=primaryExpression { $type = $e.type; }
    |   postfixExpression '[' e1=expression ']' { $type = new ExprType($e1.type); $type.setArray(); }
    |   p=postfixExpression '(' ')' { $type = $p.type; }
    |   p=postfixExpression '(' l=argumentExpressionList ')' { $type = new ExprType($p.type, $l.type); }
    |   p=postfixExpression '.' Identifier { $type = $p.type; }
    |   p=postfixExpression '->' Identifier { $type = $p.type; $type.setPointer(); }
    |   p=postfixExpression '++' { $type = $p.type; }
    |   p=postfixExpression '--' { $type = $p.type; }
    |   '(' typeName ')' '{' initializerList '}' { $type = new ExprType(); }
    |   '(' typeName ')' '{' initializerList ',' '}' { $type = new ExprType(); }
    |   '__extension__' '(' typeName ')' '{' initializerList '}' { $type = new ExprType(); }
    |   '__extension__' '(' typeName ')' '{' initializerList ',' '}' { $type = new ExprType(); }
    ;

argumentExpressionList returns[ExprType type]
    :   e=assignmentExpression { $type = $e.type; }
    |   l=argumentExpressionList ',' e=assignmentExpression { $type = new ExprType($l.type, $e.type);} 
    ;

unaryExpression returns [ExprType type]
    :   e=postfixExpression { $type = $e.type; }
    |   '++' e1=unaryExpression { $type = new ExprType($e1.type); }
    |   '--' e1=unaryExpression { $type = new ExprType($e1.type); }
    |   unaryOperator e2=castExpression { $type = new ExprType($e2.type); }
    |   'sizeof' unaryExpression { $type = new ExprType(); }
    |   'sizeof' '(' typeName ')' { $type = new ExprType(); }
    |   '_Alignof' '(' typeName ')' { $type = new ExprType(); }
    |   '&&' Identifier { $type = new ExprType(); } // GCC extension address of label 
    ;

unaryOperator
    :   '&' | '*' | '+' | '-' | '~' | '!'
    ;

castExpression returns [ExprType type]
    :   '(' typeName ')' e=castExpression { $type = $e.type; }
    |   '__extension__' '(' typeName ')' e=castExpression { $type = $e.type; }
    |   e1=unaryExpression { $type = $e1.type; }
    |   DigitSequence { $type = new ExprType(); } // for
    ;

multiplicativeExpression returns [ExprType type]
    :   e=castExpression { $type = $e.type; }
    |   e1=multiplicativeExpression '*' e2=castExpression { $type = new ExprType($e1.type, $e2.type); $type.setArith(); }
    |   e1=multiplicativeExpression '/' e2=castExpression { $type = new ExprType($e1.type, $e2.type); $type.setArith(); }
    |   e1=multiplicativeExpression '%' e2=castExpression { $type = new ExprType($e1.type, $e2.type); $type.setArith(); }
    ;

additiveExpression returns [ExprType type]
    :   e=multiplicativeExpression { $type = $e.type; }
    |   e1=additiveExpression '+' e2=multiplicativeExpression { $type = new ExprType($e1.type, $e2.type); $type.setArith(); }
    |   e1=additiveExpression '-' e2=multiplicativeExpression { $type = new ExprType($e1.type, $e2.type); $type.setArith(); }
    ;

shiftExpression returns [ExprType type]
    :   e=additiveExpression { $type = $e.type; }
    |   e1=shiftExpression '<<' e2=additiveExpression { $type = new ExprType($e1.type, $e2.type); }
    |   e1=shiftExpression '>>' e2=additiveExpression { $type = new ExprType($e1.type, $e2.type); }
    ;

relationalExpression returns [ExprType type]
    :   e=shiftExpression { $type = $e.type; }
    |   e1=relationalExpression '<' e2=shiftExpression { $type = new ExprType($e1.type, $e2.type); }
    |   e1=relationalExpression '>' e2=shiftExpression { $type = new ExprType($e1.type, $e2.type); }
    |   e1=relationalExpression '<=' e2=shiftExpression { $type = new ExprType($e1.type, $e2.type); }
    |   e1=relationalExpression '>=' e2=shiftExpression { $type = new ExprType($e1.type, $e2.type); }
    ;

equalityExpression returns [ExprType type]
    :   e=relationalExpression { $type = $e.type; }
    |   e1=equalityExpression '==' e2=relationalExpression { $type = new ExprType($e1.type, $e2.type); }
    |   e1=equalityExpression '!=' e2=relationalExpression { $type = new ExprType($e1.type, $e2.type); }
    ;

andExpression returns [ExprType type]
    :   e=equalityExpression { $type = $e.type; }
    |   e1=andExpression '&' e2=equalityExpression { $type = new ExprType($e1.type, $e2.type); }
    ;

exclusiveOrExpression returns [ExprType type]
    :   e=andExpression { $type = $e.type; }
    |   e1=exclusiveOrExpression '^' e2=andExpression { $type = new ExprType($e1.type, $e2.type); }
    ;

inclusiveOrExpression returns [ExprType type]
    :   e=exclusiveOrExpression { $type = $e.type; }
    |   e1=inclusiveOrExpression '|' e2=exclusiveOrExpression { $type = new ExprType($e1.type, $e2.type); }
    ;

logicalAndExpression returns [ExprType type]
    :   e=inclusiveOrExpression { $type = $e.type; }
    |   e1=logicalAndExpression '&&' e2=inclusiveOrExpression { $type = new ExprType($e1.type, $e2.type); }
    ;

logicalOrExpression returns [ExprType type]
    :   e=logicalAndExpression { $type = $e.type; }
    |   e1=logicalOrExpression '||' e2=logicalAndExpression { $type = new ExprType($e1.type, $e2.type); }
    ;

conditionalExpression returns [ExprType type]
    :   e1=logicalOrExpression {
    	$type = $e1.type;
    }
    |   e1=logicalOrExpression '?' e2=expression ':' e3=conditionalExpression { 
    	$type = new ExprType($e1.type, $e2.type, $e3.type); 
    }
    ;

assignmentExpression returns [ExprType type]
    :   e=conditionalExpression { $type = $e.type; }
    |   e1=unaryExpression op=assignmentOperator e2=assignmentExpression {
    	$type = new ExprType($e1.type, $e2.type);

        if ($op.text.equals("*=") || 
	   $op.text.equals("/=") || 
	   $op.text.equals("%=") || 
	   $op.text.equals("+=") || 
	   $op.text.equals("-="))
           $type.setArith();
    }
    |   DigitSequence { $type = new ExprType(); } // for
    ;

assignmentOperator
    :   '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|='
    ;

expression returns [ExprType type]
    :   e=assignmentExpression { $type = $e.type; }
    |   e1=expression ',' e2=assignmentExpression { $type = new ExprType($e1.type, $e2.type); }
    ;

constantExpression returns [ExprType type]
    :   e=conditionalExpression { $type = $e.type; }
    ;

declaration
    :   declarationSpecifiers initDeclaratorList ';'
    | 	declarationSpecifiers ';'
    |   staticAssertDeclaration
    ;

declarationSpecifiers
    :   declarationSpecifier+
    ;

declarationSpecifiers2
    :   declarationSpecifier+
    ;

declarationSpecifier
    :   storageClassSpecifier
    |   typeSpecifier
    |   typeQualifier
    |   functionSpecifier
    |   alignmentSpecifier
    ;

initDeclaratorList
    :   initDeclarator
    |   initDeclaratorList ',' initDeclarator
    ;

initDeclarator
    :   declarator
    |   declarator '=' initializer
    ;

storageClassSpecifier
    :   'typedef'
    |   'extern'
    |   'static'
    |   '_Thread_local'
    |   'auto'
    |   'register'
    ;

typeSpecifier
    :   ('void'
    |   'char'
    |   'short'
    |   'int'
    |   'long'
    |   'float'
    |   'double'
    |   'signed'
    |   'unsigned'
    |   '_Bool'
    |   '_Complex'
    |   '__m128'
    |   '__m128d'
    |   '__m128i')
    |   '__extension__' '(' ('__m128' | '__m128d' | '__m128i') ')'
    |   atomicTypeSpecifier
    |   structOrUnionSpecifier
    |   enumSpecifier
    |   typedefName
    |   '__typeof__' '(' constantExpression ')' // GCC extension
    |   typeSpecifier pointer
    ;

structOrUnionSpecifier
    :   structOrUnion Identifier? '{' structDeclarationList '}'
    |   structOrUnion Identifier
    ;

structOrUnion
    :   'struct'
    |   'union'
    ;

structDeclarationList
    :   structDeclaration
    |   structDeclarationList structDeclaration
    ;

structDeclaration
    :   specifierQualifierList structDeclaratorList? ';'
    |   staticAssertDeclaration
    ;

specifierQualifierList
    :   typeSpecifier specifierQualifierList?
    |   typeQualifier specifierQualifierList?
    ;

structDeclaratorList
    :   structDeclarator
    |   structDeclaratorList ',' structDeclarator
    ;

structDeclarator
    :   declarator
    |   declarator? ':' constantExpression
    ;

enumSpecifier
    :   'enum' Identifier? '{' enumeratorList '}'
    |   'enum' Identifier? '{' enumeratorList ',' '}'
    |   'enum' Identifier
    ;

enumeratorList
    :   enumerator
    |   enumeratorList ',' enumerator
    ;

enumerator
    :   enumerationConstant
    |   enumerationConstant '=' constantExpression
    ;

enumerationConstant
    :   Identifier
    ;

atomicTypeSpecifier
    :   '_Atomic' '(' typeName ')'
    ;

typeQualifier
    :   'const'
    |   'restrict'
    |   'volatile'
    |   '_Atomic'
    ;

functionSpecifier
    :   ('inline'
    |   '_Noreturn'
    |   '__inline__' // GCC extension
    |   '__stdcall')
    |   gccAttributeSpecifier
    |   '__declspec' '(' Identifier ')'
    ;

alignmentSpecifier
    :   '_Alignas' '(' typeName ')'
    |   '_Alignas' '(' constantExpression ')'
    ;

declarator
    :   pointer? directDeclarator gccDeclaratorExtension*
    ;

directDeclarator
    :   Identifier
    |   '(' declarator ')'
    |   directDeclarator '[' typeQualifierList? ']'
    |   directDeclarator tok='[' typeQualifierList? e=assignmentExpression ']' { 
        if (CMain.verbosityLevel >= CMain.MEDIUM)	
	   ExprTypeCounter.printType($tok.getLine(), $e.type);

    	ExprTypeCounter.incType($e.type); 
    }
    |   directDeclarator '[' tok='static' typeQualifierList? e=assignmentExpression ']' { 
        if (CMain.verbosityLevel >= CMain.MEDIUM)	
	   ExprTypeCounter.printType($tok.getLine(), $e.type);

    	ExprTypeCounter.incType($e.type); 
    }
    |   directDeclarator '[' typeQualifierList tok='static' e=assignmentExpression ']' { 
        if (CMain.verbosityLevel >= CMain.MEDIUM)	
	   ExprTypeCounter.printType($tok.getLine(), $e.type);

    	ExprTypeCounter.incType($e.type); 
    }
    |   directDeclarator '[' typeQualifierList? '*' ']'
    |   directDeclarator '(' parameterTypeList ')'
    |   directDeclarator '(' identifierList? ')'
    |   Identifier ':' DigitSequence  // bit field
    |   '(' typeSpecifier? pointer directDeclarator ')' // function pointer like: (__cdecl *f)
    ;

gccDeclaratorExtension
    :   '__asm' '(' StringLiteral+ ')'
    |   gccAttributeSpecifier
    ;

gccAttributeSpecifier
    :   '__attribute__' '(' '(' gccAttributeList ')' ')'
    ;

gccAttributeList
    :   gccAttribute (',' gccAttribute)*
    |   // empty
    ;

gccAttribute
    :   ~(',' | '(' | ')') // relaxed def for "identifier or reserved word"
        ('(' argumentExpressionList? ')')?
    |   // empty
    ;

nestedParenthesesBlock
    :   (   ~('(' | ')')
        |   '(' nestedParenthesesBlock ')'
        )*
    ;

pointer
    :   '*' typeQualifierList?
    |   '*' typeQualifierList? pointer
    |   '^' typeQualifierList? // Blocks language extension
    |   '^' typeQualifierList? pointer // Blocks language extension
    ;

typeQualifierList
    :   typeQualifier
    |   typeQualifierList typeQualifier
    ;

parameterTypeList
    :   parameterList
    |   parameterList ',' '...'
    ;

parameterList
    :   parameterDeclaration
    |   parameterList ',' parameterDeclaration
    ;

parameterDeclaration
    :   declarationSpecifiers declarator
    |   declarationSpecifiers2 abstractDeclarator?
    ;

identifierList
    :   Identifier
    |   identifierList ',' Identifier
    ;

typeName
    :   specifierQualifierList abstractDeclarator?
    ;

abstractDeclarator
    :   pointer
    |   pointer? directAbstractDeclarator gccDeclaratorExtension*
    ;

directAbstractDeclarator
    :   '(' abstractDeclarator ')' gccDeclaratorExtension*
    |   '[' typeQualifierList? ']'
    |   tok='[' typeQualifierList? e=assignmentExpression ']' { 
        if (CMain.verbosityLevel >= CMain.MEDIUM)	
	   ExprTypeCounter.printType($tok.getLine(), $e.type);

    	ExprTypeCounter.incType($e.type); 
    }
    |   '[' tok='static' typeQualifierList? e=assignmentExpression ']' { 
        if (CMain.verbosityLevel >= CMain.MEDIUM)	
	   ExprTypeCounter.printType($tok.getLine(), $e.type);

    	ExprTypeCounter.incType($e.type); 
    }
    |   '[' typeQualifierList tok='static' e=assignmentExpression ']' { 
        if (CMain.verbosityLevel >= CMain.MEDIUM)	
	   ExprTypeCounter.printType($tok.getLine(), $e.type);

    	ExprTypeCounter.incType($e.type); 
    }
    |   '[' '*' ']'
    |   '(' parameterTypeList? ')' gccDeclaratorExtension*
    |   directAbstractDeclarator '[' typeQualifierList? ']'
    |   directAbstractDeclarator tok='[' typeQualifierList? e=assignmentExpression ']' { 
        if (CMain.verbosityLevel >= CMain.MEDIUM)	
	   ExprTypeCounter.printType($tok.getLine(), $e.type);

    	ExprTypeCounter.incType($e.type); 
    }
    |   directAbstractDeclarator '[' tok='static' typeQualifierList? e=assignmentExpression ']' { 
        if (CMain.verbosityLevel >= CMain.MEDIUM)	
	   ExprTypeCounter.printType($tok.getLine(), $e.type);

    	ExprTypeCounter.incType($e.type); 
    }
    |   directAbstractDeclarator '[' typeQualifierList tok='static' e=assignmentExpression ']' { 
        if (CMain.verbosityLevel >= CMain.MEDIUM)	
	   ExprTypeCounter.printType($tok.getLine(), $e.type);

    	ExprTypeCounter.incType($e.type); 
    }
    |   directAbstractDeclarator '[' '*' ']'
    |   directAbstractDeclarator '(' parameterTypeList? ')' gccDeclaratorExtension*
    ;

typedefName
    :   Identifier
    ;

initializer
    :   e=assignmentExpression { 
        if (CMain.verbosityLevel >= CMain.MEDIUM)	
	   ExprTypeCounter.printType(0, $e.type);

    	ExprTypeCounter.incType($e.type); 
    }
    |   '{' initializerList '}'
    |   '{' initializerList ',' '}'
    ;

initializerList
    :   designation? initializer
    |   initializerList ',' designation? initializer
    ;

designation
    :   designatorList '='
    ;

designatorList
    :   designator
    |   designatorList designator
    ;

designator
    :   '[' constantExpression ']'
    |   '.' Identifier
    ;

staticAssertDeclaration
    :   '_Static_assert' '(' constantExpression ',' StringLiteral+ ')' ';'
    ;

statement
    :   labeledStatement
    |   compoundStatement
    |   expressionStatement
    |   selectionStatement
    |   iterationStatement
    |   jumpStatement
    |   ('__asm' | '__asm__') ('volatile' | '__volatile__') '(' (logicalOrExpression (',' logicalOrExpression)*)? (':' (logicalOrExpression (',' logicalOrExpression)*)?)* ')' ';'
    ;

labeledStatement
    :   Identifier ':' statement
    |   'case' constantExpression ':' statement
    |   'default' ':' statement
    ;

compoundStatement
    :   '{' blockItemList? '}'
    ;

blockItemList
    :   blockItem
    |   blockItemList blockItem
    ;

blockItem
    :   statement
    |   declaration
    ;

expressionStatement
    : ';'
    |   e=expression tok=';' { 
        if (CMain.verbosityLevel >= CMain.MEDIUM)	
	   ExprTypeCounter.printType($tok.getLine(), $e.type);

    	ExprTypeCounter.incType($e.type); 
    }
    ;

selectionStatement
    :   tok='if' '(' e=expression ')' statement ('else' statement)? { 
          if (CMain.verbosityLevel >= CMain.MEDIUM) 
	      ExprTypeCounter.printType($tok.getLine(), $e.type);

    	ExprTypeCounter.incType($e.type); 
    }
    |   tok='switch' '(' e=expression ')' statement { 
          if (CMain.verbosityLevel >= CMain.MEDIUM) 
	      ExprTypeCounter.printType($tok.getLine(), $e.type);

    	ExprTypeCounter.incType($e.type); 
    }
    ;

iterationStatement
    :   tok=While '(' e=expression ')' statement { 
          if (CMain.verbosityLevel >= CMain.MEDIUM) 
	      ExprTypeCounter.printType($tok.getLine(), $e.type);

    	ExprTypeCounter.incType($e.type); 
    }
    |   tok=Do statement While '(' e=expression ')' ';' { 
          if (CMain.verbosityLevel >= CMain.MEDIUM) 
	      ExprTypeCounter.printType($tok.getLine(), $e.type);

    	  ExprTypeCounter.incType($e.type); 
    }
    |   For '(' forCondition ')' statement
    ;

forCondition
	:   forDeclaration ';' forExpression? ';' forExpression?
	|   expression? ';' forExpression? ';' forExpression?
	;

forDeclaration
    :   declarationSpecifiers initDeclaratorList
	| 	declarationSpecifiers
    ;

forExpression
    :   e=assignmentExpression { 
        if (CMain.verbosityLevel >= CMain.MEDIUM)	
	   ExprTypeCounter.printType(0, $e.type);

    	ExprTypeCounter.incType($e.type); 
    }
    |   forExpression tok=',' e=assignmentExpression { 
        if (CMain.verbosityLevel >= CMain.MEDIUM)	
	   ExprTypeCounter.printType($tok.getLine(), $e.type);

    	ExprTypeCounter.incType($e.type); 
    }
    ;

jumpStatement
    :   'goto' Identifier ';'
    |   'continue' ';'
    |   'break' ';'
    |   'return' ';'
    |   tok='return' e=expression ';' { 
       	if (CMain.verbosityLevel >= CMain.MEDIUM) 
	   ExprTypeCounter.printType($tok.getLine(), $e.type);

    	ExprTypeCounter.incType($e.type); 
    }
    |   'goto' unaryExpression ';' // GCC extension
    ;

compilationUnit
    :   translationUnit? EOF { ExprTypeCounter.incLines($EOF.getLine()); }
    ;

translationUnit
    :   externalDeclaration
    |   translationUnit externalDeclaration
    ;

externalDeclaration
    :   functionDefinition
    |   declaration
    |   ';' // stray ;
    ;

functionDefinition
    :   declarationSpecifiers? declarator declarationList? compoundStatement
    ;

declarationList
    :   declaration
    |   declarationList declaration
    ;

Auto : 'auto';
Break : 'break';
Case : 'case';
Char : 'char';
Const : 'const';
Continue : 'continue';
Default : 'default';
Do : 'do';
Double : 'double';
Else : 'else';
Enum : 'enum';
Extern : 'extern';
Float : 'float';
For : 'for';
Goto : 'goto';
If : 'if';
Inline : 'inline';
Int : 'int';
Long : 'long';
Register : 'register';
Restrict : 'restrict';
Return : 'return';
Short : 'short';
Signed : 'signed';
Sizeof : 'sizeof';
Static : 'static';
Struct : 'struct';
Switch : 'switch';
Typedef : 'typedef';
Union : 'union';
Unsigned : 'unsigned';
Void : 'void';
Volatile : 'volatile';
While : 'while';

Alignas : '_Alignas';
Alignof : '_Alignof';
Atomic : '_Atomic';
Bool : '_Bool';
Complex : '_Complex';
Generic : '_Generic';
Imaginary : '_Imaginary';
Noreturn : '_Noreturn';
StaticAssert : '_Static_assert';
ThreadLocal : '_Thread_local';

LeftParen : '(';
RightParen : ')';
LeftBracket : '[';
RightBracket : ']';
LeftBrace : '{';
RightBrace : '}';

Less : '<';
LessEqual : '<=';
Greater : '>';
GreaterEqual : '>=';
LeftShift : '<<';
RightShift : '>>';

Plus : '+';
PlusPlus : '++';
Minus : '-';
MinusMinus : '--';
Star : '*';
Div : '/';
Mod : '%';

And : '&';
Or : '|';
AndAnd : '&&';
OrOr : '||';
Caret : '^';
Not : '!';
Tilde : '~';

Question : '?';
Colon : ':';
Semi : ';';
Comma : ',';

Assign : '=';
// '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|='
StarAssign : '*=';
DivAssign : '/=';
ModAssign : '%=';
PlusAssign : '+=';
MinusAssign : '-=';
LeftShiftAssign : '<<=';
RightShiftAssign : '>>=';
AndAssign : '&=';
XorAssign : '^=';
OrAssign : '|=';

Equal : '==';
NotEqual : '!=';

Arrow : '->';
Dot : '.';
Ellipsis : '...';

Identifier
    :   IdentifierNondigit
        (   IdentifierNondigit
        |   Digit
        )*
    ;

fragment
IdentifierNondigit
    :   Nondigit
    |   UniversalCharacterName
    //|   // other implementation-defined characters...
    ;

fragment
Nondigit
    :   [a-zA-Z_]
    ;

fragment
Digit
    :   [0-9]
    ;

fragment
UniversalCharacterName
    :   '\\u' HexQuad
    |   '\\U' HexQuad HexQuad
    ;

fragment
HexQuad
    :   HexadecimalDigit HexadecimalDigit HexadecimalDigit HexadecimalDigit
    ;

Constant
    :   IntegerConstant
    |   FloatingConstant
    //|   EnumerationConstant
    |   CharacterConstant
    ;

fragment
IntegerConstant
    :   DecimalConstant IntegerSuffix?
    |   OctalConstant IntegerSuffix?
    |   HexadecimalConstant IntegerSuffix?
    |	BinaryConstant
    ;

fragment
BinaryConstant
	:	'0' [bB] [0-1]+
	;

fragment
DecimalConstant
    :   NonzeroDigit Digit*
    ;

fragment
OctalConstant
    :   '0' OctalDigit*
    ;

fragment
HexadecimalConstant
    :   HexadecimalPrefix HexadecimalDigit+
    ;

fragment
HexadecimalPrefix
    :   '0' [xX]
    ;

fragment
NonzeroDigit
    :   [1-9]
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
HexadecimalDigit
    :   [0-9a-fA-F]
    ;

fragment
IntegerSuffix
    :   UnsignedSuffix LongSuffix?
    |   UnsignedSuffix LongLongSuffix
    |   LongSuffix UnsignedSuffix?
    |   LongLongSuffix UnsignedSuffix?
    ;

fragment
UnsignedSuffix
    :   [uU]
    ;

fragment
LongSuffix
    :   [lL]
    ;

fragment
LongLongSuffix
    :   'll' | 'LL'
    ;

fragment
FloatingConstant
    :   DecimalFloatingConstant
    |   HexadecimalFloatingConstant
    ;

fragment
DecimalFloatingConstant
    :   FractionalConstant ExponentPart? FloatingSuffix?
    |   DigitSequence ExponentPart FloatingSuffix?
    ;

fragment
HexadecimalFloatingConstant
    :   HexadecimalPrefix HexadecimalFractionalConstant BinaryExponentPart FloatingSuffix?
    |   HexadecimalPrefix HexadecimalDigitSequence BinaryExponentPart FloatingSuffix?
    ;

fragment
FractionalConstant
    :   DigitSequence? '.' DigitSequence
    |   DigitSequence '.'
    ;

fragment
ExponentPart
    :   'e' Sign? DigitSequence
    |   'E' Sign? DigitSequence
    ;

fragment
Sign
    :   '+' | '-'
    ;

DigitSequence
    :   Digit+
    ;

fragment
HexadecimalFractionalConstant
    :   HexadecimalDigitSequence? '.' HexadecimalDigitSequence
    |   HexadecimalDigitSequence '.'
    ;

fragment
BinaryExponentPart
    :   'p' Sign? DigitSequence
    |   'P' Sign? DigitSequence
    ;

fragment
HexadecimalDigitSequence
    :   HexadecimalDigit+
    ;

fragment
FloatingSuffix
    :   'f' | 'l' | 'F' | 'L'
    ;

fragment
CharacterConstant
    :   '\'' CCharSequence '\''
    |   'L\'' CCharSequence '\''
    |   'u\'' CCharSequence '\''
    |   'U\'' CCharSequence '\''
    ;

fragment
CCharSequence
    :   CChar+
    ;

fragment
CChar
    :   ~['\\\r\n]
    |   EscapeSequence
    ;

fragment
EscapeSequence
    :   SimpleEscapeSequence
    |   OctalEscapeSequence
    |   HexadecimalEscapeSequence
    |   UniversalCharacterName
    ;

fragment
SimpleEscapeSequence
    :   '\\' ['"?abfnrtv\\]
    ;

fragment
OctalEscapeSequence
    :   '\\' OctalDigit
    |   '\\' OctalDigit OctalDigit
    |   '\\' OctalDigit OctalDigit OctalDigit
    ;

fragment
HexadecimalEscapeSequence
    :   '\\x' HexadecimalDigit+
    ;

StringLiteral
    :   EncodingPrefix? '"' SCharSequence? '"'
    ;

fragment
EncodingPrefix
    :   'u8'
    |   'u'
    |   'U'
    |   'L'
    ;

fragment
SCharSequence
    :   SChar+
    ;

fragment
SChar
    :   ~["\\\r\n]
    |   EscapeSequence
    |   '\\\n'   // Added line
    |   '\\\r\n' // Added line
    ;

ComplexDefine
    :   '#' Whitespace? 'define'  ~[#]*
        -> skip
    ;
         
// ignore the following asm blocks:
/*
    asm
    {
        mfspr x, 286;
    }
 */
AsmBlock
    :   'asm' ~'{'* '{' ~'}'* '}'
	-> skip
    ;
	
// ignore the lines generated by c preprocessor                                   
// sample line : '#line 1 "/home/dm/files/dk1.h" 1'                           
LineAfterPreprocessing
    :   '#line' Whitespace* ~[\r\n]*
        -> skip
    ;  

LineDirective
    :   '#' Whitespace? DecimalConstant Whitespace? StringLiteral ~[\r\n]*
        -> skip
    ;

PragmaDirective
    :   '#' Whitespace? 'pragma' Whitespace ~[\r\n]*
        -> skip
    ;

Whitespace
    :   [ \t]+
        -> skip
    ;

Newline
    :   (   '\r' '\n'?
        |   '\n'
        )
        -> skip
    ;

BlockComment
    :   '/*' .*? '*/'
        -> skip
    ;

LineComment
    :   '//' ~[\r\n]*
        -> skip
    ;

PreProcessing
   :    '#' ~[\r\n]* 
   	-> skip
   ;

