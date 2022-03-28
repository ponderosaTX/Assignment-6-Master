import java.io.*;
import java.util.*;

/** Interpreter Classes */

/** The exception class for Jam run-time errors */
class EvalException extends RuntimeException {
  EvalException(String msg) { super(msg); }
}

/** The visitor interface for interpreting AST's */
interface EvalVisitor extends ASTVisitor<JamVal> {
  
  /** Returns the environment embedded in this EvalVisitor */
  Environment env();
  
  /** Constructs a UnOpVisitor with the specified evaluated arg. */
  UnOpVisitor<JamVal> newUnOpVisitor(JamVal arg);
    
  /** Constructs a BinOpVisitor with the specified unevaluated arguments. */
  BinOpVisitor<JamVal> newBinOpVisitor(AST arg1, AST arg2);
    
  /** Constructs a FunVisitor with the specified array of unevaluated arguments */
  FunVisitor<JamVal> newFunVisitor(AST args[]);
}

/** Jam closure represention for programs with symbolic variables*/
class VarClosure extends JamFun implements Closure {
  private Map map;
  private SymEvaluator eval;
  VarClosure(Map m, SymEvaluator e) { map = m; eval = e; }
  public int arity() { return map.vars().length; }
  public JamVal apply(JamVal[] args) {
    Variable[] vars = map.vars();
    VarEnv newEnv = eval.env();
    int n = vars.length;
    if (n != args.length) throw new EvalException("closure " + this + " applied to " + 
       args.length + " arguments instead of " + n + " arguments");
    for (int i = n-1 ; i >= 0; i--) 
      newEnv = newEnv.cons(new Binding(vars[i],args[i]));
    return map.body().accept((SymASTVisitor<JamVal>) eval.newEvalVisitor(newEnv));
  }                                                 
  public <RtnType> RtnType accept(FunVisitor<RtnType> jfv) { return jfv.forClosure(this); }
  public String toString() { return "VarClosure<" + map + ">"; }
}

/** Jam closure representation for programs with static distance coordinates */
class SDClosure extends JamFun implements Closure {
  private SMap smap;
  private SDEvaluator eval;
  SDClosure(SMap sm, SDEvaluator e) { smap = sm; eval = e; }
  public int arity() { return smap.arity(); }
  public JamVal apply(JamVal[] args) {
    SDEnv newEnv = eval.env();
    int n = smap.arity();
    if (n != args.length) throw new EvalException("closure " + this + " applied to " + 
       args.length + " arguments instead of " + n + " arguments");
      newEnv = newEnv.cons(args);
    return smap.body().accept((SDASTVisitor<JamVal>) eval.newEvalVisitor(newEnv));
  }                                                 
  public <RtnType> RtnType accept(FunVisitor<RtnType> jfv) { return jfv.forClosure(this); }
  public String toString() { return "SDClosure<" + smap + ">"; }
}

class Interpreter {
  
  /** Primary fields */
  private Parser parser;
  private SConverter sConverter;
  
  Interpreter(Parser p) { 
    parser = p;
    sConverter = p.sConverter();
  }
  Interpreter(String fileName) throws IOException { this(new Parser(fileName)); }
  Interpreter(Reader reader) { this(new Parser(reader)); }
    
    /** Parses, checks, and interprets the input embeded in parser */
    public JamVal eval() {
    SymAST prog = parser.checkProg();
    return prog.accept(valueValueVisitor); 
  }
  
  /** Parses and checks the input embeded in parser, converts it to SD form, and interprets it. */
  public JamVal SDEval() {
    SDAST prog = parser.statCheckProg();
    return prog.accept(SDEvalVisitor); 
  }
  /** Parses, checks, CPS converts, and interprets the input embedded in parser using the SymAST representation. */
  public JamVal cpsEval() {
    SymAST prog = parser.cpsProg();
    return prog.accept(valueValueVisitor);
  }
  /** Parses, checks, CPS converts, SD converts, and interprets the input embedded in parser using the SDAST representation. */
  public JamVal SDCpsEval() {
    SDAST prog = parser.statCpsProg();
    return prog.accept(SDEvalVisitor);
  }
  
  /** Renames variables in parsed program, a SymAST, so no variable is shadowed. */
  public SymAST unshadow() {
    SymAST prog = parser.checkProg();
    return prog;
  }
  
  /** Returns the CPS form of the embedded proggram. */
  public SymAST convertToCPS() { return parser.cpsProg(); }
  
  /** Returns the SDAST for the embedded program. */
  public SDAST convertToSD() { 
    SDAST prog = parser.statCheckProg();
    return prog;
  }
  
  /** Visitor that evaluates programs represented in SymAST form. */
  private SymASTVisitor<JamVal> valueValueVisitor = new SymEvaluator(EmptyVarEnv.ONLY);
  
  /** Visitor that evaluates programs represented in SymAST form. */
  private SDASTVisitor<JamVal> SDEvalVisitor = new SDEvaluator(EmptySDEnv.ONLY);
  
  /* Returns parsed program in SymAST form, parsing the program from file if necessary. */
  public SymAST getUSAST() { return parser.checkProg(); }
  
  /* Returns parsed program in SDAST form, parsing the proram from and converting it if necessary. */ 
  public SDAST getSDAST() { return parser.statCheckProg(); }
  
  public String getUSProg() { return getUSAST().toString(); }
  
  public String getRawProg() { return parser.parseProg().toString(); }
  
  public String getSDProg() { return getSDAST().toString(); }
}

/* General visitor class for performing interpretation; defines ComASTVistor methods which are common to SymAST and SDAST representations. */
abstract class Evaluator<Env extends Environment> implements EvalVisitor {
  
  /* Assumes that:
   *   OpTokens are unique
   *   Only objects used as boolean values are BoolConstant.TRUE and BoolConstant.FALSE
   * Hence,  == can be used to compare OpTokens, and BoolConstants.
   */
  
  Env env;  // getter defined below
  
  /* Constructor */
  Evaluator(Env e) { env = e; }
  
  private JamVal[] evalArgs(AST[] args) {
    int n = args.length;
    JamVal[] vals = new JamVal[n];
    for (int i = 0; i < n; i++) vals[i] = args[i].accept(this);
    return vals;
  }
  
  /* EvalVisitor methods */
  
  /** Getter for env field */
  public Env env() { return env; }
  
  /* ASTVisitor methods.  EvalVisitor extends ASTVisitor<JamVal>. */
  public UnOpVisitor<JamVal> newUnOpVisitor(JamVal arg) { return new UnOpEvaluator(arg); } 
  public BinOpVisitor<JamVal> newBinOpVisitor(AST arg1, AST arg2) { return new BinOpEvaluator(arg1, arg2); }  
  public FunVisitor<JamVal> newFunVisitor(AST args[]) {  return new FunEvaluator(evalArgs(args)); }
  
  /* ComASTVisitor<JamVal> methods.  ASTVisitor<JamVal> extends ComASTVisitor<JamVal>. */
  
  /** Method for an aborting error, used in subclasses; package visibility */ 
  JamVal forDefault(AST a) { throw new EvalException(a + " is not in the domain of the visitor " + getClass()); }
  public JamVal forBoolConstant(BoolConstant b) { return b; }
  public JamVal forIntConstant(IntConstant i) { return i; }
  public JamVal forEmptyConstant(EmptyConstant n) { return JamEmpty.ONLY; }
  public JamVal forPrimFun(PrimFun f) { return f; }
  public JamVal forUnOpApp(UnOpApp u) { return u.rator().accept(newUnOpVisitor(u.arg().accept(this))); }
  public JamVal forBinOpApp(BinOpApp b) { return b.rator().accept(newBinOpVisitor(b.arg1(), b.arg2())); }
 
  public JamVal forApp(App a) {
    JamVal rator = a.rator().accept(this);
    if (rator instanceof JamFun)  {
      //System.err.println(Evaluator.this);
      //System.err.println(newFunVisitor(a.args()).getClass());
      return ((JamFun) rator).accept(newFunVisitor(a.args()));
    }
    throw new EvalException(rator + " appears at head of application " + a  + " but it is not a valid function");
  }
  
  public JamVal forIf(If i) {
    JamVal test = i.test().accept(this);
    if (! (test instanceof BoolConstant))
      throw new EvalException("non Boolean " + test + " used as test in if");
    if (test == BoolConstant.TRUE) return i.conseq().accept(this);
    return i.alt().accept(this);
  }

  public JamVal forBlock(Block b) {
    AST[] exps = b.exps();
    int n = exps.length;
    for (int i = 0; i < n-1; i++) exps[i].accept(this);
    return exps[n-1].accept(this);
  }
  
  /* Methods common to SymASTVisitor and SDASTVisitor but not semantically shared. */
  
  /** In either a SymAST or SDAST, evaluating letcc node throws an exception; we are forced to include it our
    * abstract syntax in order to type check shared code.  This construct is supported in this project by 
    * performing a syntactic transformation as part of the the cps transformation that eliminates the letcc construct 
    * from the cpsed code.  Hence, letcc abstract syntax nodes should never appear in code that is being interpreted. */
  public JamVal forLetcc(Letcc host) { return forDefault(host); }
  
  /** Remaining visitor methods are abstract; they are defined differently in SymAST and SDAST evaluation.  For each 
    * form of evaluation, some methods below generate run-time errors because the corresponding nodes do not appear
    * in well-formed instances of those AST types. */;
  abstract public JamVal forVariable(Variable host);
  abstract public JamVal forMap(Map host);
  abstract public JamVal forLet(Let host);
  abstract public JamVal forLetRec(LetRec host);
  
  abstract public JamVal forSDPair(SDPair host);
  abstract public JamVal forSMap(SMap host);
  abstract public JamVal forSLet(SLet host);
  abstract public JamVal forSLetRec(SLetRec host);

  /* Inner classes */
  
  class FunEvaluator implements FunVisitor<JamVal> {
    
    /** Evaluated arguments */
    JamVal[] vals;

    /** number of arguments */
    int arity;
    
    FunEvaluator(JamVal[] jvs) {
      vals = jvs;
      arity = vals.length;
      // System.err.println("FunEvaluator created with vals = " + ToString.toString(vals, ","));
    }
    
    public JamVal forPrimFun(PrimFun primFun) { return primFun.accept(primEvaluator); }
    
    public JamVal forClosure(Closure c) { return c.apply(vals); }
    
    /* Field bound to anonymous inner classes */
    PrimFunVisitor<JamVal> primEvaluator = 
      new PrimFunVisitor<JamVal>() { /* ANONYMOUS CLASS */
      
      private JamVal primFunError(String fn) {
        throw new EvalException("Primitive function `" + fn + "' applied to " + arity + " arguments");
      }
      
      private JamCons toJamCons(JamVal val, String fun) {
        if (val instanceof JamCons) return (JamCons) val;
        throw new EvalException("Primitive function `" + fun + "' applied to argument " + val + " that is not a JamCons");
      }
      
      public JamVal forFunctionPPrim() {
        if (arity != 1) return primFunError("function?");
        return BoolConstant.toBoolConstant(vals[0] instanceof JamFun);
      }
      
      public JamVal forNumberPPrim() {
        if (arity != 1) return primFunError("number?");
        return BoolConstant.toBoolConstant(vals[0] instanceof IntConstant);
      }
      
      public JamVal forListPPrim() {
        if (arity != 1) return primFunError("list?");
        return BoolConstant.toBoolConstant(vals[0] instanceof JamList);
      }
      
      public JamVal forConsPPrim() {
        if (arity != 1) return primFunError("cons?");
        return BoolConstant.toBoolConstant(vals[0] instanceof JamCons);
      }
      
      public JamVal forEmptyPPrim() {
        if (arity != 1) return primFunError("null?");
        return BoolConstant.toBoolConstant(vals[0] instanceof JamEmpty);
      }
      
      public JamVal forConsPrim() {
        if (arity != 2) return primFunError("cons"); 
        if (! (vals[1] instanceof JamList))
          throw new EvalException("Second argument " + vals[1] + " to `cons' is not a JamList");
        return new JamCons(vals[0], (JamList) vals[1]);
      }
      
      public JamVal forArityPrim() { 
        if (arity != 1) return primFunError("arity");
        if (! (vals[0] instanceof JamFun) ) throw new EvalException("arity applied to argument " +
                                                                    vals[0]);
        return ((JamFun) vals[0]).accept(arityEvaluator);
      }
      
      public JamVal forRefPPrim() {
        if (arity != 1) return primFunError("ref?");
//        System.err.println("ref? applied to " + vals[0] + " =  "+ BoolConstant.toBoolConstant(vals[0] instanceof JamRef));
        return BoolConstant.toBoolConstant(vals[0] instanceof JamRef);
      }
        
      public JamVal forFirstPrim() { return toJamCons(vals[0], "first").first(); }
      public JamVal forRestPrim() { return toJamCons(vals[0], "rest").rest(); }
      
      public JamVal forAsBoolPrim() {
        JamVal val = vals[0];
        if (val instanceof BoolConstant) return val;
        else throw new EvalException("The Jam value " + val + " must be of boolean type");
      }
    };
    
    FunVisitor<IntConstant> arityEvaluator = new FunVisitor<IntConstant>() { /* ANONYMOUS CLASS */
      public IntConstant forClosure(Closure jc) { return new IntConstant(jc.arity()); }
      public IntConstant forPrimFun(PrimFun jpf) { return jpf.accept(primArityEvaluator); }
    };
    
    PrimFunVisitor<IntConstant> primArityEvaluator = 
      new PrimFunVisitor<IntConstant>() { /* ANONYMOUS CLASS */
      
      public IntConstant forFunctionPPrim() { return new IntConstant(1); }
      public IntConstant forNumberPPrim() { return new IntConstant(1); }
      public IntConstant forListPPrim() { return new IntConstant(1); }
      public IntConstant forConsPPrim() { return new IntConstant(1); }
      public IntConstant forEmptyPPrim() { return new IntConstant(1); }
      public IntConstant forArityPrim() { return new IntConstant(1); }
      public IntConstant forConsPrim() { return new IntConstant(2); }
      public IntConstant forRefPPrim() { return new IntConstant(1); }
      public IntConstant forFirstPrim() { return new IntConstant(1); }
      public IntConstant forRestPrim() { return new IntConstant(1); }
      public IntConstant forAsBoolPrim() { return new IntConstant(1); }
    };
  }
  
  static class UnOpEvaluator implements UnOpVisitor<JamVal> {
    private JamVal val;
    
    UnOpEvaluator(JamVal jv) { val = jv; }
    
    private IntConstant checkInteger(UnOp op) {
      if (val instanceof IntConstant) return (IntConstant) val;
      throw new EvalException("Unary operator `" + op + "' applied to non-integer " + val);
    }
    
    private BoolConstant checkEval(UnOp op) {
      if (val instanceof BoolConstant) return (BoolConstant) val;
      throw new EvalException("Unary operator `" + op + "' applied to non-boolean " + val);
    }
    
    private JamRef checkRef(UnOp op) {
      if (val instanceof JamRef) return (JamRef) val;
      throw new EvalException("Unary operator `" + op + "' applied to non-reference" + val);
    }
    public JamVal forUnOpPlus(UnOpPlus op) { return checkInteger(op); }
    public JamVal forUnOpMinus(UnOpMinus op) { 
      return new IntConstant(- checkInteger(op).value()); 
    }
    public JamVal forOpTilde(OpTilde op) { return checkEval(op).not(); }
    public JamVal forOpBang(OpBang op) { return checkRef(op).value(); }
    public JamVal forOpRef(OpRef op) { return new JamRef(val); }
  }
  
  class BinOpEvaluator implements BinOpVisitor<JamVal> { 
    private AST arg1, arg2;
    
    BinOpEvaluator(AST a1, AST a2) { arg1 = a1; arg2 = a2; }
    
    private IntConstant evalIntegerArg(AST arg, BinOp b) {
      JamVal val = arg.accept(Evaluator.this);
      if (val instanceof IntConstant) return (IntConstant) val;
      throw new EvalException("Binary operator `" + b + "' applied to non-integer " + val);
    }
    
    private BoolConstant evalBoolArg(AST arg, BinOp b) {
      JamVal val = arg.accept(Evaluator.this);
      if (val instanceof BoolConstant) return (BoolConstant) val;
      throw new EvalException("Binary operator `" + b + "' applied to non-boolean " + val);
    }
    
    public JamVal forBinOpPlus(BinOpPlus op) {
      return new IntConstant(evalIntegerArg(arg1,op).value() + evalIntegerArg(arg2,op).value());
    }
    public JamVal forBinOpMinus(BinOpMinus op) {
      return new IntConstant(evalIntegerArg(arg1,op).value() - evalIntegerArg(arg2,op).value());
    }
    
    public JamVal forOpTimes(OpTimes op) {
      return new IntConstant(evalIntegerArg(arg1,op).value() * evalIntegerArg(arg2,op).value());
    }
    
    public JamVal forOpDivide(OpDivide op) {
      int divisor = evalIntegerArg(arg2,op).value();
      if (divisor == 0) throw new EvalException("Attempt to divide by zero");
      return new IntConstant(evalIntegerArg(arg1,op).value() / divisor);
    }
    
    public JamVal forOpEquals(OpEquals op) {
      return BoolConstant.toBoolConstant(arg1.accept(Evaluator.this).equals(arg2.accept(Evaluator.this)));
    }
    
    public JamVal forOpNotEquals(OpNotEquals op) {
      return BoolConstant.toBoolConstant(! arg1.accept(Evaluator.this).equals(arg2.accept(Evaluator.this)));
    }
    
    public JamVal forOpLessThan(OpLessThan op) {
      return BoolConstant.toBoolConstant(evalIntegerArg(arg1,op).value() < evalIntegerArg(arg2,op).value());
    }
    
    public JamVal forOpGreaterThan(OpGreaterThan op) {
      return BoolConstant.toBoolConstant(evalIntegerArg(arg1,op).value() > evalIntegerArg(arg2,op).value());
    }
    
    public JamVal forOpLessThanEquals(OpLessThanEquals op) {
      return BoolConstant.toBoolConstant(evalIntegerArg(arg1,op).value() <= evalIntegerArg(arg2,op).value());
    }
    
    public JamVal forOpGreaterThanEquals(OpGreaterThanEquals op) {
      return BoolConstant.toBoolConstant(evalIntegerArg(arg1,op).value() >= evalIntegerArg(arg2,op).value());
    }
    
    public JamVal forOpAnd(OpAnd op) {
      BoolConstant b1 = evalBoolArg(arg1,op);
      if (b1 == BoolConstant.FALSE) return BoolConstant.FALSE;
      return evalBoolArg(arg2,op);
    }
    public JamVal forOpOr(OpOr op) {
      BoolConstant b1 = evalBoolArg(arg1,op);
      if (b1 == BoolConstant.TRUE) return BoolConstant.TRUE;
      return evalBoolArg(arg2,op);
    }
    public JamVal forOpGets(OpGets op) {
      JamVal val1 = arg1.accept(Evaluator.this);
      if (! (val1 instanceof JamRef)) throw new EvalException("Left argument " + arg1 + " of <- is not a JamRef");
      ((JamRef) val1).setValue(arg2.accept(Evaluator.this));
      return JamUnit.ONLY;                            
    }
  }
}

class SymEvaluator extends Evaluator<VarEnv> {

  public SymEvaluator(VarEnv e) { super(e); }
  
  /* EvalVisitor methods for SymASTs */
  public SymASTVisitor<JamVal> newEvalVisitor(VarEnv env) { return new SymEvaluator(env); }
  public JamVal forVariable(Variable v) { return env.lookup(v); } 
  public JamVal forMap(Map m) { return new VarClosure(m, this); }   
  public JamVal forLet(Let l) {
    /* Extract binding vars and exps (rhs's) from l */
    Variable[] vars = l.vars();
    SymAST[] exps = l.exps();
    int n = vars.length;
    /* Construct newEnv for Let body and exps; vars are bound to values of corresponding exps using newEvalVisitor */
    VarEnv newEnv = env();
    Binding[] bindings = new Binding[n];
    for (int i = n-1; i >= 0; i--) {
      bindings[i] = new Binding(vars[i], exps[i].accept(this));  // bind var[i] to exps[i] in this evaluator
      newEnv = newEnv.cons(bindings[i]);          
    }
    SymASTVisitor<JamVal> newEvalVisitor = newEvalVisitor(newEnv);
    return l.body().accept(newEvalVisitor);
  }
  
  public JamVal forLetRec(LetRec l) { 
    /* Extract binding vars and exps (rhs's) from l */
    Variable[] vars = l.vars();
    SymAST[] exps = l.exps();
    int n = vars.length;
    /* Construct newEnv for Let body and exps; vars are bound to values of corresponding exps using newEvalVisitor */
    VarEnv newEnv = env();
    
    Binding[] bindings = new Binding[n];
    for (int i = n-1; i >= 0; i--) {
      bindings[i] = new Binding(vars[i], null);  // bind var[i], setting value to null, which is not a JamVal
      newEnv = newEnv.cons(bindings[i]);          
    }
    
    SymASTVisitor<JamVal> newEvalVisitor = newEvalVisitor(newEnv);
    
    // fix up the dummy values
    for (int i = 0; i < n; i++) 
      bindings[i].setBinding(exps[i].accept(newEvalVisitor));  // modifies newEnv and newEvalVisitor
    
    return l.body().accept(newEvalVisitor);
  }
  
  /* EvalVisitor methods for evaluating SDASTs that are never invoked in the evaluation of well-formed SymASTs. */
  public JamVal forSDPair(SDPair host) { return forDefault(host); }
  public JamVal forSMap(SMap host) { return forDefault(host); }
  public JamVal forSLet(SLet host) { return forDefault(host); }
  public JamVal forSLetRec(SLetRec host) { return forDefault(host); } 
}

class SDEvaluator extends Evaluator<SDEnv> implements SDASTVisitor<JamVal> {
  
  SDEvaluator(SDEnv env) { super(env); }
  
  /*  EvalVisitor methods for evaluating SDASTs. */
  public SDASTVisitor<JamVal> newEvalVisitor(SDEnv env) { return null; /* STUB */ }
  public JamVal forSDPair(SDPair p)  { return null; /* STUB */ }
  public JamVal forSMap(SMap sm) { return null; /* STUB */ }
  public JamVal forSLet(SLet sl) { return null; /* STUB */ }
  public JamVal forSLetRec(SLetRec slr) { return null; /* STUB */ }
 
  
  /* Methods that are never invoked in the evaluation of well-formed SymASTs */
  public JamVal forVariable(Variable host) { return forDefault(host); }
  public JamVal forMap(Map host) { return forDefault(host); }
  public JamVal forLet(Let host) { return forDefault(host); }
  public JamVal forLetRec(LetRec host) { return forDefault(host); }
}


