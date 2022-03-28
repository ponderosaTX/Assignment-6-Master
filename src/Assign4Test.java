import java.util.StringTokenizer;
import junit.framework.TestCase;
import java.io.*;
import java.util.concurrent.ExecutionException;

public class Assign4Test extends TestCase {

  public Assign4Test (String name) {
    super(name);
  }
 
 /** The following 9 check methods create an interpreter object with the specified String as the program, invoke the
    * respective evaluation method (valueValue, valueName, valueNeed, etc.), and check that the result matches the
    * (given) expected output.
    */
  private void evalCheck(String name, String answer, String program) {
    Interpreter interp = new Interpreter(new StringReader(program));
    assertEquals("by-value-value " + name, answer, interp.eval().toString());
  }

  public void testNumberP() {
    try {
      String output = "number?";
      String input = "number?";
      evalCheck("numberP", output, input );

    } catch (Exception e) {
      e.printStackTrace();
      fail("numberP threw " + e);
    }
  } //end of func
  

  public void testMathOp() {
    try {
      String output = "18";
      String input = "2 * 3 + 12";
      evalCheck("mathOp", output, input );

    } catch (Exception e) {
      e.printStackTrace();
      fail("mathOp threw " + e);
    }
  } //end of func
  
  
  public void testIfExpr1() {
    try {
      String output = "18";
      String input = "if (4 > 0) then 18 else 0";
      evalCheck("IfExpr1", output, input );

    } catch (Exception e) {
      e.printStackTrace();
      fail("mathOp threw " + e);
    }
  } //end of func 
   
  public void testIfExpr2() {
    try {
      String output = "18";
      String input = "if (4 > 0) then (5)(5) else 18";
      evalCheck("IfExpr2", output, input );
      fail("IfExpr2 did not abort");
      
    } catch (EvalException e) {
    } catch (Exception e) {
      e.printStackTrace();
      fail("IfExpr2 threw " + e);
    }
  } //end of func 
   
   
  public void testParseException() {
    try {
      String output = "haha";
      String input = " 1 +";
      evalCheck("parseException", output, input );
      
      fail("parseException did not throw ParseException exception");
    } catch (ParseException e) {   
      //e.printStackTrace();
      
    } catch (Exception e) {
      e.printStackTrace();
      fail("parseException threw " + e);
    }
  } //end of func
  

  public void testEvalException() {
    try {
      String output = "mojo";
      String input = "1 + number?";
      evalCheck("evalException", output, input );

         fail("evalException did not throw EvalException exception");
      } catch (EvalException e) {   
         //e.printStackTrace();
      
    } catch (Exception e) {
      e.printStackTrace();
      fail("evalException threw " + e);
    }
  } //end of func
  

  public void testAppend() {
    try {
      String output = "(1 2 3 1 2 3)";
      String input = "let Y    := map f to              let g := map x to f(map z1,z2 to (x(x))(z1,z2));     in g(g);  APPEND := map ap to            map x,y to               if x = empty then y else cons(first(x), ap(rest(x), y)); l      := cons(1,cons(2,cons(3,empty))); in (Y(APPEND))(l,l)";
      evalCheck("append", output, input );

    } catch (Exception e) {
      e.printStackTrace();
      fail("append threw " + e);
    }
  } //end of func
  

  public void testLetRec() {
    try {
      String output = "(1 2 3 1 2 3)";
      String input = "letrec append := map x,y to if x = empty then y else cons(first(x), append(rest(x), y)); " +  
                     "in let l := cons(1,cons(2,cons(3,empty))); in append(l,l)";
      evalCheck("letRec", output, input );

    } catch (Exception e) {
      e.printStackTrace();
      fail("letRec threw " + e);
    }
  } //end of func
 

  public void testEmptyBlock() {
    try {
      String output = "0";
      String input = "{ }";
      evalCheck("emptyBlock", output, input );

         fail("emptyBlock did not throw ParseException exception");
      } catch (ParseException e) {   
         //e.printStackTrace();
      
    } catch (Exception e) {
      e.printStackTrace();
      fail("emptyBlock threw " + e);
    }
  } //end of func
  

  public void testBlock() {
    try {
      String output = "1";
      String input = "{3; 2; 1}";
      evalCheck("block", output, input );

    } catch (Exception e) {
      e.printStackTrace();
      fail("block threw " + e);
    }
  } //end of func
  

  public void testDupVar() {
    try {
      String output = "ha!";
      String input = "let x:=3; x:=4; in x";
      evalCheck("dupVar", output, input );

         fail("dupVar did not throw SyntaxException exception");
      } catch (SyntaxException e) {   
         //e.printStackTrace();
      
    } catch (Exception e) {
      e.printStackTrace();
      fail("dupVar threw " + e);
    }
  } //end of func
  

  public void testRefApp() {
    try {
      String output = "(ref 17)";
      String input = "let x := ref 10; in {x <- 17; x}";
      evalCheck("refApp", output, input);  // mutation is lost in nameXXX because the rhs of x is re-evaluated
    } catch (Exception e) {
      e.printStackTrace();
      fail("refApp threw " + e);
    }
  } //end of func
  

  public void testRefref() {
    try {
      String output = "(ref (ref 4))";
      String input = "let x:= ref 4; " +
                     "in let y:= ref x; in y";
      evalCheck("refref", output, input );

    } catch (Exception e) {
      e.printStackTrace();
      fail("refref threw " + e);
    }
  } //end of func
 
  public void testRefP() {
    try {
      String output = "true";
      String input = "ref?(ref 4)";
      evalCheck("refref", output, input );
    } catch (Exception e) {
      e.printStackTrace();
      fail("refref threw " + e);
    }
  } //end of func
  public void testBangApp() {
    try {
      String output = "10";
      String input = "let x := ref 10; in !x";
      evalCheck("bangApp", output, input );

    } catch (Exception e) {
      e.printStackTrace();
      fail("bangApp threw " + e);
    }
  } //end of func

  public void testComplexBang() {
    try {
      String output = "3";
      String input = "let x:= ref 3; in let y:= ref x; in !!y";
      evalCheck("complexBang", output, input);
    } catch (Exception e) {
      e.printStackTrace();
      fail("complexBang threw " + e);
    }
  }

  public void testComplexBlock() {
    try {
      String output = "ruh roh";
      String input = "{let x := ref 10; in x <- 17; x";
      evalCheck("complexBlock", output, input);
      fail("complexBlock didn't throw a ParseException");
    } catch (ParseException e) {
    } catch (Exception e) {
      e.printStackTrace();
      fail("complexBlock threw " + e);
    }
  }
  
  public void testProblem2() {
    try {
      String output = "(100)";
      String input = "letrec maplist := map f,s to if empty?(s) then empty else cons(f(first(s)), maplist(f, rest(s)));" +
                     "in maplist(map x to x*x, maplist(map x to 1000/x, cons(100, empty)))";
      evalCheck("Problem2", output, input);
    } catch (Exception e) {
      e.printStackTrace();
      fail("Problem2 threw " + e);
    }
  }
    
}





