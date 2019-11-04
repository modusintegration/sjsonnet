package sjsonnet

import utest._

object DefaultExprTests extends TestSuite{
  def eval(s: String) = {
    new Interpreter(
      SjsonnetMain.createParseCache(),
      Map(),
      Map(),
      DummyPath(),
      (_, _) => None
    ).interpret(s, DummyPath("(memory)")) match{
      case Right(x) => x
      case Left(e) => throw new Exception(e)
    }
  }
  def tests = Tests{
    test("defaultExpr") {
      eval("local obj = {}; obj.nonExistent default \"HelloWorld\" ").value.toString() ==> "HelloWorld"
      eval("local obj = {}; obj.nonExistent default false || true ").value ==> true
      eval("local obj = {}; 1 + obj.nonExistent default 2").value ==> 3
      eval("local obj = {}; (1 + obj.nonExistent) default 2").value ==> 2
      eval("local obj = { a: 1 }; obj.nonExistent default obj.a default obj.anotherNonExistent").value ==> 1
      eval("local obj = { a: 1 }; obj.nonExistent default obj.anotherNonExistent default obj.a").value ==> 1
    }
  }
}
