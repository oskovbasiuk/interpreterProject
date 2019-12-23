const SYMBOL = /[_a-zA-Z]/;
const NUMBER = /[.0-9]/;
const OPERATOR = /[\+\-\*\/><]/;
const WHITESPACE = /[ \s]/;
const SPECIAL = /[(){},:;=]/;
const STRGAP = /['"]/;

var isOperator = function(c) {
    return OPERATOR.test(c);
  },
  isDigit = function(c) {
    return NUMBER.test(c);
  },
  isWhiteSpace = function(c) {
    return WHITESPACE.test(c);
  },
  isSpecial = function(c) {
    return SPECIAL.test(c);
  },
  isStrGap = function(c) {
    return STRGAP.test(c);
  },
  isSymbol = function(c) {
    return SYMBOL.test(c);
  };

class Parser {
  constructor(tok, stop_at) {
    this.counter = -1;
    this.tokens = tok;
    this.stop_at = stop_at;
    this.special = 0;
  }

  move_next() {
    return this.tokens[++this.counter];
  }

  peek_next() {
    var i = this.counter + 1;
    return this.tokens[i];
  }
  multi_expr(divider, end) {
    var ret = [];
    //failIfAtEnd
    var typ = this.peek_next().type;
    if (typ == end) {
      this.move_next();
    } else {
      var arg_parser = new Parser(this.tokens, {
        divider: divider,
        end: end
      });
      arg_parser.counter = this.counter;
      var p;
      while (typ != end) {
        p = arg_parser.next_expr();
        if (typeof p != "undefined") {
          ret.push(p);
        }
        typ = arg_parser.peek_next().type;
        arg_parser.move_next();
        //failIfAtEnd
      }
      this.counter = arg_parser.counter;
    }
    return ret;
  }

  next_expr(prev) {
    //failIfAtEnd
    if (this.peek_next()) var curr = this.peek_next();
    else return prev;

    var [typ, value] = [curr.type, curr.value];

    if (typ == this.stop_at.end || typ == this.stop_at.divider) {
      return prev;
    }
    curr = this.move_next();
    var nxt = {};
    var args;
    if ((typ == "number" || typ == "string" || typ == "symbol") && typeof prev == "undefined") {
      return this.next_expr({
        type: typ,
        value: value
      });
    } else if (typ == "operator") {
      nxt = this.next_expr();
      return this.next_expr({
        type: "operation",
        value: value,
        prev: prev,
        nxt: nxt
      });
    } else if (typ == "(") {
      args = this.multi_expr(",", ")");
      return this.next_expr({
        type: "call",
        prev: prev,
        args: args
      });
    } else if (typ == "{") {
      var params = prev;
      var body = this.multi_expr(";", "}");
      return this.next_expr({
        type: "function",
        params: params,
        body: body
      });
    } else if (typ == "=") {
      if (prev.type != "symbol") {
        throw "Assign error";
      }
      nxt = this.next_expr();
      return this.next_expr({
        type: "assignment",
        prev: prev,
        nxt: nxt
      });
    } else {
      var msg = `Unexpected token -> ${typ} ${value} `;
      throw msg;
    }
  }
}

function lex(input) {
  var tokens = [],
    i = 0,
    char = "";
  var advance = function() {
    return (char = input[++i]);
  };
  var addToken = function(value, type) {
    tokens.push({
      type,
      value
    });
  };

  var scanString = function(c) {
      var string = "";
      c = advance();
      while (c != '"' && c != "'") {
        string += c;
        c = advance();
        if (typeof c == "undefined" || i >= input.length) {
          throw "String error";
        }
      }
      c = advance();
      return string;
    },
    scanNumber = function(c) {
      var number = "";
      var dots = 0;
      while (NUMBER.test(c)) {
        number += c;
        if (c == ".") {
          ++dots;
        }
        c = advance();
      }
      if (dots > 1 || i >= input.length) throw "Number error";
      return number;
    },
    scanIdentifier = function(c) {
      var identifier = "";
      while (SYMBOL.test(c)) {
        identifier += c;
        c = advance();
      }
      if (i >= input.length) throw " Identifier error";
      return identifier;
    };
  while (i < input.length) {
    char = input[i];
    if (isWhiteSpace(char)) {
      advance();
    } else if (isOperator(char)) {
      addToken(char, "operator");
      advance();
    } else if (isSpecial(char)) {
      addToken("", char);
      advance();
    } else if (isStrGap(char)) {
      addToken(scanString(char), "string");
    } else if (isDigit(char)) {
      addToken(scanNumber(char), "number");
    } else if (isSymbol(char)) {
      addToken(scanIdentifier(char), "symbol");
    } else {
      throw "error";
    }
  }
  return tokens;
}

function parse(tokens) {
  var parser = new Parser(tokens, {
    divider: ";",
    end: ";"
  });
  var p = [];
  i = 0;
  while (typeof parser.peek_next() != "undefined") {
    p.push(parser.next_expr());
    if (typeof p[i] != "undefined") {
      ++i;
    }
    parser.move_next();
  }
  return p;
}

//--------------------------------------------------------------------------------------------------------------

class Env {
  constructor(parent) {
    this.items = {};
    this.parent = parent;
  }
  getItem(name) {
    if (this.items.hasOwnProperty(name)) {
      return this.items[name];
    } else if (typeof this.parent != "undefined") {
      return this.parent.getItem(name);
    } else {
      return undefined;
    }
  }
  setItem(name, value) {
    this.items[name] = value;
  }
}

function eval_expr(expression, env) {
  var typ = expression.type;
  if (typ == "number") {
    return {
      type: "number",
      value: parseFloat(expression.value)
    };
  } else if (typ == "string") {
    return {
      type: "string",
      value: expression.value
    };
  } else if (typ == "undefined") {
    return {
      type: "undefined"
    };
  } else if (typ == "operation") {
    return _operation(expression, env);
  } else if (typ == "symbol") {
    var name = expression.value;
    var ret = env.getItem(name);
    if (typeof ret == "undefined") {
      throw `Error no such variable ${name}`;
    }
    return ret;
  } else if (typ == "assignment") {
    var vName = expression.prev.value;
    var value = eval_expr(expression.nxt, env);
    env.setItem(vName, value);
    return value;
  } else if (typ == "call") {
    return _function_call(expression, env);
  } else if (typ == "function") {
    env.setItem(expression.params.prev.value, expression);
    if (expression.params.prev.value == "if") {
      _function_call(expression.params, env);
    } else if (expression.params.prev.value == "ifLoop") {
      _function_call(expression.params, env);
    }
    return {
      type: "function",
      params: expression.params,
      body: expression.body,
      environment: new Env(env)
    };
  } else {
    throw "unknown expression";
  }
}

function _operation(expression, env) {
  var arg1 = eval_expr(expression.prev, env);
  var arg2 = eval_expr(expression.nxt, env);
  if (expression.value == "+") {
    var tp = "number";
    if (arg1.type == "string" || arg2.type == "string") {
      tp = "string";
    }
    return {
      type: tp,
      value: arg1.value + arg2.value
    };
  } else if (expression.value == "-") {
    return {
      type: "number",
      value: arg1.value - arg2.value
    };
  } else if (expression.value == "*") {
    return {
      type: "number",
      value: arg1.value * arg2.value
    };
  } else if (expression.value == "/") {
    return {
      type: "number",
      value: arg1.value / arg2.value
    };
  } else if (expression.value == ">") {
    return {
      type: "number",
      value: arg1.value > arg2.value
    };
  } else if (expression.value == "<") {
    return {
      type: "number",
      value: arg1.value < arg2.value
    };
  } else if (expression.value == "==") {
    //REWORK
    return {
      type: "number",
      value: arg1.value / arg2.value
    };
  } else {
    throw "Unknown operation";
  }
}

function _function_call(expression, env) {
  var func = eval_expr(expression.prev, env);
  var args = [];

  for (const arg of expression.args) {
    args.push(eval_expr(arg, env));
  }

  if (func.type == "function") {
    var par = func.params.args;
    var body = func.body;
    func.environment = new Env(env);
    var new_env = func.environment;
    var isCustomFunc = false;

    if (expression.prev.value == "print") {
      isCustomFunc = true;
      for (const iterator of args) {
        console.log(iterator.value);
      }
      return "undefined";
    } else if (expression.prev.value == "searchIndexes") {
      isCustomFunc = true;
      var searchStr = args[0];
      var searchIn = args[1];
      if (typeof searchIn == "undefined") {
        return "undefined";
      }

      var indexesOf = [];
      var expr = new RegExp(searchStr.value, "g");
      var arr = searchIn.value.match(expr);
      var i = -1;
      for (const iterator of arr) {
        i = searchIn.value.indexOf(iterator, i + 1);
        indexesOf.push(i);
      }

      return {
        type: "number",
        value: indexesOf
      };
    } else if (expression.prev.value == "searchElements") {
      isCustomFunc = true;
      var searchStr = args[0];
      var searchIn = args[1];
      if (typeof searchIn == "undefined") {
        return "undefined";
      }

      var expr = new RegExp(searchStr.value, "g");
      var arr = searchIn.value.match(expr);
      if (arr == null) {
        return "undefined";
      }

      return {
        type: "string",
        value: arr
      };
    } else if (expression.prev.value == "indexAccess") {
      isCustomFunc = true;
      var str = args[0];
      var index = args[1];
      if (typeof index == "undefined" || index.value < 0 || index.value > str.value.length - 1) {
        return "undefined";
      }
      return {
        type: "string",
        value: str.value[index.value]
      };
    } else if (expression.prev.value == "indexCount") {
      isCustomFunc = true;
      var str = args[0];
      if (str.value.length < 1) {
        return "undefined";
      }
      return {
        type: "string",
        value: str.value.length
      };
    } else if (expression.prev.value == "if") {
      isCustomFunc = true;
      var i = 0;
      if (args[0] != undefined) {
        for (const iterator of args) {
          if (iterator.type == "number") {
            if (i == 0) {
              break;
            } else {
              iterator.type = "string";
              iterator.value = toString(iterator.value);
            }
          }
          if (!(iterator.type == "string") || args[0].value != iterator.value) {
            return "undefined";
          }
          ++i;
        }

        for (const iterator of args) {
          if (iterator.value == false) {
            return "undefined";
          }
        }
      } else {
        return "undefined";
      }
    } else if (expression.prev.value == "ifLoop") {
      isCustomFunc = true;
      while (eval_expr(expression.args[0], env).value != false) {
        for (const iterator of body) {
          eval_expr(iterator, env);
        }
      }
      return "undefined";
    }

    if (!isCustomFunc)
      for (var i = 0; i < par.length; ++i) {
        new_env.setItem(par[i].value, args[i]);
      }

    var funcEnd;
    if (isCustomFunc) {
      new_env = env;
    }
    for (const iterator of body) {
      funcEnd = eval_expr(iterator, new_env);
    }
    return funcEnd;
  } else {
    throw "Not a function!";
  }
}

function preprocessing(file) {
  var insPat = /#insert \(\w+\.txt\)/;
  var Found;
  var FS = require("fs");
  while ((Found = file.match(insPat))) {
    var fileName = Found[0].slice(Found[0].indexOf("(") + 1, Found[0].indexOf(")"));
    var insertFile;
    if (FS.existsSync("./" + fileName)) {
      insertFile = FS.readFileSync("./" + fileName, "utf8");
      file = file.replace(Found[0], insertFile);
    } else {
      file = file.replace(Found[0], "");
    }
  }
  return file;
}

//----------------MAIN PROCESS----------------
var FileSys = require("fs");
var mainFile = "./main.txt";

if (FileSys.existsSync(mainFile)) {
  var Program = FileSys.readFileSync(mainFile, "utf8");
  Program = preprocessing(Program);
  var tree = parse(lex(Program));
  var environment = new Env();
  environment.setItem("print", {
    type: "function",
    params: {},
    body: []
  });
  environment.setItem("searchIndexes", {
    type: "function",
    params: {},
    body: []
  });
  environment.setItem("searchElements", {
    type: "function",
    params: {},
    body: []
  });
  environment.setItem("indexAccess", {
    type: "function",
    params: {},
    body: []
  });
  environment.setItem("indexCount", {
    type: "function",
    params: {},
    body: []
  });
  for (const iterator of tree) {
    eval_expr(iterator, environment);
  }
} else {
  console.log("No such main file.");
}
