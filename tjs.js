function* tokenize(data) {
  var re_tok = new RegExp([
    '\\(', '\\)',
    '\\[', '\\]',
    '\\s+',       // whitespace
    '"([^"]*)"',  // quoted strings
    '[0-9]+[.0-9]+',  // numbers
    '[a-zA-Z0-9.*/+-]+',  // identifiers
    ';.*?\n',  // comments
  ].join('|'), 'g');

  var tok = {tag:null};
  var match;
  while (match = re_tok.exec(data)) {
    tok.data = null;
    tok.span = [match.index, match.index + match[0].length];
    switch (match[0][0]) {
    case '(': case ')':
    case '[': case ']':
      tok.tag = match[0][0];
      yield tok;
      break;
    case ' ':
    case '\n':
      continue;
    case '"':
      tok.tag = '"';
      tok.data = match[1];
      yield tok;
      break;
    case ';':
      break;
    default:
      if (match[0].match(/^[0-9]/)) {
        tok.tag = 'N';
        tok.data = Number(match[0]);
      } else {
        tok.tag = 'T';
        tok.data = match[0];
      }
      yield tok;
      break;
    }
  }
}

function SExp(tag, val) {
  this.tag = tag;
  this.val = val;
}
SExp.prototype.toString = function() {
  return toStr(this);
};

function parse(data) {
  var tokens = tokenize(data);
  var stack = [];
  var cur = [];
  for (var tok of tokens) {
    switch (tok.tag) {
    case '(':
    case '[':
      stack.push([cur, tok.tag]);
      cur = [];
      break;
    case ')':
      var parent = stack.pop();
      if (parent[1] != '(')
        throw "unmatched paren";
      parent = parent[0];
      parent.push(new SExp('lst', cur));
      cur = parent;
      break;
    case ']':
      var parent = stack.pop();
      if (parent[1] != '[')
        throw "unmatched square at " + tok;
      parent = parent[0];
      parent.push(new SExp('vec', cur));
      cur = parent;
      break;
    case 'T':
      cur.push(new SExp('sym', tok.data));
      break;
    case 'N':
      cur.push(new SExp('num', tok.data));
      break;
    case '"':
      cur.push(new SExp('str', tok.data));
      break;
    default:
      console.log('unhandled tok: ' + tok.tag);
    }
  }
  if (stack.length != 0) {
    throw "unmatched (";
  }
  return cur;
}

function toStr(sexp, types) {
  var str = null;
  switch (sexp.tag) {
  case 'sym': str = sexp.val; break;
  case 'str': str = '"' + sexp.val + '"'; break;
  case 'num': str = '' + sexp.val; break;
  case 'lst':
    str = '(' +
      sexp.val.map(function(s) { return toStr(s, types); }).join(' ') +
      ')';
    break;
  case 'vec':
    str = '[' +
      sexp.val.map(function(s) { return toStr(s, types); }).join(' ') +
      ']';
    break;
  default:
    throw 'unknown type ' + sexp;
  }
  if (types && sexp.type) {
    str += ':' + sexp.type;
  }
  return str;
}

function gen(sexp) {
  switch (sexp.tag) {
    case 'sym': return sexp.val;
    case 'str': return '"' + sexp.val + '"';
    case 'num': return '' + sexp.val;
    case 'lst': return genList(sexp.val);
    case 'vec': return '[' + sexp.val.map(gen).join(', ') + ']';
    default: throw 'unknown type';
  }
}

function genFn(list) {
  var params = list[1].val;
  var body = list.slice(2);
  function genParam(param) {
    return param.val;
  }
  return 'function(' + params.map(genParam).join(', ') + ') {' +
    genSExps(body) + '}';
}

function genList(list) {
  var f = list[0].val;
  var args = list.slice(1);
  if (f[0] == '.') {
    f = list[1].val + f;
    args = list.slice(2);
  }

  var ops = {'+':true};
  var builtins = {'fn': genFn};
  if (f in builtins) {
    return builtins[f](list);
  } else if (f in ops) {
    return args.map(gen).join(f);
  } else {
    return f + '(' + args.map(gen).join(', ') + ')';
  }
}

function genSExps(list) {
  return list.map(gen);
}

function Tbase(name) {
  this.name = name;
}
Tbase.prototype.tag = 'builtin';
Tbase.prototype.toString = function() {
  return this.name;
};
function Tfn(args, ret) {
  this.args = args || [];
  this.ret = ret;
}
Tfn.prototype.tag = 'tfn';
Tfn.prototype.toString = function() {
  return '<fn(' + this.args.join(',') + ')->' + this.ret + '>';
};
function Tvar(num) {
  this.tnum = num;
};
Tvar.prototype.tag = 'tvar';
Tvar.prototype.toString = function() {
  return 't' + this.tnum;
};
function Tapp(f, arg) {
  this.f = f;
  this.arg = arg;
};
function Tarray(t) {
  this.elem = t;
}
Tapp.prototype.tag = 'tapp';
Tapp.prototype.toString = function() {
  return this.f + '<' + this.arg + '>';
};

var tvar = 0;
function gentvar() {
  return new Tvar(tvar++);
}

var TNum = new Tbase('Num');
var TString = new Tbase('String');
var TArray = new Tbase('Array');
function newTArray(arg) {
  return new Tapp(TArray, arg);
}

// "standard library" definitions; outermost environment.
var std = {
  '+': new Tfn([TNum, TNum], TNum),
  'map': function() {
    var a = gentvar();
    var b = gentvar();
    return new Tfn([new Tfn([a], b), newTArray(a)], newTArray(b));
  }(),
  'str': new Tfn([TNum], TString),
};

// Add a 'type' field to sexp and children, gathering type-variable
// constraints as pairs in constraints.
function addTypes(env, sexp, constraints) {
  var type = null;
  switch (sexp.tag) {
  case 'sym':
    if (sexp.val in env) {
      type = env[sexp.val];
    } else {
      throw 'unknown sym ' + sexp.val;
    }
    break;
  case 'str': type = TString; break;
  case 'num': type = TNum; break;
  case 'lst':
    var head = sexp.val[0];
    if (head.val == 'fn') {
      var args = sexp.val[1].val;
      // Create new entry in env for each arg.
      env = Object.create(env);
      args.forEach(function(arg) { env[arg.val] = gentvar(); });

      var body = sexp.val.slice(2);
      body.forEach(function(sexp) { addTypes(env, sexp, constraints); });
      type = new Tfn(args.map(function(arg){ return env[arg.val]; }),
                     body[body.length-1].type);
    } else {
      // Application of head to rest of sexp.
      sexp.val.forEach(function(sexp) { addTypes(env, sexp, constraints); });
      var args = sexp.val.slice(1);

      var fntype = new Tfn(args.map(function(arg) { return arg.type }),
                           gentvar());
      constraints.push([head.type, fntype]);
      type = fntype.ret;
    }
    break;
  case 'vec':
    var elemtype = gentvar();
    sexp.val.forEach(function(sexp) {
      addTypes(env, sexp, constraints);
      constraints.push([elemtype, sexp.type]);
    });
    type = newTArray(elemtype);
    break;
  default: throw 'unknown sexp: ' + sexp.tag + ': ' + sexp;
  }
  if (!type)
    throw 'no type for ' + sexp.tag + ': ' + sexp;
  sexp.type = type;
}

function typeSub(type, a, b) {
  switch (type.tag) {
  case 'builtin': return type;
  case 'tvar':
    if (type == a) return b;
    return type;
  case 'tfn':
    for (var i = 0; i < type.args.length; i++) {
      type.args[i] = typeSub(type.args[i], a, b);
    }
    type.ret = typeSub(type.ret, a, b);
    return type;
  case 'tapp':
    type.f = typeSub(type.f, a, b);
    type.arg = typeSub(type.arg, a, b);
    return type;
  default: throw 'unknown type ' + type.tag;
  }
}

// Unify constraints, producing a map of type variable to type.
function unify(constraints) {
  var subs = [];
  var sub = function(a, b) {
    constraints.forEach(function(c) {
      c[0] = typeSub(c[0], a, b);
      c[1] = typeSub(c[1], a, b);
    });
  };
  while (constraints.length) {
    var constraint = constraints.pop();
    var a = constraint[0];
    var b = constraint[1];
    var sub;
    if (a == b) {
      continue;
    } else if (a.tag == 'tvar') {
      sub(a, b);
      subs.push([a, b]);
    } else if (b.tag == 'tvar') {
      sub(b, a);
      subs.push([b, a]);
    } else if (a.tag == 'tfn' && b.tag == 'tfn') {
      if (a.args.length != b.args.length)
        throw 'arg length mismatch';
      for (var i = 0; i < a.args.length; i++)
        constraints.push([a.args[i], b.args[i]]);
      constraints.push([a.ret, b.ret]);
    } else if (a.tag == 'tapp' && b.tag == 'tapp') {
      constraints.push([a.f, b.f]);
      constraints.push([a.ret, b.ret]);
    } else {
      throw 'unify failure on ' + constraint;
    }
  }
  return subs;
}

function updateSubs(sexp, subs) {
  // Apply all substitutions to sexp's type.
  for (var i = 0, sub; sub = subs[i]; i++) {
    sexp.type = typeSub(sexp.type, sub[0], sub[1]);
  }
  // Recur in children.
  switch (sexp.tag) {
  case 'num': break;
  case 'sym': break;
  case 'vec':
  case 'lst':
    var elems = sexp.val;
    if (sexp.val[0].val == 'fn')
      elems = sexp.val.slice(2);
    for (var i = 0; i < elems.length; ++i) {
      updateSubs(elems[i], subs);
    }
    break;
  default: throw 'unknown tag ' + sexp.tag;
  }
}

function main() {
  var fs = require('fs');

  var data = fs.readFileSync('sample.tjs', 'utf8');
  var sexps = parse(data);
  var sexp = sexps[0];
  var env = std;
  var constraints = [];
  addTypes(env, sexp, constraints);
  console.log('// sexp:', toStr(sexp, true));
  var subs = unify(constraints);
  console.log('subs');
  for (var i = 0; i < subs.length; i++) {
    console.log(subs[i][0], '->', subs[i][1]);
  }
  updateSubs(sexp, subs);
  console.log('// fixed sexp:', toStr(sexp, true));
}

main();

// idea:
//   untyped macro-expansion language, generates
//   typed s-expression language, generates
//   untyped s-expression language, generates
//   javascript
