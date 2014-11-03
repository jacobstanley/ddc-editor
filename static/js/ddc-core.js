// Date:   3 Nov 2014
// Author: Jacob Stanley
//
// This is a javascript port of DDC.Core.Lexer
//

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

CodeMirror.defineMode("ddc-core", function(_config, modeConfig) {

  function switchState(source, setState, f) {
    setState(f);
    return f(source, setState);
  }

  var varStartRE = /[a-z?_]/;
  var varBodyRE  = /[a-zA-Z0-9_'$]/;

  var conStartRE = /[A-Z]/;
  var conBodyRE  = /[a-zA-Z0-9_]/;

  var opStartRE  = /[-~!@#$%&*+=:\/|<>]/;
  var opBodyRE   = /[-~!@#$%^&*+=:?\/|<>]/;

  var litStartRE = /[-0-9]/;
  var litBodyRE  = /[0-9boxwfi.#']/;

  var digitRE    = /[0-9]/;
  var parensRE   = /[(){}[\]]/;
  var punctRE    = /[.,;\\]/;

  var spaceRE    = /[ \t]/;

  function normal(source, setState) {
    //
    // Whitespace
    //
    if (source.eatWhile(spaceRE)) {
      return null;
    }

    var ch = source.next();

    //
    // Literal values
    //

    if (digitRE.test(ch)) {
      source.eatWhile(litBodyRE);
      return "number";
    }

    if (ch == '-' && source.eat(digitRE)) {
      source.eatWhile(litBodyRE);
      return "number";
    }

    //
    // Meta tokens
    //

    if (ch == '{' && source.eat('-')) {
      var t = "comment";
      return switchState(source, setState, ncomment(t, 1));
    }

    if (ch == '-' && source.eat('-')) {
      source.skipToEnd();
      return "comment";
    }

    //
    // Wrapper operator symbols
    //

    if (ch == '(') {
      var p = source.pos;
      if (source.eat(opStartRE)) {
        source.eatWhile(opBodyRE);
        if (source.eat(')')) {
          return "operator";
        }
      }
      source.pos = p;
    }

    //
    // The unit data constructor
    //

    if (ch == '(') {
      if (source.eat(')')) {
        return "property";
      }
    }

    //
    // Compound parens
    //

    if (ch == '[' && source.eat(':')) {
      return "bracket";
    }

    if (ch == ':' && source.eat(']')) {
      return "bracket";
    }

    if (ch == '{' && source.eat(':')) {
      return "bracket";
    }

    if (ch == ':' && source.eat('}')) {
      return "bracket";
    }

    //
    // Function constructors
    //

    if (ch == '~' && source.eat('>')) {
      return "bracket";
    }

    if (ch == '-' && source.eat('>')) {
      return "bracket";
    }

    if (ch == '<' && source.eat('-')) {
      return "bracket";
    }

    if (ch == '=' && source.eat('>')) {
      return "bracket";
    }

    //
    // Compound symbols
    //

    if (ch == '/' && source.eat('\\')) {
      return "bracket";
    }

    //
    // Debruijn indices
    //

    if (ch == '^' && source.eat(digitRE)) {
      return "atom";
    }

    //
    // Parens
    //

    if (parensRE.test(ch)) {
      return "bracket";
    }

    //
    // Punctuation
    //

    if (punctRE.test(ch)) {
      return "bracket";
    }

    //
    // Operator symbols
    //

    if (opStartRE.test(ch)) {
      var p = source.pos;
      source.eatWhile(opBodyRE);
      if (ch == ':' && source.pos == p) {
        return "bracket";
      }
      return "operator";
    }

    //
    // Operator body symbols
    //

    if (ch == '^') {
      return "operator";
    }

    //
    // Named constructors
    //

    if (conStartRE.test(ch)) {
      source.eatWhile(conBodyRE);
      if (source.eat('#')) {
        return "variable-3";
      }
      source.eat('\'');
      return "variable-2";
    }

    //
    // Named variables
    //

    if (varStartRE.test(ch)) {
      var p = source.pos;
      source.eatWhile(varBodyRE);
      if (source.eat('#')) {
        return "property";
      }
      if (p == 1) {
        return "variable";
      }
      return null;
    }

    //
    // Error
    //

    return "error";
  }

  function ncomment(type, nest) {
    if (nest == 0) {
      return normal;
    }
    return function(source, setState) {
      var currNest = nest;
      while (!source.eol()) {
        var ch = source.next();
        if (ch == '{' && source.eat('-')) {
          ++currNest;
        }
        else if (ch == '-' && source.eat('}')) {
          --currNest;
          if (currNest == 0) {
            setState(normal);
            return type;
          }
        }
      }
      setState(ncomment(type, currNest));
      return type;
    };
  }

  var wellKnownWords = (function() {
    var wkw = {};
    function setType(t) {
      return function () {
        for (var i = 0; i < arguments.length; i++)
          wkw[arguments[i]] = t;
      };
    }

    // bottoms
    setType("builtin")(
        "Pure",
        "Empty");

    // expression keywords
    setType("keyword")(
        "module",
        "import",
        "export",
        "foreign",
        "type",
        "value",
        "data",
        "with",
        "where",
        "in",
        "let",
        "letcase",
        "letrec",
        "private",
        "extend",
        "using",
        "withregion",
        "case",
        "of",
        "weakeff",
        "weakclo",
        "purify",
        "forget",
        "box",
        "run");

    // sugar keywords
    setType("keyword")(
        "do",
        "match",
        "else");

    // built-in sort constructors
    setType("builtin")(
        "Prop",
        "Comp");

    // built-in kind constructors
    setType("builtin")(
        "Witness",
        "Data",
        "Region",
        "Effect",
        "Closure");

    // built-in witness type constructors
    setType("builtin")(
        "Global",
        "DeepGlobal",
        "Const",
        "DeepConst",
        "Mutable",
        "DeepMutable",
        "Lazy",
        "HeadLazy",
        "Manifest",
        "Purify",
        "Emptify",
        "Disjoint",
        "Distinct");

    // built-in type constructors
    setType("builtin")(
        "Unit",
        "S",
        "Read",
        "HeadRead",
        "DeepRead",
        "Write",
        "DeepWrite",
        "Alloc",
        "DeepAlloc",
        "Use",
        "DeepUse");

    // built-in witness constructor
    setType("builtin")(
        "empty",
        "use",
        "read",
        "alloc");

    var override = modeConfig.overrideKeywords;
    if (override) for (var word in override) if (override.hasOwnProperty(word))
      wkw[word] = override[word];

    return wkw;
  })();



  return {
    startState: function ()  { return { f: normal }; },
    copyState:  function (s) { return { f: s.f }; },

    token: function(stream, state) {
      var t = state.f(stream, function(s) { state.f = s; });
      var w = stream.current();
      return wellKnownWords.hasOwnProperty(w) ? wellKnownWords[w] : t;
    },

    blockCommentStart: "{-",
    blockCommentEnd: "-}",
    lineComment: "--"
  };

});

CodeMirror.defineMIME("text/x-ddc-core", "ddc-core");

});
