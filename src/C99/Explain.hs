-- | @c99mtlc --explain E0102@: what a code means, and what to do about it.
--
-- Modelled on @rustc --explain@ by way of the sibling Mettle compiler's
-- src/error/error_explain.c. Every entry is meaning, then an example, then the
-- fix, because the reader is stuck and wants the third part.
--
-- Codes are stable. Once a number is published, its meaning does not change:
-- tests grep for them and people write them down.
module C99.Explain
  ( explain
  , explainIndex
  , codeTitle
  ) where

import Data.Char (isSpace, toUpper)

-- | @(code, one-line title, body)@.
entries :: [(String, String, [String])]
entries =
  [ ( "E0001"
    , "unexpected character in the source"
    , [ "The lexer met a byte that cannot begin any C token, such as '@',"
      , "'$' or a stray backslash."
      , ""
      , "    int x = 1 @ 2;"
      , ""
      , "Usually it is a typo for an operator, a smart quote pasted from a"
      , "document, or a stray character left by an editor. Delete it, or write"
      , "the operator you meant."
      , ""
      , "The lexer skips the character and keeps going, so any other errors"
      , "reported alongside this one are real."
      ]
    )
  , ( "E0002"
    , "unterminated literal or comment"
    , [ "A string, character constant or block comment reached the end of the"
      , "line or file without closing."
      , ""
      , "    const char *s = \"hello;"
      , ""
      , "Add the closing quote or '*/'. A string that really does span lines"
      , "either ends each line with a backslash, or is written as adjacent"
      , "literals:"
      , ""
      , "    const char *s = \"one\""
      , "                    \"two\";"
      ]
    )
  , ( "E0010"
    , "expected one token, found another"
    , [ "The parser needed a particular token here and something else came."
      , ""
      , "    int f(void) { return 1 }"
      , "                          ^ expected ';'"
      , ""
      , "Nearly always a missing ';', ')' or '}' on this line or the one"
      , "before it."
      , ""
      , "After reporting, the parser skips to the next ';' or '}' and carries"
      , "on, so one missing semicolon reports once rather than cascading down"
      , "the file."
      ]
    )
  , ( "E0011"
    , "expected an expression"
    , [ "An operator or '(' was followed by something that cannot start an"
      , "expression."
      , ""
      , "    int x = 1 + ;"
      , ""
      , "Fill in the missing operand, or delete the dangling operator."
      ]
    )
  , ( "E0012"
    , "expected a declaration at file scope"
    , [ "Only declarations and definitions may appear outside a function."
      , ""
      , "    int x = 1;"
      , "    x = 2;          /* a statement, not a declaration */"
      , ""
      , "Move the statement into a function. If it was meant as a definition,"
      , "give it a type."
      ]
    )
  , ( "E0013"
    , "expected an integer constant expression"
    , [ "Array bounds outside a function, enum values, bit-field widths and"
      , "case labels must be constants the compiler can fold."
      , ""
      , "    int n = 4;"
      , "    int a[n];       /* fine inside a function (a VLA), not at file scope */"
      , ""
      , "Use a literal, an enum constant, or #define."
      ]
    )
  , ( "E0100"
    , "redefinition"
    , [ "A name was defined twice in one scope."
      , ""
      , "    int x = 1;"
      , "    int x = 2;"
      , ""
      , "Rename one, or make the second a plain assignment. A declaration you"
      , "want in several files belongs in a header as 'extern', with exactly"
      , "one definition in one .c file."
      ]
    )
  , ( "E0102"
    , "use of an undeclared identifier"
    , [ "The name is not in scope here."
      , ""
      , "    return coutner + 1;"
      , ""
      , "Check the spelling; the compiler suggests a near match when it finds"
      , "one. Otherwise declare it above this point, or include the header"
      , "that declares it."
      ]
    )
  , ( "E0103"
    , "call to an undeclared function"
    , [ "C99 removed the implicit 'int' declaration C89 allowed, so a function"
      , "must be declared before it is called."
      , ""
      , "    puts(\"hi\");     /* without #include <stdio.h> */"
      , ""
      , "Include the header that declares it, or write a prototype."
      ]
    )
  , ( "E0110"
    , "wrong number of arguments"
    , [ "The call does not match the prototype."
      , ""
      , "    int add(int a, int b);"
      , "    add(1, 2, 3);"
      , ""
      , "The note on the diagnostic points at the declaration that decides how"
      , "many arguments are expected."
      ]
    )
  , ( "E0120"
    , "assignment to something that is not an lvalue"
    , [ "The left side of '=' must name an object."
      , ""
      , "    f() = 1;"
      , "    3 = x;"
      , ""
      , "A common cause is '=' where '==' was meant inside an if."
      ]
    )
  , ( "E0200"
    , "unsupported construct"
    , [ "The construct is valid C99 that this compiler does not lower yet."
      , ""
      , "This is a gap in c99mtlc, not a mistake in your program. The message"
      , "names the construct; rewriting around it is the only workaround."
      ]
    )
  ]

codeTitle :: String -> Maybe String
codeTitle c = lookup3 (normalize c)
  where
    lookup3 k = case [t | (code, t, _) <- entries, code == k] of
      (t : _) -> Just t
      [] -> Nothing

-- | Accepts @e0102@ and @[E0102]@ too. Someone copying a code out of terminal
-- output should not have to think about brackets.
normalize :: String -> String
normalize = map toUpper . strip . filter (`notElem` "[]")
  where
    strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

explain :: String -> Maybe [String]
explain c =
  case [(code, t, b) | (code, t, b) <- entries, code == normalize c] of
    ((code, t, b) : _) -> Just (("error[" ++ code ++ "]: " ++ t) : "" : b)
    [] -> Nothing

explainIndex :: [String]
explainIndex =
  "Error codes. Run `c99mtlc --explain <CODE>` for any of them."
    : ""
    : [ "  " ++ code ++ "  " ++ t | (code, t, _) <- entries
      ]
