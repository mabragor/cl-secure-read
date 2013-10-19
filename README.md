cl-secure-read
==============

Secure the lisp reader in spirit of Let Over Lambda. See section "Reader Security" on www.letoverlambda.com
to get the initial idea.

Example:

```lisp
CL-USER> (ql:quickload 'cl-secure-read)
CL-USER> (in-package cl-secure-read)
;; Define a function DEFAULT-RFS, which is a restricted version of READ-FROM-STRING
CL-SECURE-READ> (define-secure-read-from-string default-rfs :fail-value "caboom!")
CL-SECURE-READ> (default-rfs "123") ; this will read in number 123, as expected ...
;; ... and this will hopefully just return "caboom!",
;; not executing the removal shell-command.
CL-SECURE-READ> (default-rfs "#.(shell-eval \"rm -rf ./\"")
```

Now exports 4 macro:

  *  DEFINE-SECURE-READ-FROM-STRING - defines a function, which acts exactly like READ-FROM-STRING,
     only some macro-characters and dispatch-macro-characters (such as read-eval sequence #.) are disabled.
  *  DEFINE-SECURE-READ - same for READ or READ-PRESERVING-WHITESPACE
  *  SECURE-READ-FROM-STRING-LAMBDA - do not define READ-FROM-STRING-like function globally, but
     return a lambda instead
  *  SECURE-READ-LAMBDA - same for READ and READ-PRESERVING-WHITESPACE

Default behaviour is to take a standard readtable, force STANDARD-IO-SYNTAX, disable \*READ-EVAL\*,
and disable all macro-characters except #\' #\, #\( and #\`.

Another readtable may be used instead of standard one by specifying :READTABLE keyword to DEFINE-SAFE-READER.

What macro-characters are enabled/disabled may be controlled by either specifying :BLACKLIST and :WHITELIST
parameters of a macro explicitly, or by wrapping DEFINE-SAFE-READER form in let, which
binds SAFE-READ-FROM-STRING-WHITELIST and SAFE-READ-FROM-STRING-BLACKLIST.

The syntax of these bindings is best shown by an example

        ;; Define tightened-up version of READ-FROM-STRING ...
        (let ((safe-read-from-string-whitelist '(#\; #\! (#\# #\.) :allow-read-eval :keep-io-syntax)))
          (define-secure-read-from-string not-so-strict-read-from-string :readtable :clesh :fail-value "caboom!"))

        ;; ... and later use it in couple different ways
        (not-so-strict-read-from-string "asdf") ; this will read-in symbol ASDF
        (not-so-strict-read-from-string "#(1 2 3)") ; and this will result in "caboom!"
        (let (*read-eval)
          (not-so-strict-read-from-string "#.(1 2 3)")) ; "caboom!", disabling *READ-EVAL* dynamically

In this example, the :clesh-readtable is used a basis of a restricted readtable.
In the input, single-line comments are allowed, read-eval is not explicitly set to nil (:ALLOW-READ-EVAL),
io-syntax is not set to standard one (:KEEP-IO-SYNTAX), which means, that you can control, for example,
whether *READ-EVAL* is actually turned on in NOT-SO-STRICT-READ-FROM-STRING by
wrapping a call to it into (LET ((*READ-EVAL* ...)), that is at runtime.

Note, how #\. dispatching macro-character, defining read-eval syntax, is enabled by specifying
a list '(#\# #\.). In general, if white/blacklist contains a sublist, its first character
is interpreted as a "main" dispatching character (like #\#), while other characters are interpreted
as dispatch-macro-chars to allow/deny, that correspond to this "main" char.

Black/white-list can contain:
  * characters, which a interpreted as macro-characters to deny/allow
  * lists of characters, which a interpreted as (macro-char ,@sub-macro-chars), where
    sub-macro-chars is a list of dispatch-macro-chars to deny/allow, which correspond to the given macro-char
    (usually #\#)
  * special keywords, which for now are the following
    :allow-read-eval - do not bind *READ-EVAL* to NIL explicitly
    :keep-io-syntax - do not wrap a call to READ-FROM-STRING into WITH-STANDARD-IO-SYNTAX
    :lists - allow/deny #\) and #\(
    :quotes - allow/deny #\` #\' and #\,
    All other keywords are ignored.
   

If BLACKLIST is NIL, all the macro-characters and dispatching macro-characters of the readtable
are disabled, unless they are specified in the WHITELIST.