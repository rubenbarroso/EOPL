From SICP:

"set! is a special form, where <name> is a symbol and <new-value> is any
 expression. set! changes <name> so that its value is the result obtained
 by evaluating <new-value>"

From TSPLv3, http://www.scheme.com/tspl3/binding.html

"set! does not establish a new binding for var but rather alters the value of
 an existing binding. It first evaluates exp, then assigns var to the value of
 exp. Any subsequent reference to var within the scope of the altered binding
 evaluates to the new value."

Therefore, we don't need to extend the definitions of occurs free and occurs
bound since set! only changes the denotation of the variable, not its binding.
