Simple scheme to reliably support tail-call elimination.

* Define::

    interface Continue {
      public Object continue();
    }

    class PrimOp {
      ...

      public static Object getResult(Object current) {
        while (true) {
          if (!(current instanceof Continue)) {
            return current;
          }
          current = ((Current) current).continue();
        }
      }

      ...
    }

* Have each function invocation to invoke ``PrimOp.getResult`` after the callee has returned.

* Add a ``tailCall`` primitive to the language which accepts an application, wraps it in a ``Continue`` with
  ``INVOKEDYNAMIC``, and immediately returns it. In the syntax it might look like this::

    @fun(arg1, arg2, ...)

  or maybe if we do not want this "non-local" operation to be so implicit something like::

    tailCall(fun, arg1, arg2, ...)

* In this way, the user can decide when to take the performance hit of wrapping the result in a ``Continue`` but having
  constant stack.

* Now, what's hard to know is how much the impact will be for normal calls -- that is, how much fast the "fast path" of
  ``PrimOp.getResult`` will be. There are a few alternatives:

  - Just have a global variable that tells you whether you should invoke ``getResult`` or not, and use that directly in
    the JVM bytecode and only call ``getResult`` if the variable is not set;

  - Inline the first ``instanceof`` check.

  However, the impact should be pretty easy to verify.

* It's also unclear what the speed of tail calls would be -- again, not too hard to verify. We might win by defining
  a custom bootstrap method that always assumes that calls are fully saturated, which would be the case here.