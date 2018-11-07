package org.francesco.asmlambda.compiler

import java.lang.invoke.MethodType
import java.lang.reflect.Method

import org.francesco.asmlambda.compiler.Syntax.{Expr => SE}
import org.francesco.asmlambda.compiler.{Syntax => S}
import org.francesco.asmlambda.runtime
import org.objectweb.asm.{Handle, Label}

import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.collection.mutable.ArraySeq
import scala.language.reflectiveCalls

/** Scope checks an expression and prepares it for compilation. right now this just means
  * removing any kind of shadowing.
  */
object Rename {
  type Counters = Map[String, Int]

  private def varName(counters: Counters, v: String): String = {
    val counter = counters(v)
    if (counter == 0) {
      v
    } else {
      v + "$" + counter.toString
    }
  }

  private def bumpVar(counters: Counters, v: String): (Counters, String) = {
    val newCounters: Counters = counters.get(v) match {
      case None => counters + (v -> 0)
      case Some(counter) => counters + (v -> (counter + 1))
    }
    (newCounters, varName(newCounters, v))
  }

  private def telescope(counters: Counters, vars: ArraySeq[String]): (Counters, ArraySeq[String]) = {
    val (newCounters, reverseVars) = vars.foldLeft((counters, List.empty[String])) {
      case ((counters, varsSoFar), v) =>
        val (newCounters, newV) = bumpVar(counters, v)
        (newCounters, newV :: varsSoFar)
    }
    (newCounters, ArraySeq(reverseVars.reverse: _*))
  }

  def expr(counters: Counters, e: Syntax.Expr): Syntax.Expr =
    e match {
      case SE.Var(v) => SE.Var(varName(counters, v))
      case SE.Record(fields) => SE.Record(fields.mapValues(expr(counters, _)))
      case SE.Lookup(rec, fld) => SE.Lookup(expr(counters, rec), fld)
      case SE.Update(rec, fld, body) => SE.Update(expr(counters, rec), fld, expr(counters, body))
      case SE.Lam(args, body) =>
        val (newCounters, newArgs) = telescope(counters, args)
        SE.Lam(newArgs, expr(newCounters, body))
      case SE.App(fun, args) =>
        SE.App(expr(counters, fun), args.map(expr(counters, _)))
      case SE.Prim(prim) => SE.Prim(prim)
      case SE.PrimOp(pop) => SE.PrimOp(pop)
      case SE.ITE(cond, l, r) =>
        SE.ITE(expr(counters, cond), expr(counters, l), expr(counters, r))
      case SE.Let(v, bound, body) =>
        val (newCounters, newV) = bumpVar(counters, v)
        SE.Let(newV, expr(counters, bound), expr(newCounters, body))
      case SE.Def(v, defn, body) =>
        val newDefn = definition(counters, defn)
        val (newCounters, newV) = bumpVar(counters, v)
        SE.Def(newV, newDefn, expr(newCounters, body))
    }

  /** does two things:
    * * makes all definition names (global and local) globally unique;
    * * removes all shadowing from let- and lam-bound variables.
    */
  def definition(counters: Counters, defn: Syntax.Definition): Syntax.Definition = {
    val (newCounters, defnVars) = telescope(counters, defn.args)
    S.Definition(defnVars, expr(newCounters, defn.body))
  }

  def `package`(pkg: Syntax.Package): Syntax.Package = {
    // do not shadow definition names, either
    val counters: Counters = Map(pkg.defs.keys.map((_, 0)).toSeq: _*)
    S.Package(pkg.defs.mapValues(definition(counters, _)), expr(counters, pkg.body))
  }
}

private sealed class LambdaLift() {
  import Compiler.{Expr => E, _}
  import Compiler.Expr
  import LambdaLift._

  val generatedDefnCounters: mutable.Map[String, Int] = mutable.Map.empty
  val definitions: mutable.Map[String, Definition] = mutable.Map.empty

  def newDefinitionName(defn: String): String = {
    val count = generatedDefnCounters.getOrElse(defn, 0)
    generatedDefnCounters += (defn -> (count + 1))
    defn + "$" + count.toString
  }

  /** returns the new expressions and the free variables therein. */
  def expr(containingDefn: String, names: Names, e: Syntax.Expr): (Expr, Set[String]) =
    e match {
      case SE.Var(v) =>
        names(v) match {
          case Name.Var => (E.Var(v), Set(v))
          case Name.Def(fullDefName, capturedArgs, argsArity) =>
            (E.Def(fullDefName, capturedArgs, argsArity), Set.empty)
        }
      case SE.Record(flds) =>
        val (newFlds, freeVars) =
          flds.foldLeft((Map.empty[String, Expr], Set.empty[String])) {
            case ((newFlds, freeVars), (lbl, fldBody)) =>
              val (newFldBody, fldBodyVars) = expr(containingDefn, names, fldBody)
              (newFlds + (lbl -> newFldBody), freeVars ++ fldBodyVars)
          }
        (E.Record(newFlds), freeVars)
      case SE.Lookup(rec, lbl) =>
        val (newRec, freeVars) = expr(containingDefn, names, rec)
        (E.Lookup(newRec, lbl), freeVars)
      case SE.Update(rec, fld, body) =>
        val (newRec, freeVars1) = expr(containingDefn, names, rec)
        val (newBody, freeVars2) = expr(containingDefn, names, body)
        (E.Update(newRec, fld, newBody), freeVars1 ++ freeVars2)
      case SE.Lam(args, body) =>
        // create a new definition with on the fly with all the free variables of the body as captured
        // arg
        val bodyNames = Map(args.map(arg => (arg, Name.Var)).toSeq: _*)
        val (newBody, bodyFreeVars0) = expr(containingDefn, names ++ bodyNames, body)
        val bodyFreeVars = bodyFreeVars0 &~ Set(args: _*)
        val capturedArgs = ArraySeq(bodyFreeVars.toSeq: _*)
        val defName = newDefinitionName(containingDefn)
        definitions += (defName -> Definition(capturedArgs, args, newBody))
        (E.Def(defName, capturedArgs, args.length), bodyFreeVars)
      case SE.App(fun, args) =>
        val (newArgsReversed, argsFreeVars) = args.foldLeft((List.empty[Expr], Set.empty[String])) {
          case ((newArgs, argsFreeVars), arg) =>
            val (newArg, argFreeVars) = expr(containingDefn, names, arg)
            (newArg :: newArgs, argsFreeVars ++ argFreeVars)
        }
        val newArgs = ArraySeq(newArgsReversed.reverse: _*)
        lazy val default = {
          val (newFun, funFreeVars) = expr(containingDefn, names, fun)
          (E.DynamicCall(newFun, newArgs), argsFreeVars ++ funFreeVars)
        }
        fun match {
          case SE.PrimOp(pop) => (E.PrimOpCall(pop, newArgs), argsFreeVars)
          case SE.Var(v) =>
            names(v) match {
              case Name.Def(defName, capturedArgs, argsArity@_) => // TODO assert argsArity == args
                (E.StaticCall(defName, capturedArgs, newArgs), argsFreeVars)
              case Name.Var => default
            }
          case _ => default
        }
      case SE.Prim(prim) => (E.Prim(prim), Set.empty)
      case SE.PrimOp(pop) => sys.error(s"naked primop $pop")
      case SE.ITE(cond, l, r) =>
        val (newCond, condVars) = expr(containingDefn, names, cond)
        val (newL, lVars) = expr(containingDefn, names, l)
        val (newR, rVars) = expr(containingDefn, names, r)
        (E.ITE(newCond, newL, newR), condVars ++ lVars ++ rVars)
      case SE.Let(v, bound, body) =>
        val (newBound, boundFreeVars) = expr(containingDefn, names, bound)
        val (newBody, bodyFreeVars0) = expr(containingDefn, names + (v -> Name.Var), body)
        val bodyFreeVars = bodyFreeVars0 - v
        (E.Let(v, newBound, newBody), boundFreeVars ++ bodyFreeVars)
      case SE.Def(partialDefName, Syntax.Definition(args, bound), e) =>
        val defName = containingDefn + ":" + partialDefName
        val boundNames = Map(args.map(arg => (arg, Name.Var)): _*)
        val (newBound, boundFreeVars0) = expr(defName, names ++ boundNames, bound)
        val boundFreeVars = boundFreeVars0 &~ Set(args: _*)
        val capturedArgs = ArraySeq(boundFreeVars.toSeq: _*)
        definitions += (defName -> Definition(capturedArgs, args, newBound))
        expr(containingDefn, names + (partialDefName -> Name.Def(defName, capturedArgs, args.length)), e)
    }

  /** names is passed here because it must contain all the other definitions in the package */
  def definition(defName: String, names: Names, defn: Syntax.Definition): Unit = {
    val boundNames = Map(defn.args.map(arg => (arg, Name.Var)): _*)
    val (newBound, boundFreeVars0) = expr(defName, names ++ boundNames, defn.body)
    val boundFreeVars = boundFreeVars0 &~ Set(defn.args: _*)
    if (boundFreeVars.nonEmpty) {
      sys.error(s"Free variables $boundFreeVars in definition $defName!")
    }
    definitions += (defName -> Definition(ArraySeq.empty, defn.args, newBound))
  }
}

object LambdaLift {
  import Compiler._

  private sealed trait Name
  private object Name {

    /** this name derives from a let- or lam-bound variable */
    case object Var extends Name

    /** this name comes from a definition, with the given captured args applied.
      * note that this has the ful
      */
    case class Def(fullDefName: String, capturedArgs: ArraySeq[String], argsArity: Int) extends Name
  }
  private type Names = Map[String, Name]

  def `package`(pkg: Syntax.Package): Package = {
    val ll = new LambdaLift()
    val names0: Map[String, Name] = Map(pkg.defs.toSeq.map {
      case (defName, defn) => (defName, Name.Def(defName, ArraySeq.empty, defn.args.length))
    }: _*)
    pkg.defs.foreach { case (defName, defn) => ll.definition(defName, names0, defn) }
    val mainExpr = ll.expr("main", names0, pkg.body)._1
    Package(ll.definitions.toMap, mainExpr)
  }
}

object Compiler {
  import Compiler.{Expr => E}
  import org.francesco.asmlambda.compiler.{Syntax => S}
  import org.objectweb.asm.{ClassWriter, MethodVisitor, Opcodes, Type}

  sealed trait Expr
  object Expr {

    /** at this stage, this is for let- and lam-references only.
      * references to definitions (local or global) get turned into
      * [[Def]] or [[StaticCall]].
      */
    case class Var(v: String) extends Expr

    /** reference to a def (originally local or global). note that we
      * still need this beyond [[StaticCall]] because people might
      * reference them as functions.
      */
    case class Def(defName: String, capturedArgs: ArraySeq[String], argsArity: Int) extends Expr
    case class Record(fields: Map[String, Expr]) extends Expr
    case class Lookup(rec: Expr, field: String) extends Expr
    case class Update(rec: Expr, field: String, body: Expr) extends Expr
    case class StaticCall(defn: String, capturedArgs: ArraySeq[String], args: ArraySeq[Expr])
        extends Expr
    case class DynamicCall(fun: Expr, args: ArraySeq[Expr]) extends Expr
    case class PrimOpCall(primOp: Syntax.PrimOp, args: ArraySeq[Expr]) extends Expr
    case class Prim(prim: Syntax.Prim) extends Expr
    case class ITE(cond: Expr, left: Expr, right: Expr) extends Expr
    case class Let(v: String, bound: Expr, body: Expr) extends Expr
  }

  case class Definition(capturedArgs: ArraySeq[String], args: ArraySeq[String], body: Expr)
  case class Package(defs: Map[String, Definition], body: Expr)

  private[asmlambda] def methodDescriptor(arity: Int): MethodType = {
    MethodType.methodType(classOf[Object], Array.fill[Class[_]](arity)(classOf[Object]))
  }

  /** get the functional interface for a given arity */
  private[asmlambda] def functionalInterface(argsArity: Int): Class[_] = {
    if (argsArity <= 10 && argsArity >= 0) {
      val functions = classOf[runtime.Functions].getDeclaredClasses
      functions.filter(_.getName endsWith "Function" + argsArity.toString)(0)
    } else {
      throw new RuntimeException(s"We do not support functions with more than 10 arguments, sorry!")
    }
  }

  private def assertSingleMethod(cls: Class[_], name: String): Method = {
    val methods = cls.getMethods.filter(_.getName == name)
    if (methods.length == 0) {
      throw new RuntimeException(s"Found no $name methods in class $cls!")
    }
    if (methods.length > 1) {
      throw new RuntimeException(s"Found ${methods.length} $name methods in class $cls!")
    }
    methods(0)
  }

  /** get the apply method for the provided functional interface */
  private[asmlambda] def functionalInterfaceApply(funIface: Class[_]): Method =
    assertSingleMethod(funIface, "apply")

  private[asmlambda] def invokeDynamicDescriptor(
      capturedArgsArity: Int,
      argsArity: Int): MethodType = {
    MethodType.methodType(
      functionalInterface(argsArity),
      Array.fill[Class[_]](capturedArgsArity)(classOf[Object]))
  }

  def getAsmClassName(cls: Class[_]): String = cls.getName.replace('.', '/')

  // use the lambda metafactory that java provides as bootstrap. this was found by disassembling
  // a java file using lambdas with `javap -c -p -verbose`.
  val lambdaMetafactory: Class[_] = classOf[java.lang.invoke.LambdaMetafactory]
  val bootstapMethod: Method = assertSingleMethod(lambdaMetafactory, "metafactory")
  val bootstrapHdl: Handle = new Handle(
    Opcodes.H_INVOKESTATIC,
    getAsmClassName(lambdaMetafactory),
    bootstapMethod.getName,
    Type.getMethodDescriptor(bootstapMethod),
    false)

  private final class CompileExpr(className: String, methodVisitor: MethodVisitor) {
    private[this] sealed trait Argument
    private[this] object Argument {
      case class Expr(e: Compiler.Expr) extends Argument
      case class Local(s: String) extends Argument
    }

    private[this] def compileArguments(
        locals: Map[String, Int],
        args: Iterator[Argument]): Unit = {
      args.foreach({
        case Argument.Expr(e) => compile(locals, e)
        case Argument.Local(v) => methodVisitor.visitVarInsn(Opcodes.ALOAD, locals(v))
      })
    }

    /** compiles an expression to the bytecode that will leave the result of the
      * expression pushed on the stack.
      *
      * returns how much stack space the expression needs, which will be at least
      * 1 since it pushes the value.
      */
    def compile(locals: Map[String, Int], e: Expr): Unit = e match {
      case E.Var(v) =>
        methodVisitor.visitVarInsn(Opcodes.ALOAD, locals(v)) // push the variable on the stack

      case E.PrimOpCall(pop, args) =>
        compileArguments(locals, args.iterator.map(Argument.Expr))
        val popClass = classOf[runtime.PrimOp]
        val popMethod = pop match {
          case S.PrimOp.Add => assertSingleMethod(popClass, "add")
          case S.PrimOp.Sub => assertSingleMethod(popClass, "sub")
          case S.PrimOp.Mul => assertSingleMethod(popClass, "mul")
          case S.PrimOp.Div => assertSingleMethod(popClass, "div")
          case S.PrimOp.Eq => assertSingleMethod(popClass, "eq")
        }
        val descriptor = Type.getMethodDescriptor(popMethod)
        methodVisitor.visitMethodInsn(
          Opcodes.INVOKESTATIC,
          getAsmClassName(popClass),
          popMethod.getName,
          descriptor,
          false)

      case E.StaticCall(defn, capturedArgs, args) =>
        compileArguments(
          locals,
          capturedArgs.iterator.map[Argument](Argument.Local) ++ args.iterator.map(Argument.Expr))
        val descriptor = methodDescriptor(capturedArgs.length + args.length)
        methodVisitor.visitMethodInsn(
          Opcodes.INVOKESTATIC,
          className,
          defn,
          descriptor.toMethodDescriptorString,
          false)

      case E.Let(v, bound, body) =>
        compile(locals, bound)
        val v_local = locals.size
        methodVisitor.visitVarInsn(Opcodes.ASTORE, v_local)
        compile(locals + (v -> v_local), body)

      case E.Prim(prim) =>
        prim match {
          case Syntax.Prim.I64(v) =>
            // box the long constant
            methodVisitor.visitLdcInsn(v)
            val longClass = classOf[java.lang.Long]
            val valueOf = longClass.getMethod("valueOf", classOf[Long])
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKESTATIC,
              getAsmClassName(longClass),
              valueOf.getName,
              Type.getMethodDescriptor(valueOf),
              false)
          case Syntax.Prim.F64(v) =>
            // box the double constant
            methodVisitor.visitLdcInsn(v)
            val doubleClass = classOf[java.lang.Double]
            val valueOf = doubleClass.getMethod("valueOf", classOf[Double])
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKESTATIC,
              getAsmClassName(doubleClass),
              valueOf.getName,
              Type.getMethodDescriptor(valueOf),
              false)
          case Syntax.Prim.Bool(v) =>
            // box the boolean constant
            methodVisitor.visitLdcInsn(v)
            val booleanClass = classOf[java.lang.Boolean]
            val valueOf = booleanClass.getMethod("valueOf", classOf[Boolean])
            methodVisitor.visitMethodInsn(
              Opcodes.INVOKESTATIC,
              getAsmClassName(booleanClass),
              valueOf.getName,
              Type.getMethodDescriptor(valueOf),
              false)
          case Syntax.Prim.Text(s) =>
            // push the string directly
            methodVisitor.visitLdcInsn(s)
        }

      case E.Def(defn, capturedArgs, argsArity) =>
        compileArguments(locals, capturedArgs.map(v => Argument.Local(v)).iterator)
        val funIface = functionalInterface(argsArity)
        val applyMethod = functionalInterfaceApply(funIface)
        // Here we fill in statically the static arguments to the metafactory, see
        // <https://docs.oracle.com/javase/10/docs/api/java/lang/invoke/LambdaMetafactory.html#metafactory(java.lang.invoke.MethodHandles.Lookup,java.lang.String,java.lang.invoke.MethodType,java.lang.invoke.MethodType,java.lang.invoke.MethodHandle,java.lang.invoke.MethodType)>
        // for details
        val samMethodType = Type.getType(applyMethod)
        // TODO construct only one handle per def
        val implMethod = new Handle(
          Opcodes.H_INVOKESTATIC,
          className,
          defn,
          methodDescriptor(capturedArgs.length + argsArity).toMethodDescriptorString,
          false)
        val instantiatedMethodType = samMethodType
        methodVisitor.visitInvokeDynamicInsn(
          applyMethod.getName,
          invokeDynamicDescriptor(capturedArgs.length, argsArity).toMethodDescriptorString,
          bootstrapHdl,
          samMethodType,
          implMethod,
          instantiatedMethodType
        )

      case E.DynamicCall(fun, args) =>
        val maxs = compileArguments(
          locals,
          Iterator(Argument.Expr(fun)) ++ args.iterator.map(Argument.Expr))
        val iface = functionalInterface(args.length)
        val applyMethod = functionalInterfaceApply(iface)
        methodVisitor.visitMethodInsn(
          Opcodes.INVOKEINTERFACE,
          getAsmClassName(iface),
          applyMethod.getName,
          Type.getMethodDescriptor(applyMethod),
          true)
        maxs

      case E.ITE(cond, l, r) =>
        val r_label = new Label()
        val end_label = new Label()
        // compile and convert condition to int
        compile(locals, cond)
        val boolToInt = assertSingleMethod(classOf[runtime.PrimOp], "boolToInt")
        methodVisitor.visitMethodInsn(
          Opcodes.INVOKESTATIC,
          getAsmClassName(classOf[runtime.PrimOp]),
          boolToInt.getName,
          Type.getMethodDescriptor(boolToInt),
          false)
        // jump if 0 (false)
        methodVisitor.visitJumpInsn(Opcodes.IFEQ, r_label)
        // left branch
        compile(locals, l)
        // skip right branch, jump to ending
        methodVisitor.visitJumpInsn(Opcodes.GOTO, end_label)
        // right branch
        methodVisitor.visitLabel(r_label)
        compile(locals, r)
        methodVisitor.visitLabel(end_label)

      case E.Record(elements) =>
        // TODO this is probably better done with local variables rather than with the stack only...
        // create record object first -- so that it's going to be already in the right position in
        // the stack
        val recordClass = classOf[runtime.Record]
        methodVisitor.visitTypeInsn(Opcodes.NEW, getAsmClassName(recordClass))
        methodVisitor.visitInsn(Opcodes.DUP)
        // then create an empty hashmap
        val hashMapClass = classOf[java.util.HashMap[String, Object]]
        val hashMapConstructor = hashMapClass.getConstructor()
        val hashMapPut = assertSingleMethod(hashMapClass, "put")
        methodVisitor.visitTypeInsn(Opcodes.NEW, getAsmClassName(hashMapClass))
        methodVisitor.visitInsn(Opcodes.DUP)
        methodVisitor.visitMethodInsn(
          Opcodes.INVOKESPECIAL,
          getAsmClassName(hashMapClass),
          "<init>",
          Type.getConstructorDescriptor(hashMapConstructor),
          false)
        // add all the elements
        elements.foreach{ case (key, value) =>
          // duplicate the hashmap stack element -- we're going to pop it when putting the element
          methodVisitor.visitInsn(Opcodes.DUP)
          // push the key
          methodVisitor.visitLdcInsn(key)
          // and compile the value
          compile(locals, value)
          // put it in the hashmap
          methodVisitor.visitMethodInsn(
            Opcodes.INVOKEVIRTUAL,
            getAsmClassName(hashMapClass),
            hashMapPut.getName,
            Type.getMethodDescriptor(hashMapPut),
            false)
          // and remove the result from the stack
          methodVisitor.visitInsn(Opcodes.POP)
        }
        // finally, invoke the record constructor
        val recordConstructor = recordClass.getConstructor(hashMapClass)
        methodVisitor.visitMethodInsn(
          Opcodes.INVOKESPECIAL,
          getAsmClassName(recordClass),
          "<init>",
          Type.getConstructorDescriptor(recordConstructor),
          false)

      case E.Update(rec, fld, body) =>
        compile(locals, rec)
        methodVisitor.visitLdcInsn(fld)
        compile(locals, body)
        val popClass = classOf[runtime.PrimOp]
        val updateMethod = assertSingleMethod(popClass, "update")
        methodVisitor.visitMethodInsn(
          Opcodes.INVOKESTATIC,
          getAsmClassName(popClass),
          updateMethod.getName,
          Type.getMethodDescriptor(updateMethod),
          false)

      case E.Lookup(rec, fld) =>
        compile(locals, rec)
        methodVisitor.visitLdcInsn(fld)
        val popClass = classOf[runtime.PrimOp]
        val lookupMethod = assertSingleMethod(popClass, "lookup")
        methodVisitor.visitMethodInsn(
          Opcodes.INVOKESTATIC,
          getAsmClassName(popClass),
          lookupMethod.getName,
          Type.getMethodDescriptor(lookupMethod),
          false)
    }
  }

  private[asmlambda] def compileExpr(
      className: String,
      methodVisitor: MethodVisitor,
      locals: Map[String, Int],
      e: Expr): Unit = new CompileExpr(className, methodVisitor).compile(locals, e)

  private[asmlambda] def compileDefinition(
      className: String,
      classWriter: ClassWriter,
      defName: String,
      defn: Definition): Unit = {
    // define a static method
    val methodVisitor = classWriter.visitMethod(
      Opcodes.ACC_PUBLIC + Opcodes.ACC_STATIC,
      defName,
      methodDescriptor(defn.capturedArgs.length + defn.args.length).toMethodDescriptorString,
      null,
      null)
    methodVisitor.visitCode()

    // generate the code, by pushing instruction that generate the result and returning it.
    compileExpr(
      className,
      methodVisitor,
      Map((defn.capturedArgs ++ defn.args).zipWithIndex: _*),
      defn.body)
    methodVisitor.visitInsn(Opcodes.ARETURN)

    // trigger automatic computation of maxs (see COMPUTE_MAXS)
    methodVisitor.visitMaxs(0, 0)

    // done
    methodVisitor.visitEnd()
  }

  /** returns a class file with the compiled bytecode.
    */
  def apply(className0: String, pkg: Package): Array[Byte] = {
    val className = className0.replace('.', '/')
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_MAXS + ClassWriter.COMPUTE_FRAMES)

    // create a new class inheriting from Object -- we just use it as a container
    // for our definitions.
    classWriter.visit(
      Opcodes.V1_8,
      Opcodes.ACC_PUBLIC,
      className,
      null,
      getAsmClassName(classOf[java.lang.Object]),
      Array.empty)

    // now create a static method for each definition
    pkg.defs.foreach {
      case (defName, defn) => compileDefinition(className, classWriter, defName, defn)
    }
    // and for the main function
    compileDefinition(
      className,
      classWriter,
      "main",
      Definition(ArraySeq.empty, ArraySeq.empty, pkg.body))

    classWriter.toByteArray
  }

  def run(className: String, pkg: Package): Object = {
    val bytecode = apply(className, pkg)

    // setup loader
    val runtimeClasses = Seq(
      classOf[org.francesco.asmlambda.runtime.PrimOp],
      classOf[org.francesco.asmlambda.runtime.PrimOpError],
      classOf[org.francesco.asmlambda.runtime.Record]) ++
      (0 to 10).map(functionalInterface)
    val runtimeClassesMap: HashMap[String, Class[_]] = HashMap(runtimeClasses.map(cls => (cls.getName, cls)): _*)
    val classLoader: ClassLoader { def defineClass(name: String, bytecode: Array[Byte]): Class[_] } =
      new ClassLoader() {
        override def findClass(name: String): Class[_] =
          runtimeClassesMap.getOrElse(name, null)

        def defineClass(name: String, bytecode: Array[Byte]): Class[_] =
          defineClass(name, bytecode, 0, bytecode.length)
      }

    // define and run
    val `class` = classLoader.defineClass(className, bytecode)
    val main = `class`.getMethod("main")
    main.invoke(null)
  }
}
