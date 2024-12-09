datatype Atom = NIL | SYMBOL of string
datatype SExp = ATOM of Atom | CONS of SExp * SExp
exception Empty
exception Undefined

fun initEnv () = fn str:string => raise Undefined

fun define name oldEnv value =
    let 
        fun newEnv x = if x = name then value else oldEnv x
    in
        newEnv
    end

fun emptyNestedEnv () =
    let 
        val emptyList : (string -> SExp) list = [initEnv ()]
    in
        emptyList
    end

fun topEnv [] = raise Empty
  | topEnv (x::xs) = x

fun popEnv [] = raise Empty
  | popEnv (x::xs) = xs

fun pushEnv x xs = (x :: xs)

fun defineNested (str:string) lst out =
    let 
        val temp = topEnv lst
        val updatedEnv = define str temp out
        val updatedList = popEnv lst
        val newEnvList = pushEnv updatedEnv updatedList
    in
        newEnvList
    end

fun find (str:string) [] = raise Undefined
  | find (str:string) (env::envs) =
    (env str 
      handle Undefined => find str envs)
