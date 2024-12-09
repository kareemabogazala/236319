datatype Atom = NIL | SYMBOL of string
datatype SExp = ATOM of Atom | CONS of SExp * SExp

fun parse tokens =
  let
    fun parseList [] = (ATOM NIL, [])
      | parseList (")" :: rest) = (ATOM NIL, rest)
      | parseList ("(" :: rest) =
          let
            val (listExp, rest') = parseList rest
            val (nextExp, rest2) = parseList rest'
          in
            (CONS (listExp, nextExp), rest2)
          end
      | parseList (x :: xs) =
          let
            val atom = ATOM (SYMBOL x)
            val (nextExp, rest') = parseList xs
          in
            (CONS (atom, nextExp), rest')
          end

    fun fixList (CONS (left, ATOM NIL)) = left
      | fixList exp = exp

    val (exp, rest) = parseList tokens
  in
    fixList exp
  end