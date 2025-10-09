# Parsing

echo hello {world,haskell}.hs !

StringComplex (String "echo") 
    (StringComplex (String "hello") 
        (StringComplex (Combination [Variant ["world" "haskell"\] String ".hs\])
            (StringComplex (String "!"))))

ProgramCall(StringComplex (String "echo"), \[StringComplex (String "hello"), StringComplex (Combination [Variant ["world" "haskell"\] String ".hs\])   )
