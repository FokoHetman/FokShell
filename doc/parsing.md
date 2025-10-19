# 1. Basics

## 1.1 Program calls
echo hello {world,haskell}.hs !

StringComplex (String "echo") 
    (StringComplex (String "hello") 
        (StringComplex (Combination [Variant ["world" "haskell"\] String ".hs\])
            (StringComplex (String "!"))))

ProgramCall(StringComplex (String "echo"), \[StringComplex (String "hello"), StringComplex (Combination [Variant ["world" "haskell"\] String ".hs\])   )

## 1.2 Blockers

(as of now) there are 2 types of blockers:
* NodeBlockers - they block parsing of a node, as another node is found, ex: `&` in `echo hi && echo hello`
* StringBlockers - they block parsing of specific blocks within string (refer to program calls), ex: `,` in `echo {hi,hello}.hs`

## 1.3 bash-isms

todo
