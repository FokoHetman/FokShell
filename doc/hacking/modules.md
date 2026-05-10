# Theory

Modules in FokShell are a way to hook into the core event loop. Following hooks are exposed:

- initHook - executed upon shell's startup
- exitHook - executed upon exitting the shell
- resetHook - a helper hook used in haltAction etc. to reset the state of all Modules
- preHook - executed on keypress, *before* primitive keybinds handling
- postHook - executed on keypress, *after* primitive keybinds handling
-# "primitive keybinds handling" refers to keybinds like: left/right arrow, inputs, etc.

## Modules

### Colorscheme

used to define global colorschemes, and the currently used one

```hs
ColorschemeModule
    { colorschemes = 
      [
        Colorscheme
        { successColor = RGB 52 219 102
        , errorColor = RGB 219 77 52
        , infoColor = RGB 91 91 91
        , textColor = RGB 255 255 255
        , userColors = []
        }
      ]
    , current = 0
    }
```

### CursorModule

used to define the cursor style

```hs
CursorModule
    { shape = BlinkingBar
    , color = RGB 200 200 255
    }
```

### HistoryModule

used to collect, read, write and index history

```hs
-- historyFile is an useful helper, as the module itself is more abstract than files.
historyFile (withHomeDir ".config/fokshell/history") 10000
    { addBuiltins = True -- note: currently not implemented. set to False instead.
    }
```

### JobManager

Job Manager is used to run the parser, convert node into a task and finally deploy and store running jobs.
Preprocessors are used on the `Node` received from the parser before deploying the job.

```hs
JobManagerModule
    { jobs = []
    , preprocessors = [connectPreprocessors [substituter "~" (T.pack <$> getHomeDirectory) 1, envVarPreprocessor]]
    }
```

### ParserModule

Defines the global parser for your shell

```hs
ParserModule
    { parser = r3
    }
    where
    r0 = primitives empty
    r1 = pcall r0 <|> r0
    r2 = pipes r1 <|> r1
    r3 = chains r2 <|> r2
    primitives, pcall, pipes, chains :: Parser Node -> Parser Node
    primitives lower = Node <$> (parse lower :: Parser Primitive)
    pcall lower = Node <$> (parse lower :: Parser ProcessCall)
    pipes lower = Node <$> (parse lower :: Parser PipelineExp)
    chains lower = Node <$> (parse lower :: Parser ChainExp)
```

### PromptModule

Defines the prompt using a component system (yet to be made really good & useful).

```hs
PromptModule 
  { components =
    fmap (PromptComponent . TextComponent)
    [ (pure "[", \cs -> foreground $ cs.userColors!!0)
    , (T.pack <$> getEffectiveUserName, \cs -> bold <> foreground (cs.userColors!!0))
      , (pure "@", \cs -> foreground $ cs.userColors!!0)
      , (T.pack <$> getHostName, \cs -> bold <> foreground (cs.userColors!!0))
      , (pure ":", \cs -> foreground $ cs.userColors!!0)
      , (getFormattedDirectory, \cs -> bold <> foreground (cs.userColors!!0))
      , (pure "]$ ", \cs -> foreground $ cs.userColors!!0)
    ]
  }
```

### TabCompletion

Module defining behavior of Tab Completion.

```hs
(def :: TabCompletion)
    { sortAlgorithm = const sort
    , autocomplete = def
    , maxSuggestions = 10
    , shadowText = True
    , completionRules = []
    }
```
