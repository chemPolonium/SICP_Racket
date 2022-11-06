# sicp

我相信能看到这玩意的都会点英文，至少英文比我这个四级挂过一遍的人好，所以我就不写中文版了。

This is my SICP (Structure and Interpretation of Computer Programs) reading note.

I use Racket to do the exercises.

Issues (both in Chinese or English) (both bugs or whatever you are confused about) are welcomed.

## Why I use Racket?

There are some advantages using Racket:

* good support for windows: based on chez, it runs well on windows.
* GUI: it comes with a pretty good gui which can highlight the elements when hover it.
* easy to install: just use [Scoop](https://scoop.sh), which is a good package manager for windows and it's ready to go.
* good support for vscode.

## Basic Setup

### Use DrRacket

First you need to install Scoop following the guide on [Scoop](https://scoop.sh) (it's super easy, just two lines in powershell). Then you can use scoop to install racket:

```
scoop install racket
```

And it's done! Open DrRacket in Start Menu (of Start Menu/Scoop Apps) and code!

### Use vscode

I recommend [Magic Racket](https://marketplace.visualstudio.com/items?itemName=evzen-wybitul.magic-racket) and [Racket Helpers](https://marketplace.visualstudio.com/items?itemName=Calvin-LL.racket-helpers) for best vscode experience.

Magic Racket needs install language-server to run. So we need to use `raco`(included in the Racket installation) to install it:

```
raco pkg install racket-langserver
```

There are two buttons "Load file in REPL" and "Run file in Terminal" on the top right. If you want to run interactively you can use "Load file in REPL", this will run the file. Then with new lines you write, select the code you want to run, use `alt enter` to run it (see the notes below! Sometimes it can get an error). Or just use "Load file in REPL" and the code will run if changed. If you just want to run all the file at once, use "Run file in Terminal".

Personnaly, I use these setups in my vscode config file:

```
"[racket]": {
        "editor.guides.indentation": false,
        "editor.formatOnType": true,
        "editor.tabSize": 2
    },
    "racket-helpers.enableDoubleClick": false,
```

I added these configs because:

* Racket (or Scheme) has its own indent method showing the structure of the program, the indentation guide is useless and can sometimes be confusing.
* vscode does not understand the indentation for Racket, so the auto indent will sometimes make life harder, so each time I hit enter, I hope the code is formatted by the extension.
* Sometimes the 2-space tab size is useful in Racket.
* Racket Helpers provide a double-click method to select the whole brackets, however it's not useful when refactoring the code. I would recommend use `ctrl shift up/down` instead.

Some notes for using vscode: use `alt enter` to "run in REPL" cannot handle long lines, so if you get error you can try using "Run file in Terminal"