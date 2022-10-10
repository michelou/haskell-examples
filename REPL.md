# <span id="top">Haskell REPL</span> <span style="size:25%;"><a href="README.md">↩</a></span>

<table style="font-family:Helvetica,Arial;font-size:14px;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:100px;"><a href="https://www.haskell.org/" rel="external"><img style="border:0;" src="https://wiki.haskell.org/wikiupload/6/62/Double_lambda.png" width="100" alt="Haskell project"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">This document presents usage examples of the <a href="https://www.haskell.org/" rel="external">GHC</a> REPL.
  </td>
  </tr>
</table>

## <span id="">REPL commands</span>

<pre style="font-size:80%;">
<b>&gt; <a href="https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/ghci.html">ghci</a></b>
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
Loaded package environment from <a href="https://docs.microsoft.com/en-us/windows/deployment/usmt/usmt-recognized-environment-variables#variables-that-are-recognized-only-in-the-user-context">%APPDATA%</a>\ghc\x86_64-mingw32-8.10.7\environments\default
&nbsp;
Prelude> let inc x = x + 1
Prelude> inc 3
4
Prelude> :info inc
inc :: Num a => a -> a  -- Defined at <interactive>:1:5
Prelude> :q
Leaving GHCi.
</pre>

> **:mag_right:** The session history is saved in reverse order into file `ghci_history` :
> <pre style="font-size:80%;">
> <b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/type">type</a> <a href="https://docs.microsoft.com/en-us/windows/deployment/usmt/usmt-recognized-environment-variables#variables-that-are-recognized-only-in-the-user-context">%APPDATA%</a>\ghc\ghci_history</b>
> :q
> :info inc
> inc 3
> let inc x = x + 1
> </pre>
> Use the prefix <code>:!</code> to run a shell command, e.g. to clear the screen.
> <pre style="font-size:80%;">
> <b>:!</b> cls
> </pre>

## <span id="footnotes">Footnotes</span>

<span id="footnote_01">[1]</span> ***Command-line Options in GHCi*** [↩](#anchor_01)

<dl><dd>
See online <a href="https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html#ghci-cmd-line-options">GHCi</a> documentation.
</dd></dl>

***

*[mics](https://lampwww.epfl.ch/~michelou/)/October 2022* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->
