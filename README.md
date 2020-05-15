# <span id="top">Haskell on Microsoft Windows</span>

<table style="font-family:Helvetica,Arial;font-size:14px;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:120px;"><a href="https://www.haskell.org/"><img style="border:0;" src="https://wiki.haskell.org/wikiupload/6/62/Double_lambda.png" width="120" alt="Haskell logo"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">This repository gathers <a href="https://www.haskell.org/">Haskell</a> examples coming from various websites and books.<br/>
  It also includes several <a href="https://en.wikibooks.org/wiki/Windows_Batch_Scripting">batch files</a> for experimenting with <a href="https://www.haskell.org/">Haskell</a> on the <b>Microsoft Windows</b> platform.
  </td>
  </tr>
</table>

[Dotty][dotty_examples], [GraalVM][graalvm_examples], [Kotlin][kotlin_examples], [LLVM][llvm_examples], [Node.js][nodejs_examples] and [TruffleSqueak][trufflesqueak_examples] are other topics we are currently investigating.

## <span id="proj_deps">Project dependencies</span>

This project relies on the following external software for the **Microsoft Windows** plaform:

- [Cabal 3.2][cabal_downloads] ([*changelog*][cabal_changelog])
- [Haskell 8.10][haskell_downloads] ([*release notes*][haskell_relnotes])

Optionally one may also install the following software:

- [Git 2.26][git_downloads] ([*release notes*][git_relnotes])
- [hlint 3.1][hlint_downloads] <sup id="anchor_01">[[1]](#footnote_01)</sup> ([*changelog*][hlint_changelog])
- [hpack 0.34][hpack_downloads] <sup id="anchor_01">[[1]](#footnote_01)</sup> ([*changelog*][hpack_changelog])
- [Stack 2.3][stack_downloads] ([*changelog*][stack_changelog])

> **&#9755;** ***Installation policy***<br/>
> When possible we install software from a [Zip archive][zip_archive] rather than via a Windows installer. In our case we defined **`C:\opt\`** as the installation directory for optional software tools (*in reference to* the [`/opt/`][unix_opt] directory on Unix).

For instance our development environment looks as follows (*May 2020*) <sup id="anchor_02">[[2]](#footnote_02)</sup>:

<pre style="font-size:80%;">
C:\opt\ghc-8.10.1\        <i>(2.4 GB)</i>
C:\opt\ghc-8.10.1\hlint\  <i>( 66 MB)</i>
C:\opt\ghc-8.10.1\hpack\  <i>( 45 MB)</i>
C:\opt\ghc-8.10.1\stack\  <i>( 64 MB)</i>
C:\opt\Git-2.26.2\        <i>(271 MB)</i>
</pre>

<!--
> **:mag_right:** GHC features two backends: the default native code generator (option `-fasm`) and the LLVM (version 7) code generator (option `-fllvm`). The C code generator is deprecated since GHC 7.0.
-->

> **:mag_right:** [Git for Windows][git_downloads] provides a BASH emulation used to run [**`git`**][git_cli] from the command line (as well as over 250 Unix commands like [**`awk`**][man1_awk], [**`diff`**][man1_diff], [**`file`**][man1_file], [**`grep`**][man1_grep], [**`more`**][man1_more], [**`mv`**][man1_mv], [**`rmdir`**][man1_rmdir], [**`sed`**][man1_sed] and [**`wc`**][man1_wc]).

## <span id="structure">Directory structure</span>

This project is organized as follows:

<pre style="font-size:80%;">
docs\
examples\{HelloWorld, ..}
README.md
setenv.bat
</pre>

where

- directory [**`docs\`**](docs/) contains [Haskell] related papers/articles.
- directory [**`examples\`**](examples/) contains [Haskell] examples grabbed from various websites (see file [**`examples\README.md`**](examples/README.md)).
- file [**`README.md`**](README.md) is the [Markdown][github_markdown] document for this page.
- file [**`setenv.bat`**](setenv.bat) is the batch command for setting up our environment.

<!--
> **:mag_right:** We use [VS Code][microsoft_vscode] with the extension [Markdown Preview Github Styling](https://marketplace.visualstudio.com/items?itemName=bierner.markdown-preview-github-styles) to edit our Markdown files (see article ["Mastering Markdown"](https://guides.github.com/features/mastering-markdown/) from [GitHub Guides][github_guides].
-->

We also define a virtual drive **`H:`** in our working environment in order to reduce/hide the real path of our project directory (see article ["Windows command prompt limitation"][windows_limitation] from Microsoft Support).
> **:mag_right:** We use the Windows external command [**`subst`**][windows_subst] to create virtual drives; for instance:
>
> <pre style="font-size:80%;">
> <b>&gt; subst H: %USERPROFILE%\workspace\haskell-examples</b>
> </pre>

In the next section we give a brief description of the [batch files][windows_batch_file] present in this project.

## <span id="commands">Batch commands</span>

We distinguish different sets of batch commands:

1. [**`setenv.bat`**](setenv.bat) - This batch command makes external tools such as [**`cabal.exe`**][cabal_userguide], [**`haddock.exe`**][haddock_userguide], [**`ghc.exe`**][ghc_userguide] and [**`stack.exe`**][stack_userguide] directly available from the command prompt (see section [**Project dependencies**](#proj_deps)).

   <pre style="font-size:80%;">
   <b>&gt; setenv help</b>
   Usage: setenv { &lt;option&gt; | &lt;subcommand&gt; }
   &nbsp;
     Options:
       -debug      show commands executed by this script
       -verbose    display environment settings
   &nbsp;
     Subcommands:
       help        display this help message
   </pre>


2. [**`examples\*\build.bat`**](examples/dotty-example-project/build.bat) - Each example can be built/run using the [**`build`**](examples/dotty-example-project/build.bat) command.<br/>

    <pre style="font-size:80%;">
    <b>&gt; build</b>
    Usage: build { &lt;option&gt; | &lt;subcommand&gt; }
    &nbsp;
    Options:
      -debug      show commands executed by this script
      -timer      display total elapsed time
      -verbose    display progress messages
    &nbsp;
    Subcommands:
      clean       delete generated files
      compile     generate program executable
      doc         generate HTML documentation
      help        display this help message
      run         execute the generated program
    </pre>

## <span id="usage">Usage examples</span>

#### `setenv.bat`

Command [**`setenv`**](setenv.bat) is executed once to setup our development environment; it makes external tools such as [**`cabal.exe`**][cabal_userguide], [**`haddock.exe`**][haddock_userguide], [**`ghc.exe`**][ghc_userguide] and [**`stack.exe`**][stack_userguide] directly available from the command prompt:

<pre style="font-size:80%;">
<b>&gt; setenv</b>
Tool versions:
   cabal 3.2.0.0, ghc version 8.10.1, stack 2.3.1,
   haddock 2.24.0, hlint v3.1.1, hpack 0.34.1,
   git 2.26.2.windows.1, diff 3.7

<b>&gt; where hlint hpack stack</b>
C:\opt\ghc-8.10.1\hlint\bin\hlint.exe
C:\opt\ghc-8.10.1\hpack\bin\hpack.exe
C:\opt\ghc-8.10.1\stack\stack.exe
</pre>

Command [**`setenv -verbose`**](setenv.bat) also displays the tool paths and defined variables:

<pre style="font-size:80%;">
<b>&gt; setenv -verbose</b>
Tool versions:
   cabal 3.2.0.0, ghc version 8.10.1, stack 2.3.1,
   haddock 2.24.0, hlint v3.1.1, hpack 0.34.1,
   git 2.26.2.windows.1, diff 3.7
Tool paths:
   C:\opt\ghc-8.10.1\bin\cabal.exe
   C:\opt\ghc-8.10.1\bin\ghc.exe
   C:\opt\ghc-8.10.1\stack\stack.exe
   C:\opt\ghc-8.10.1\bin\haddock.exe
   C:\opt\ghc-8.10.1\hlint\bin\hlint.exe
   C:\opt\ghc-8.10.1\hpack\bin\hpack.exe
   C:\opt\Git-2.26.2\bin\git.exe
   C:\opt\Git-2.26.2\mingw64\bin\git.exe
   C:\opt\Git-2.26.2\usr\bin\diff.exe
</pre>

## <span id="footnotes">Footnotes</span>

<a name="footnote_01">[1]</a> ***Hackage installation*** [↩](#anchor_01)

<p style="margin:0 0 1em 20px;">
We use <a href="https://www.haskell.org/cabal/"><code>cabal</code></a> to install Haskell packages, e.g. <a href="https://hackage.haskell.org/package/hlint"><code>hlint</code></a> and  <a href="https://hackage.haskell.org/package/hpack"><code>hpack</code></a>.<br/>See  document <a href="CABAL.md"><code>CABAL.md</code></a> for more information.
</p>


<a name="footnote_02">[2]</a> ***Downloads*** [↩](#anchor_02)

<p style="margin:0 0 1em 20px;">
In our case we downloaded the following installation files (<a href="#proj_deps">see section 1</a>):
</p>
<pre style="margin:0 0 1em 20px; font-size:80%;">
<a href="https://www.haskell.org/cabal/download.html">cabal-install-3.2.0.0-x86_64-unknown-mingw32.zip</a>  <i>(  5 MB)</i>
<a href="https://downloads.haskell.org/ghc/8.10.1/">ghc-8.10.1-x86_64-unknown-mingw32.tar.xz </a>         <i>(377 MB)</i>
<a href="https://git-scm.com/download/win">PortableGit-2.26.2-64-bit.7z.exe</a>                  <i>( 41 MB)</i>
<a href="https://github.com/commercialhaskell/stack/releases">stack-2.3.1-windows-x86_64.zip</a>                    <i>( 15 MB)</i>
</pre>

***

*[mics](https://lampwww.epfl.ch/~michelou/)/May 2020* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[cabal_changelog]: https://hackage.haskell.org/package/Cabal/changelog
[cabal_downloads]: https://www.haskell.org/cabal/download.html
[cabal_userguide]: https://www.haskell.org/cabal/users-guide/
[dotty_examples]: https://github.com/michelou/dotty-examples
[ghc_userguide]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using.html
[git_cli]: https://git-scm.com/docs/git
[git_downloads]: https://git-scm.com/download/win
[git_relnotes]: https://raw.githubusercontent.com/git/git/master/Documentation/RelNotes/2.26.2.txt
[github_markdown]: https://github.github.com/gfm/
[graalvm_examples]: https://github.com/michelou/graalvm-examples
[haddock_userguide]: https://www.haskell.org/haddock/doc/html/index.html
[haskell]: https://www.haskell.org
[haskell_downloads]: https://downloads.haskell.org/~ghc/8.10.1/
[haskell_relnotes]: https://downloads.haskell.org/ghc/8.10.1/docs/html/users_guide/8.10.1-notes.html
[hlint_changelog]: https://hackage.haskell.org/package/hlint-3.1.1/changelog
[hlint_downloads]: https://hackage.haskell.org/package/hlint
[hpack_changelog]: https://hackage.haskell.org/package/hpack-0.34.1/changelog
[hpack_downloads]: https://hackage.haskell.org/package/hpack
[kotlin_examples]: https://github.com/michelou/kotlin-examples
[llvm_examples]: https://github.com/michelou/llvm-examples
[man1_awk]: https://www.linux.org/docs/man1/awk.html
[man1_diff]: https://www.linux.org/docs/man1/diff.html
[man1_file]: https://www.linux.org/docs/man1/file.html
[man1_grep]: https://www.linux.org/docs/man1/grep.html
[man1_more]: https://www.linux.org/docs/man1/more.html
[man1_mv]: https://www.linux.org/docs/man1/mv.html
[man1_rmdir]: https://www.linux.org/docs/man1/rmdir.html
[man1_sed]: https://www.linux.org/docs/man1/sed.html
[man1_wc]: https://www.linux.org/docs/man1/wc.html
[nodejs_examples]: https://github.com/michelou/nodejs-examples
[stack_changelog]: https://docs.haskellstack.org/en/stable/ChangeLog/#v231
[stack_downloads]: https://github.com/commercialhaskell/stack/releases
[stack_userguide]: https://docs.haskellstack.org/en/stable/GUIDE/
[trufflesqueak_examples]: https://github.com/michelou/trufflesqueak-examples
[unix_opt]: https://tldp.org/LDP/Linux-Filesystem-Hierarchy/html/opt.html
[windows_batch_file]: https://en.wikibooks.org/wiki/Windows_Batch_Scripting
[windows_limitation]: https://support.microsoft.com/en-gb/help/830473/command-prompt-cmd-exe-command-line-string-limitation
[windows_subst]: https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/subst
[zip_archive]: https://www.howtogeek.com/178146/htg-explains-everything-you-need-to-know-about-zipped-files/
