# <span id="top">Haskell on Microsoft Windows</span>

<table style="font-family:Helvetica,Arial;font-size:14px;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:100px;"><a href="https://www.haskell.org/" rel="external"><img style="border:0;" src="https://wiki.haskell.org/wikiupload/6/62/Double_lambda.png" width="100" alt="Haskell logo"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">This repository gathers <a href="https://www.haskell.org/" rel="external">Haskell</a> examples coming from various websites and books.<br/>
  It also includes several <a href="https://en.wikibooks.org/wiki/Windows_Batch_Scripting" rel="external">batch files</a> for experimenting with <a href="https://www.haskell.org/" rel="external">Haskell</a> on the <b>Microsoft Windows</b> platform.
  </td>
  </tr>
</table>

[GraalVM][graalvm_examples], [Kotlin][kotlin_examples], [LLVM][llvm_examples], [Node.js][nodejs_examples], [Scala 3][dotty_examples] and [TruffleSqueak][trufflesqueak_examples] are other topics we are currently monitoring.

## <span id="proj_deps">Project dependencies</span>

This project relies on the following external software for the **Microsoft Windows** plaform:

- [Cabal 3.2][cabal_downloads] ([*changelog*][cabal_changelog])
- [Git 2.30][git_downloads] ([*release notes*][git_relnotes])
- [Haskell 8.10][haskell_downloads] ([*release notes*][haskell_relnotes])

Optionally one may also install the following software:

- [Apache Maven 3.6][apache_maven] ([requires Java 7][apache_maven_history])  ([*release notes*][apache_maven_relnotes])
- [hlint 3.2][hlint_downloads] <sup id="anchor_01">[[1]](#footnote_01)</sup> ([*changelog*][hlint_changelog])
- [hpack 0.34][hpack_downloads] <sup id="anchor_01">[[1]](#footnote_01)</sup> ([*changelog*][hpack_changelog])
- [Oracle OpenJDK 11][oracle_openjdk] <sup id="anchor_01">[[1]](#footnote_01)</sup> ([*release notes*][oracle_openjdk_relnotes], for Maven)
- [HTF 0.14][htf_downloads] ([*changelog*][htf_changelog])
- [ormolu 0.1][ormolu_downloads] ([*changelog*][ormolu_changelog])
- [Stack 2.5][stack_downloads] ([*changelog*][stack_changelog])

> **&#9755;** ***Installation policy***<br/>
> When possible we install software from a [Zip archive][zip_archive] rather than via a Windows installer. In our case we defined **`C:\opt\`** as the installation directory for optional software tools (*in reference to* the [`/opt/`][unix_opt] directory on Unix).

For instance our development environment looks as follows (*February 2021*) <sup id="anchor_02">[[2]](#footnote_02)</sup>:

<pre style="font-size:80%;">
C:\opt\apache-maven-3.6.3\         <i>( 10 MB)</i>
C:\opt\ghc-8.10.3\                 <i>(2.4 GB)</i>
C:\opt\ghc-8.10.3\hlint-3.2.7\     <i>( 68 MB)</i>
C:\opt\ghc-8.10.3\hpack-0.34.3\    <i>( 49 MB)</i>
C:\opt\ghc-8.10.3\HTF-0.14.0.5\    <i>( 16 MB)</i>
C:\opt\ghc-8.10.3\ormolu-0.1.4.1\  <i>( 58 MB)</i>
C:\opt\ghc-8.10.3\stack-2.5.1\     <i>( 70 MB)</i>
C:\opt\Git-2.30.0\                 <i>(290 MB)</i>
C:\opt\jdk-11.0.10+9\              <i>(181 MB)</i>
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
parconc-examples\
<a href="CABAL.md">CABAL.md</a>
README.md
<a href="RESOURCES.md">RESOURCES.md</a>
<a href="setenv.bat">setenv.bat</a>
</pre>

where

- directory [**`docs\`**](docs/) contains [Haskell] related papers/articles.
- directory [**`examples\`**](examples/) contains [Haskell] examples grabbed from various websites (see file [**`examples\README.md`**](examples/README.md)).
- directory [**`parconc-examples`**](parconc-examples/) contains [Haskell] examples from Simon Marlow's [book][book_parconc] (see file [**`parconc-examples\README.md`**](parconc-examples/README.md)).
- file [**`CABAL.md`**](CABAL.md) gathers usage information about the Cabal tool.
- file **`README.md`** is the [Markdown][github_markdown] document for this page.
- file [**`RESOURCES.md`**](RESOURCES.md) gathers Haskell related informations.
- file [**`setenv.bat`**](setenv.bat) is the batch command for setting up our environment.

<!--
> **:mag_right:** We use [VS Code][microsoft_vscode] with the extension [Markdown Preview Github Styling](https://marketplace.visualstudio.com/items?itemName=bierner.markdown-preview-github-styles) to edit our Markdown files (see article ["Mastering Markdown"](https://guides.github.com/features/mastering-markdown/) from [GitHub Guides][github_guides].
-->

We also define a virtual drive **`H:`** in our working environment in order to reduce/hide the real path of our project directory (see article ["Windows command prompt limitation"][windows_limitation] from Microsoft Support).
> **:mag_right:** We use the Windows external command [**`subst`**][windows_subst] to create virtual drives; for instance:
>
> <pre style="font-size:80%;">
> <b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/subst">subst</a> H: %USERPROFILE%\workspace\haskell-examples</b>
> </pre>

In the next section we give a brief description of the [batch files][windows_batch_file] present in this project.

## <span id="commands">Batch commands</span>

We distinguish different sets of batch commands:

1. [**`setenv.bat`**](setenv.bat) - This batch command makes external tools such as [**`cabal.exe`**][cabal_userguide], [**`haddock.exe`**][haddock_userguide], [**`ghc.exe`**][ghc_userguide] and [**`stack.exe`**][stack_userguide] directly available from the command prompt (see section [**Project dependencies**](#proj_deps)).

   <pre style="font-size:80%;">
   <b>&gt; <a href="./setenv.bat">setenv</a> help</b>
   Usage: setenv { &lt;option&gt; | &lt;subcommand&gt; }
   &nbsp;
     Options:
       -debug      show commands executed by this script
       -verbose    display environment settings
   &nbsp;
     Subcommands:
       help        display this help message
   </pre>

2. [**`examples\*\build.bat`**](examples/Factorial/build.bat) - Each example can be built/run using the [**`build`**](examples/Factorial/build.bat) command.<br/>

    <pre style="font-size:80%;">
    <b>&gt; <a href="examples/Factorial/build.bat">build</a></b>
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

### `setenv.bat`

Command [**`setenv`**](setenv.bat) is executed once to setup our development environment; it makes external tools such as [**`cabal.exe`**][cabal_userguide], [**`haddock.exe`**][haddock_userguide], [**`ghc.exe`**][ghc_userguide] and [**`stack.exe`**][stack_userguide] directly available from the command prompt:

<pre style="font-size:80%;">
<b>&gt; <a href="setenv.bat">setenv</a></b>
Tool versions:
   cabal 3.2.0.0, ghc version 8.10.3, stack 2.5.1,
   haddock 2.24.0, hlint v3.2.7, hpack 0.34.3, htfpp 0.14.0.5
   ormolu 0.1.4.1, git 2.30.0.windows.1, diff 3.7

<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> hlint hpack stack</b>
C:\opt\ghc-8.10.3\hlint\bin\hlint.exe
C:\opt\ghc-8.10.3\hpack\bin\hpack.exe
C:\opt\ghc-8.10.3\stack\stack.exe
</pre>

Command [**`setenv -verbose`**](setenv.bat) also displays the tool paths and defined variables:

<pre style="font-size:80%;">
<b>&gt; <a href="setenv.bat">setenv</a> -verbose</b>
Tool versions:
   cabal 3.2.0.0, ghc version 8.10.3, stack 2.5.1,
   haddock 2.24.0, hlint v3.2.7, hpack 0.34.3, htfpp 0.14.0.5
   ormolu 0.1.4.1, git 2.30.0.windows.1, diff 3.7
Tool paths:
   C:\opt\ghc-8.10.3\bin\cabal.exe
   C:\opt\ghc-8.10.3\bin\ghc.exe
   C:\opt\ghc-8.10.3\stack-2.5.1\stack.exe
   C:\opt\ghc-8.10.3\bin\haddock.exe
   C:\opt\ghc-8.10.3\hlint-3.2.7\bin\hlint.exe
   C:\opt\ghc-8.10.3\hpack-0.34.3\bin\hpack.exe
   C:\opt\ghc-8.10.3\HTF-0.14.0.5\bin\htfpp.exe
   C:\opt\ghc-8.10.3\ormolu-0.1.4.1\bin\ormolu.exe
   C:\opt\Git-2.30.0\bin\git.exe
   C:\opt\Git-2.30.0\mingw64\bin\git.exe
   C:\opt\Git-2.30.0\usr\bin\diff.exe
</pre>

## <span id="footnotes">Footnotes</span>

<b name="footnote_01">[1]</b> ***Hackage installation*** [↩](#anchor_01)

<p style="margin:0 0 1em 20px;">
We use <a href="https://www.haskell.org/cabal/"><code>cabal</code></a> to install Haskell packages, e.g. <a href="https://hackage.haskell.org/package/hlint"><code>hlint</code></a> and  <a href="https://hackage.haskell.org/package/hpack"><code>hpack</code></a>.<br/>See  document <a href="CABAL.md"><code>CABAL.md</code></a> for more information.
</p>

<b name="footnote_02">[2]</b> ***Downloads*** [↩](#anchor_02)

<p style="margin:0 0 1em 20px;">
In our case we downloaded the following installation files (<a href="#proj_deps">see section 1</a>):
</p>
<pre style="margin:0 0 1em 20px; font-size:80%;">
<a href="https://www.haskell.org/cabal/download.html">cabal-install-3.2.0.0-x86_64-unknown-mingw32.zip</a>  <i>(  5 MB)</i>
<a href="https://downloads.haskell.org/ghc/8.10.3/">ghc-8.10.3-x86_64-unknown-mingw32.tar.xz </a>         <i>(411 MB)</i>
<a href="https://git-scm.com/download/win">PortableGit-2.30.0-64-bit.7z.exe</a>                  <i>( 41 MB)</i>
<a href="https://github.com/commercialhaskell/stack/releases">stack-2.5.1-windows-x86_64.zip</a>                    <i>( 15 MB)</i>
</pre>

***

*[mics](https://lampwww.epfl.ch/~michelou/)/February 2021* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[apache_maven]: https://maven.apache.org/download.cgi
[apache_maven_cli]: https://maven.apache.org/ref/current/maven-embedder/cli.html
[apache_maven_history]: https://maven.apache.org/docs/history.html
[apache_maven_relnotes]: https://maven.apache.org/docs/3.6.3/release-notes.html
[book_parconc]: https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/
[cabal_changelog]: https://hackage.haskell.org/package/Cabal/changelog
[cabal_downloads]: https://www.haskell.org/cabal/download.html
[cabal_userguide]: https://www.haskell.org/cabal/users-guide/
[dotty_examples]: https://github.com/michelou/dotty-examples
[ghc_userguide]: https://downloads.haskell.org/ghc/latest/docs/html/users_guide/using.html
[git_cli]: https://git-scm.com/docs/git
[git_downloads]: https://git-scm.com/download/win
[git_relnotes]: https://raw.githubusercontent.com/git/git/master/Documentation/RelNotes/2.30.0.txt
[github_markdown]: https://github.github.com/gfm/
[graalvm_examples]: https://github.com/michelou/graalvm-examples
[haddock_userguide]: https://www.haskell.org/haddock/doc/html/index.html
[haskell]: https://www.haskell.org
[haskell_downloads]: https://downloads.haskell.org/ghc/latest/
[haskell_relnotes]: https://downloads.haskell.org/ghc/8.10.3/docs/html/users_guide/8.10.3-notes.html
[hlint_changelog]: https://hackage.haskell.org/package/hlint/changelog
[hlint_downloads]: https://hackage.haskell.org/package/hlint
[hpack_changelog]: https://hackage.haskell.org/package/hpack/changelog
[hpack_downloads]: https://hackage.haskell.org/package/hpack
[htf_changelog]: https://hackage.haskell.org/package/HTF-0.14.0.5/changelog
[htf_downloads]: https://hackage.haskell.org/package/HTF
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
[oracle_openjdk]: https://adoptopenjdk.net/?variant=openjdk11&jvmVariant=hotspot
<!-- also: https://github.com/AdoptOpenJDK/openjdk8-binaries/releases/tag/jdk8u252-b09 -->
[oracle_openjdk_relnotes]: https://mail.openjdk.java.net/pipermail/jdk-updates-dev/2020-October/004007.html
[ormolu_changelog]: https://hackage.haskell.org/package/ormolu-0.1.4.1/changelog
[ormolu_downloads]: https://hackage.haskell.org/package/ormolu
[stack_changelog]: https://docs.haskellstack.org/en/stable/ChangeLog/
[stack_downloads]: https://github.com/commercialhaskell/stack/releases
[stack_userguide]: https://docs.haskellstack.org/en/stable/GUIDE/
[trufflesqueak_examples]: https://github.com/michelou/trufflesqueak-examples
[unix_opt]: https://tldp.org/LDP/Linux-Filesystem-Hierarchy/html/opt.html
[windows_batch_file]: https://en.wikibooks.org/wiki/Windows_Batch_Scripting
[windows_limitation]: https://support.microsoft.com/en-gb/help/830473/command-prompt-cmd-exe-command-line-string-limitation
[windows_subst]: https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/subst
[zip_archive]: https://www.howtogeek.com/178146/htg-explains-everything-you-need-to-know-about-zipped-files/
