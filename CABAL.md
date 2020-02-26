# <span id="top">Cabal on Microsoft Windows</span> <span style="size:25%;"><a href="README.md">↩</a></span>

<table style="font-family:Helvetica,Arial;font-size:14px;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:120px;"><a href="https://www.haskell.org/"><img src="https://wiki.haskell.org/wikiupload/6/62/Double_lambda.png" width="120"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">This repository gathers <a href="https://www.haskell.org/">Haskell</a> examples coming from various websites and books.<br/>
  It also includes several <a href="https://en.wikibooks.org/wiki/Windows_Batch_Scripting">batch files</a> for experimenting with <a href="https://www.haskell.org/">Haskell</a> on the <b>Microsoft Windows</b> platform.
  </td>
  </tr>
</table>

[Dotty][dotty_examples], [GraalSqueak][graalsqueak_examples], [Kotlin][kotlin_examples], [LLVM][llvm_examples] and [Node.js][nodejs_examples] are other topics we are currently investigating.

## <span id="proj_deps">Project dependencies</span>

This project relies on the following external software for the **Microsoft Windows** plaform:

- [Cabal 3][cabal_downloads] ([*changelog*][cabal_changelog])
- [Haskell 8][haskell_downloads] ([*release notes*][haskell_relnotes])

Optionally one may also install the following software:

- [hlint 2.2][hlint_downloads] ([*changelog*][hlint_changelog])

> **&#9755;** ***Installation policy***<br/>
> When possible we install software from a [Zip archive][zip_archive] rather than via a Windows installer. In our case we defined **`C:\opt\`** as the installation directory for optional software tools (*in reference to* the [`/opt/`][unix_opt] directory on Unix).

For instance our development environment looks as follows (*February 2020*) <sup id="anchor_01">[[1]](#footnote_01)</sup>:

<pre style="font-size:80%;">
C:\opt\ghc-8.8.2\     <i>( 2.4 GB)</i>
C:\opt\hlint-2.2.11\  <i>(74.5 MB)</i>
</pre>

> **:mag_right:** [Git for Windows][git_releases] provides a BASH emulation used to run [**`git`**][git_cli] from the command line (as well as over 250 Unix commands like [**`awk`**][man1_awk], [**`diff`**][man1_diff], [**`file`**][man1_file], [**`grep`**][man1_grep], [**`more`**][man1_more], [**`mv`**][man1_mv], [**`rmdir`**][man1_rmdir], [**`sed`**][man1_sed] and [**`wc`**][man1_wc]).

## <span id="cabal">Cabal installation</span>

For Windows users, a precompiled program
is provided (`cabal.exe`). Download the [Zip archive][cabal_downloads] and put it somewhere on your `%PATH%`
(in our case `C:\opt\ghc-8.8.2\bin\`).

<pre style="font-size:80%;">
<b>&gt; cabal update</b>
Config file path source is default config file.
Config file %APPDATA%\cabal\config not found.
Writing default configuration to
%APPDATA%\cabal\config
Downloading the latest package list from hackage.haskell.org
</pre>

## <span id="hlint">hlint installation</span>

<pre style="font-size:80%;">
<b>&gt; cabal install hlint</b>
Resolving dependencies...
Build profile: -w ghc-8.8.2 -O1
In order, the following will be built (use -v for more details):
 - base-compat-0.11.1 (lib) (requires download & build)
[...]
Starting     hlint-2.2.11 (lib)
Building     hlint-2.2.11 (lib)
Installing   hlint-2.2.11 (lib)
Completed    hlint-2.2.11 (lib)
Starting     hlint-2.2.11 (exe:hlint)
Building     hlint-2.2.11 (exe:hlint)
Installing   hlint-2.2.11 (exe:hlint)
Completed    hlint-2.2.11 (exe:hlint)
Symlinking 'hlint.exe'
cabal: Symlinking feature not available on Windows
</pre>

Since the last installation step fails on Windows, we search for the path to the `hlint` installation directory inside the Cabal local store and copies its contents to a new directory (e.g. `c:\opt\hlint-2.2.11\`):

<pre style="font-size:80%;">
<b>&gt; where /r  %APPDATA%\cabal hlint.exe</b>
%APPDATA%\cabal\store\ghc-8.8.2\hlint-2.2.11-066eefed7da269917dff3557d14ff051b3b1d4d4\bin\hlint.exe
&nbsp;
<b>&gt; xcopy /e /i /q %APPDATA%\cabal\store\ghc-8.8.2\hlint-2.2.11-066eefed7da269917dff3557d14ff051b3b1d4d4 c:\opt\hlint-2.2.11</b>
12 file(s) copied
</pre>

## <span id="footnotes">Footnotes</span>

<a name="footnote_01">[1]</a> ***Downloads*** [↩](#anchor_01)

<p style="margin:0 0 1em 20px;">
In our case we downloaded the following installation files (<a href="#proj_deps">see section 1</a>):
</p>
<pre style="margin:0 0 1em 20px; font-size:80%;">
<a href="https://www.haskell.org/cabal/download.html">cabal-install-3.0.0.0-x86_64-unknown-mingw32.zip</a>  <i>(  6 MB)</i>
<a href="">ghc-8.8.2-x86_64-unknown-mingw32.tar.xz</a>           <i>(377 MB)</i>
</pre>

<a name="footnote_02">[2]</a> ***Examples of <code>.cabal</code> files*** [↩](#anchor_02)


***

*[mics](https://lampwww.epfl.ch/~michelou/)/February 2020* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[article_abela]: http://www.cse.chalmers.se/~abela/master/layout-parsing.html
[cabal_changelog]: https://hackage.haskell.org/package/Cabal/changelog
[cabal_downloads]: https://www.haskell.org/cabal/download.html
[dotty_examples]: https://github.com/michelou/dotty-examples
[ghc_parser]: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/parser
[git_cli]: https://git-scm.com/docs/git
[git_releases]: https://git-scm.com/download/win
[git_relnotes]: https://raw.githubusercontent.com/git/git/master/Documentation/RelNotes/2.25.0.txt
[github_markdown]: https://github.github.com/gfm/
[graalsqueak_examples]: https://github.com/michelou/graalsqueak-examples
[haskell]: https://www.haskell.org
[haskell_downloads]: https://downloads.haskell.org/~ghc/8.8.2/
[haskell_relnotes]: https://downloads.haskell.org/~ghc/8.8.2/docs/html/users_guide/8.8.2-notes.html
[hlint_changelog]: https://hackage.haskell.org/package/hlint-2.2.11/changelog
[hlint_downloads]: https://hackage.haskell.org/package/hlint
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
[unix_opt]: http://tldp.org/LDP/Linux-Filesystem-Hierarchy/html/opt.html
[windows_batch_file]: https://en.wikibooks.org/wiki/Windows_Batch_Scripting
[windows_limitation]: https://support.microsoft.com/en-gb/help/830473/command-prompt-cmd-exe-command-line-string-limitation
[windows_subst]: https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/subst
[zip_archive]: https://www.howtogeek.com/178146/htg-explains-everything-you-need-to-know-about-zipped-files/
