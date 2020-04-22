# <span id="top">Cabal on Microsoft Windows</span> <span style="size:25%;"><a href="README.md">↩</a></span>

<table style="font-family:Helvetica,Arial;font-size:14px;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:120px;"><a href="https://www.haskell.org/"><img style="border:0;" src="https://wiki.haskell.org/wikiupload/6/62/Double_lambda.png" width="120" alt="Haskell Logo"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">This document gathers usage information on running <a href="https://www.haskell.org/cabal/">Cabal</a>, a system for building and packaging <a href="https://www.haskell.org/">Haskell</a> libraries and programs, on the Windows platform.
  </td>
  </tr>
</table>

[Dotty][dotty_examples], [GraalSqueak][graalsqueak_examples], [GraalVM][graalvm_examples], [Kotlin][kotlin_examples], [LLVM][llvm_examples] and [Node.js][nodejs_examples] are other topics we are currently investigating.

## <span id="proj_deps">Project dependencies</span>

This project relies on the following external software for the **Microsoft Windows** plaform:

- [Cabal 3.2][cabal_downloads] ([*changelog*][cabal_changelog])
- [Haskell 8][haskell_downloads] ([*release notes*][haskell_relnotes])

Optionally one may also install the following software:

- [Git 2.26][git_downloads] ([*release notes*][git_relnotes])
- [hlint 2.2][hlint_downloads] ([*changelog*][hlint_changelog])
- [hpack 0.33][hpack_downloads] ([*changelog*][hpack_changelog])
- [Stack 2.1][stack_downloads] ([*changelog*][stack_changelog])

> **&#9755;** ***Installation policy***<br/>
> When possible we install software from a [Zip archive][zip_archive] rather than via a Windows installer. In our case we defined **`C:\opt\`** as the installation directory for optional software tools (*in reference to* the [`/opt/`][unix_opt] directory on Unix).

For instance our development environment looks as follows (*April 2020*) <sup id="anchor_01">[[1]](#footnote_01)</sup>:

<pre style="font-size:80%;">
C:\opt\ghc-8.10.1\        <i>(  2.4 GB)</i>
C:\opt\ghc-8.10.1\hlint\  <i>( 74.5 MB)</i> <i>(copied from %APPDATA%\cabal\store\ghc-8.10.1\hlint-2.2.11-xx\)</i>
C:\opt\ghc-8.10.1\hpack\  <i>( 45.0 MB)</i> <i>(copied from %APPDATA%\cabal\store\ghc-8.10.1\hpack-0.33.0-xx\)</i>
C:\opt\ghc-8.10.1\stack\  <i>( 64.3 MB)</i>
C:\opt\Git-2.26.0\        <i>(269.1 MB)</i>
</pre>

## <span id="cabal"><code>cabal</code> installation</span>

For Windows users, a precompiled program
is provided (`cabal.exe`). Download the [Zip archive][cabal_downloads] and put it somewhere on your `%PATH%`
(in our case `C:\opt\ghc-8.10.1\bin\`).

<pre style="font-size:80%;">
<b>&gt; where cabal</b>
C:\opt\ghc-8.10.1\bin\cabal.exe
&nbsp;
<b>&gt; cabal update</b>
Config file path source is default config file.
Config file %APPDATA%\cabal\config not found.
Writing default configuration to
%APPDATA%\cabal\config
Downloading the latest package list from hackage.haskell.org
</pre>

## <span id="hlint"><code>hlint</code> installation</span>

We install [`hlint`][hlint_downloads] manually on Windows.

<pre style="font-size:80%;">
<b>&gt; cabal install hlint</b>
Resolving dependencies...
Build profile: -w ghc-8.10.1 -O1
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

Since the last installation step fails on Windows, we search for the path to the `hlint` installation directory inside the [Cabal][cabal_downloads] local store and copies its contents to a new directory (e.g. `c:\opt\ghc-8.10.1\hlint\`):

<pre style="font-size:80%;">
<b>&gt; where /r  %APPDATA%\cabal hlint.exe</b>
%APPDATA%\cabal\store\ghc-8.10.1\hlint-2.2.11-066eefed7da269917dff3557d14ff051b3b1d4d4\bin\hlint.exe
&nbsp;
<b>&gt; xcopy /e /i /q %APPDATA%\cabal\store\ghc-8.10.1\hlint-2.2.11-066eefed7da269917dff3557d14ff051b3b1d4d4 c:\opt\ghc-8.10.1\hlint</b>
12 file(s) copied
</pre>

## <span id="footnotes">Footnotes</span>

<a name="footnote_01">[1]</a> ***Downloads*** [↩](#anchor_01)

<p style="margin:0 0 1em 20px;">
In our case we downloaded the following installation files (<a href="#proj_deps">see section 1</a>):
</p>
<pre style="margin:0 0 1em 20px; font-size:80%;">
<a href="https://www.haskell.org/cabal/download.html">cabal-install-3.2.0.0-x86_64-unknown-mingw32.zip</a>  <i>(  5 MB)</i>
<a href="">ghc-8.10.1-x86_64-unknown-mingw32.tar.xz</a>           <i>(377 MB)</i>
</pre>

<a name="footnote_02">[2]</a> ***<code>stack.yaml</code> versus a <code>.cabal</code> file*** [↩](#anchor_02)
<p style="margin:0 0 1em 20px;">
Differences between a <code>stack.yaml</code> file and a <code>.cabal</code> file &#8213; as described in the <a href="https://docs.haskellstack.org/en/stable/stack_yaml_vs_cabal_package_file/">Online Stack documentation</a> &#8213; can be resumed as follows: 
<ul>
<li><a href="https://www.haskell.org/cabal/">Cabal</a> is a build system, which is used by <a href="https://docs.haskellstack.org/en/stable/README/">Stack</a>. Cabal defines the concept of a <i>package</i> (eg. name, version, 0 or more executables, etc.).</li>
<li><a href="https://docs.haskellstack.org/en/stable/README/">Stack</a> is a build tool that works <i>on top</i> of the <a href="https://www.haskell.org/cabal/">Cabal</a> build system, and defines a new concept called a <i>project</i> (eg. GHC options, etc.)</li>
</ul>
</p>

***

*[mics](https://lampwww.epfl.ch/~michelou/)/April 2020* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[article_abela]: http://www.cse.chalmers.se/~abela/master/layout-parsing.html
[cabal_changelog]: https://hackage.haskell.org/package/Cabal/changelog
[cabal_downloads]: https://www.haskell.org/cabal/download.html
[dotty_examples]: https://github.com/michelou/dotty-examples
[ghc_parser]: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/parser
[git_cli]: https://git-scm.com/docs/git
[git_downloads]: https://git-scm.com/download/win
[git_relnotes]: https://raw.githubusercontent.com/git/git/master/Documentation/RelNotes/2.26.0.txt
[github_markdown]: https://github.github.com/gfm/
[graalsqueak_examples]: https://github.com/michelou/graalsqueak-examples
[graalvm_examples]: https://github.com/michelou/graalvm-examples
[haskell]: https://www.haskell.org
[haskell_downloads]: https://downloads.haskell.org/~ghc/8.10.1/
[haskell_relnotes]: https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/8.10.1-notes.html
[hlint_changelog]: https://hackage.haskell.org/package/hlint-2.2.11/changelog
[hlint_downloads]: https://hackage.haskell.org/package/hlint
[hpack_changelog]: https://hackage.haskell.org/package/hpack-0.33.0/changelog
[hpack_downloads]: https://hackage.haskell.org/package/hpack
[kotlin_examples]: https://github.com/michelou/kotlin-examples
[llvm_examples]: https://github.com/michelou/llvm-examples
[nodejs_examples]: https://github.com/michelou/nodejs-examples
[stack_changelog]: https://docs.haskellstack.org/en/stable/ChangeLog/
[stack_downloads]: https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows
[unix_opt]: http://tldp.org/LDP/Linux-Filesystem-Hierarchy/html/opt.html
[windows_batch_file]: https://en.wikibooks.org/wiki/Windows_Batch_Scripting
[windows_limitation]: https://support.microsoft.com/en-gb/help/830473/command-prompt-cmd-exe-command-line-string-limitation
[windows_subst]: https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/subst
[zip_archive]: https://www.howtogeek.com/178146/htg-explains-everything-you-need-to-know-about-zipped-files/
