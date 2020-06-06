# <span id="top">Cabal on Microsoft Windows</span> <span style="size:25%;"><a href="README.md">↩</a></span>

<table style="font-family:Helvetica,Arial;font-size:14px;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:120px;"><a href="https://www.haskell.org/"><img style="border:0;" src="https://wiki.haskell.org/wikiupload/6/62/Double_lambda.png" width="120" alt="Haskell logo"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">This document gathers usage information on running <a href="https://www.haskell.org/cabal/">Cabal</a>, a system for building and packaging <a href="https://www.haskell.org/">Haskell</a> libraries and programs, on the Windows platform.
  </td>
  </tr>
</table>


## <span id="cabal"><code>cabal</code> installation</span>

Windows users can download the Zip archive [cabal-install-XXXX-x86_64-unknown-mingw32.zip][cabal_downloads] and put the `cabal.exe` executable somewhere on the `%PATH%`
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

[HLint][hlint_readme] is a tool for suggesting possible improvements to [Haskell] code. We install [`hlint`][hlint_downloads] in two steps on Windows.

<pre style="font-size:80%;">
<b>&gt; cabal install hlint</b>
Resolving dependencies...
Build profile: -w ghc-8.10.1 -O1
In order, the following will be built (use -v for more details):
 - ghc-lib-parser-ex-8.10.0.5 (lib) (requires download & build)
[...]
Starting     hlint-3.1.4 (lib)
Building     hlint-3.1.4 (lib)
Installing   hlint-3.1.4 (lib)
Completed    hlint-3.1.4 (lib)
Starting     hlint-3.1.4 (exe:hlint)
Building     hlint-3.1.4 (exe:hlint)
Installing   hlint-3.1.4 (exe:hlint)
Completed    hlint-3.1.4 (exe:hlint)
Symlinking 'hlint.exe'
cabal: Symlinking feature not available on Windows
</pre>

Since the last installation step fails on Windows, we search for the path to the `hlint` installation directory inside the [Cabal][cabal_downloads] local store and copies its contents to a new directory (e.g. `c:\opt\ghc-8.10.1\hlint\`):

<pre style="font-size:80%;">
<b>&gt; where /r  %APPDATA%\cabal hlint.exe</b>
%APPDATA%\cabal\store\ghc-8.10.1\hlint-3.1.4-26c927f181ac907eed243ee781eb54759bcbf473\bin\hlint.exe
&nbsp;
<b>&gt; xcopy /e /i /q %APPDATA%\cabal\store\ghc-8.10.1\hlint-3.1.4-26c927f181ac907eed243ee781eb54759bcbf473 c:\opt\ghc-8.10.1\hlint</b>
12 file(s) copied
&nbsp;
<b>&gt; where hlint</b>
C:\opt\ghc-8.10.1\hlint\bin\hlint.exe
&nbsp;
<b>&gt; hlint --version</b>
HLint v3.1.4, (C) Neil Mitchell 2006-2020
</pre>

> **:mag_right:** We can check the latest available version of package `hlint` with command `cabal list hlint`:
> <pre style="font-size:80%;">
> <b>&gt; cabal list hlint</b>
> * hlint
>     Synopsis: Source code suggestions
>     Default available version: 3.1.4
>     Installed versions: [ Not installed ]
>     Homepage: https://github.com/ndmitchell/hlint#readme
>     License:  BSD3
> [..]
> </pre>

## <span id="hpack"><code>hpack</code> installation</span>

[Hpack][hpack_readme] is a format for Haskell packages. Similarly to `hlint` we install [`hpack`][hpack_downloads] in two steps on Windows.

<pre style="font-size:80%;">
<b>&gt; cabal install hpack</b>
Resolving dependencies...
Build profile: -w ghc-8.10.1 -O1
In order, the following will be built (use -v for more details):
 - cabal-doctest-1.0.8 (lib) (requires build)
[...]
Starting     hpack-0.34.1 (lib)
Building     hpack-0.34.1 (lib)
Installing   hpack-0.34.1 (lib)
Completed    hpack-0.34.1 (lib)
Starting     hpack-0.34.1 (exe:hpack)
Building     hpack-0.34.1 (exe:hpack)
Installing   hpack-0.34.1 (exe:hpack)
Completed    hpack-0.34.1 (exe:hpack)
Symlinking 'hpack.exe'
cabal: Symlinking feature not available on Windows
</pre>

Since the last installation step fails on Windows, we search for the path to the `hpack` installation directory inside the [Cabal][cabal_downloads] local store and copies its contents to a new directory (e.g. `c:\opt\ghc-8.10.1\hpack\`):

<pre style="font-size:80%;">
<b>&gt; where /r  %APPDATA%\cabal hpack.exe</b>
%APPDATA%\cabal\store\ghc-8.10.1\hpack-0.34.1-f68688feb83b5293acf1f7e59f01ab5e8e700938\bin\hpack.exe
&nbsp;
<b>&gt; xcopy /e /i /q %APPDATA%\cabal\store\ghc-8.10.1\hpack-0.34.1-f68688feb83b5293acf1f7e59f01ab5e8e700938 c:\opt\ghc-8.10.1\hpack</b>
3 file(s) copied
&nbsp;
<b>&gt; where hpack</b>
C:\opt\ghc-8.10.1\hlint\bin\hpack.exe
</pre>

> **:mag_right:** We can check the latest available version of package `hpack` with command `cabal list hpack`:
> <pre style="font-size:80%;">
> <b>&gt; cabal list hpack</b>
> * hpack
>     Synopsis: A modern format for Haskell packages
>     Default available version: 0.34.1
>     Installed versions: [ Not installed ]
>     Homepage: https://github.com/sol/hpack#readme
>     License:  MIT
> [..]
> </pre>

## <span id="footnotes">Footnotes</span>

<a name="footnote_01">[1]</a> ***Downloads*** [↩](#anchor_01)

<p style="margin:0 0 1em 20px;">
In our case we downloaded the following installation files (<a href="#proj_deps">see section 1</a>):
</p>
<pre style="margin:0 0 1em 20px; font-size:80%;">
<a href="https://www.haskell.org/cabal/download.html">cabal-install-3.2.0.0-x86_64-unknown-mingw32.zip</a>  <i>(  5 MB)</i>
<a href="https://downloads.haskell.org/~ghc/8.10.1/">ghc-8.10.1-x86_64-unknown-mingw32.tar.xz</a>          <i>(377 MB)</i>
<a href="https://docs.haskellstack.org/en/stable/install_and_upgrade/#manual-download">stack-2.3.1-windows-x86_64.zip</a>                    <i>( 15 MB)</i>
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

*[mics](https://lampwww.epfl.ch/~michelou/)/June 2020* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[article_abela]: http://www.cse.chalmers.se/~abela/master/layout-parsing.html
[cabal_changelog]: https://hackage.haskell.org/package/Cabal/changelog
[cabal_downloads]: https://www.haskell.org/cabal/download.html
[dotty_examples]: https://github.com/michelou/dotty-examples
[ghc_parser]: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/parser
[git_cli]: https://git-scm.com/docs/git
[git_downloads]: https://git-scm.com/download/win
[git_relnotes]: https://raw.githubusercontent.com/git/git/master/Documentation/RelNotes/2.26.2.txt
[github_markdown]: https://github.github.com/gfm/
[graalvm_examples]: https://github.com/michelou/graalvm-examples
[haskell]: https://www.haskell.org
[haskell_downloads]: https://downloads.haskell.org/~ghc/8.10.1/
[haskell_relnotes]: https://downloads.haskell.org/~ghc/8.10.1/docs/html/users_guide/8.10.1-notes.html
[hlint_changelog]: https://hackage.haskell.org/package/hlint-3.1.4/changelog
[hlint_downloads]: https://hackage.haskell.org/package/hlint
[hlint_readme]: https://hackage.haskell.org/package/hlint-3.1.4#readme
[hpack_changelog]: https://hackage.haskell.org/package/hpack-0.34.1/changelog
[hpack_downloads]: https://hackage.haskell.org/package/hpack
[hpack_readme]: https://github.com/sol/hpack#readme
[kotlin_examples]: https://github.com/michelou/kotlin-examples
[llvm_examples]: https://github.com/michelou/llvm-examples
[nodejs_examples]: https://github.com/michelou/nodejs-examples
[stack_changelog]: https://docs.haskellstack.org/en/stable/ChangeLog/
[stack_downloads]: https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows
[trufflesqueak_examples]: https://github.com/michelou/trufflesqueak-examples
[unix_opt]: http://tldp.org/LDP/Linux-Filesystem-Hierarchy/html/opt.html
[windows_batch_file]: https://en.wikibooks.org/wiki/Windows_Batch_Scripting
[windows_limitation]: https://support.microsoft.com/en-gb/help/830473/command-prompt-cmd-exe-command-line-string-limitation
[windows_subst]: https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/subst
[zip_archive]: https://www.howtogeek.com/178146/htg-explains-everything-you-need-to-know-about-zipped-files/
