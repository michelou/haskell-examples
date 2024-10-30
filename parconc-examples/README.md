# <span id="top">Book &ndash; <i>Parallel and Concurrent Programming in Haskell</i></span> <span style="size:30%;"><a href="../README.md">⬆</a></span>

<table style="font-family:Helvetica,Arial;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:120px;"><a href="https://www.haskell.org/" rel="external"><img src="../docs/images/Double_lambda.png" width="120" alt="Haskell project"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">The <a href="."><strong><code>parconc-examples\</code></strong></a> directory contains <a href="https://www.haskell.org/" rel="external" alt="Haskell">Haskell</a> examples presented in <a href="https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/">Marlow's book</a> "<i>Parallel and Concurrent Programming in Haskell</i>" (<a href="https://www.oreilly.com/" rel="external">O'Reilly</a>, 2013).</td>
  </tr>
</table>

In the following we present the two examples [**`rpar`**](#rpar) and [**`strat`**](#strat).

> **:mag_right:** The source archive with the original code examples is available from repository [simonmar/parconc-examples][simonmar_repo] on [GitHub](https://github.com/) (see also the original [README.md][simonmar_readme]).

We can build/run code examples in directory [**`parconc-examples\`**](./) in several ways. For instance we have the following configuration files for example [**`rpar`**](#rpar):

| Build&nbsp;tool               | Build&nbsp;file                      | Parent file                |
|-------------------------------|--------------------------------------|----------------------------|
| [**`build.bat`**](./build.bat) | [`parconc-examples.cabal`](./parconc-examples.cabal) <sup id="anchor_01">[1](#footnote_01)</sup>  | n.a.                       |
| [**`cabal.exe`**][cabal_cli]      | [`parconc-examples.cabal`](./parconc-examples.cabal) | n.a.             |
| [**`mvn.cmd`**][apache_maven_cli] | [`pom.xml`](./pom.xml)       | [**`pom.xml`**](./pom.xml) |
| [**`stack.exe`**][stack_cli]      | [`stack.yaml`](./stack.yaml) | n.a.                       |


## <span id="rpar">`rpar` Example</span>

### <span id="rpar_cabal">***Cabal***</span>

<pre style="font-size:80%;">
<b>&gt; <a href="https://man.archlinux.org/man/cabal.1">cabal</a> clean &amp; <a href="https://man.archlinux.org/man/cabal.1">cabal</a> run rpar</b>
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - parconc-examples-0.4.7 (exe:rpar) (first run)
Configuring executable 'rpar' for parconc-examples-0.4.7..
Preprocessing executable 'rpar' for parconc-examples-0.4.7..
Building executable 'rpar' for parconc-examples-0.4.7..
[1 of 1] Compiling Main             ( src\rpar.hs, H:\parconc-examples\dist-newstyle\build\x86_64-windows\ghc-8.10.7\parconc-examples-0.4.7\x\rpar\build\rpar\rpar-tmp\Main.o )
Linking H:\parconc-examples\dist-newstyle\build\x86_64-windows\ghc-8.10.7\parconc-examples-0.4.7\x\rpar\build\rpar\rpar.exe ...
rpar: user error (Pattern match failure in do expression at src\rpar.hs:18:3-5)
</pre>

### <span id="rpar_stack">***Stack***</span>

<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.haskellstack.org/en/stable/build_command/">stack</a> clean &amp;&amp; <a href="https://docs.haskellstack.org/en/stable/build_command/" rel="external">stack</a> build</b>
</pre>

<!--
will build all the executables and install them in a platform-specific
subdirectory under `.stack-work/install`.
-->

### <span id="rpar_batch">***Batch***</span>

Command [`build clean run`](./build.bat) builds and executes the [Haskell] program [`src\rpar.hs`](src/rpar.hs):
<pre style="font-size:80%;">
<b>&gt; <a href="./build.bat">build</a> clean run</b>
time: 0.00s
(24157817,14930352)
time: 3.76s
</pre>

With option `-debug` command [`build`](./build.bat) also displays several useful informations, e.g.
- Execution environment : display *actual value* of properties, options, variables.
- Conditional processing : compile *only if* target is older than sources.
- Execution transparency : the executed console command with options and arguments can be *copied/run separately*.
<pre style="font-size:80%;">
<b>&gt; <a href="./build.bat">build</a> -debug clean run</b>
[build] Options    : _TIMER=0 _VERBOSE=0
[build] Subcommands: _CLEAN=1 _COMPILE=1 _DOC=0 _RUN=1
[build] Variables  : _EXEC=rpar
[build] rmdir /s /q "H:\parconc-examples\target"
[build] "ghc.exe" -Wall -Wincomplete-uni-patterns -hidir "H:\parconc-examples\target\gen" -odir "H:\parconc-examples\target\gen" -threaded -i"H:\parconc-examples\lib\monad-par-0.3.5:H:\parconc-examples\lib\parallel-3.2.2.0:H:\parconc-examples\lib\timeit-2.0" -XHaskell2010 -o "H:\parconc-examples\target\rpar.exe" -Rghc-timing H:\parconc-examples\src\rpar.hs
[1 of 4] Compiling Control.Parallel ( ... )
[2 of 4] Compiling Control.Seq      ( ... )
[3 of 4] Compiling Control.Parallel.Strategies ( ... )
[4 of 4] Compiling Main             ( ... )
Linking H:\parconc-examples\target\rpar.exe ...
&lt;&lt;ghc: 437400616 bytes, 124 GCs, 10149480/33063872 avg/max bytes residency (10 samples), 86M in use, 0.000 INIT (0.001 elapsed), 0.328 MUT (13.692 elapsed), 0.188 GC (0.305 elapsed) :ghc&gt;&gt;
[build] "H:\parconc-examples\target\rpar.exe" 1
time: 0.00s
(24157817,14930352)
time: 3.76s
[build] _EXITCODE=0
</pre>

## <span id="strat">`strat` Example</span> <sup style="font-size:60%;">[**&#9650;**](#top)</sup>

### <span id="strat_cabal">***Cabal***</span>

<pre style="font-size:80%;">
<b>&gt; <a href="https://man.archlinux.org/man/cabal.1">cabal</a> clean & cabal run strat</b>
Resolving dependencies...
Build profile: -w ghc-8.10.7 -O1
In order, the following will be built (use -v for more details):
 - parconc-examples-0.4.7 (exe:strat) (first run)
Configuring executable 'strat' for parconc-examples-0.4.7..
Preprocessing executable 'strat' for parconc-examples-0.4.7..
Building executable 'strat' for parconc-examples-0.4.7..
[1 of 1] Compiling Main             ( src\strat.hs, H:\parconc-examples\dist-newstyle\build\x86_64-windows\ghc-8.10.7\parconc-examples-0.4.7\x\strat\build\strat\strat-tmp\Main.o )
Linking H:\parconc-examples\dist-newstyle\build\x86_64-windows\ghc-8.10.7\parconc-examples-0.4.7\x\strat\build\strat\strat.exe ...
(14930352,24157817)
</pre>

### <span id="strat_stack">***Stack***</span>

<pre style="font-size:80%;">
</pre>

### <span id="strat_batch">***Batch***</span>

With option `-exec:strat` command [`build`](./build.bat) builds and executes the [Haskell] application `strat`:
<pre style="font-size:80%;">
<b>&gt; <a href="./build.bat">build</a> -verbose -exec:strat clean run</b>
Delete directory "target"
Compile Haskell source files to directory "H:\parconc-examples\target"
Execute Haskell program "target\strat.exe"
(14930352,24157817)
</pre>

With option `-debug` command [`build`](./build.bat) also displays several useful informations, e.g.
- Execution environment : display *actual value* of properties, options, variables.
- Conditional processing : compile *only if* target is older than sources.
- Execution transparency : the executed console command with options and arguments can be *copied/run separately*.
<pre style="font-size:80%;">
<b>&gt; <a href="./build.bat">build</a> -debug -exec:strat run</b>
[build] Options    : _TIMER=0 _VERBOSE=0
[build] Subcommands: _CLEAN=0 _COMPILE=1 _DOC=0 _RUN=1
[build] Variables  : "CABAL_DIR=%APPDATA%\cabal"
[build] Variables  : "GHC_HOME=C:\opt\ghc-8.10.7"
[build] Variables  : _EXEC=strat
[build] 20210921191143 Target : "H:\parconc-examples\target\strat.exe"
[build] 20200910092417 Sources: "H:\parconc-examples\src\*.hs"
[build] _ACTION_REQUIRED=0
[build] "H:\parconc-examples\target\strat.exe" 1
(14930352,24157817)
[build] _EXITCODE=0
</pre>


## <span id="footnotes">Footnotes</span>

<span id="footnote_01">[1]</span> ***build.bat*** [↩](#anchor_01)

<dl><dd>
In project <code>Factorial</code> the batch file <a href="./build.bat"><code>build.bat</code></a> reads several properties directly from <a href="./parconc-examples.cabal"><code>Factorial.cabal</code></a> if the Cabal project file is present; for instance: <code>name</code>, <code>synopsis</code>, <code>version</code>, <code>executable</code>, <code>main-is</code>, <code>ghc-options</code> and <code>default-language</code>.
</dd></dl>

***

*[mics](https://lampwww.epfl.ch/~michelou/)/November 2024* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[apache_maven_cli]: https://maven.apache.org/ref/3.6.3/maven-embedder/cli.html
[cabal_cli]: https://cabal.readthedocs.io/en/stable/intro.html
[cabal_userguide]: https://www.haskell.org/cabal/users-guide/
[haskell]: https://www.haskell.org
[simonmar_readme]: https://github.com/simonmar/parconc-examples/blob/master/README.md
[simonmar_repo]: https://github.com/simonmar/parconc-examples
[stack_cli]: https://docs.haskellstack.org/en/stable/build_command/
[stack_userguide]: https://docs.haskellstack.org/en/stable/GUIDE/
