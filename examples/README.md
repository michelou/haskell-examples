# <span id="top">Haskell examples</span> <span style="size:30%;"><a href="../README.md">⬆</a></span>

<table style="font-family:Helvetica,Arial;font-size:14px;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:120px;"><a href="https://www.haskell.org/"><img style="border:0;" src="https://wiki.haskell.org/wikiupload/6/62/Double_lambda.png" width="120" alt="Haskell logo"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">Directory <strong><code>examples\</code></strong> contains <a href="https://www.haskell.org/" alt="Haskell">Haskell</a> code examples coming from various websites - mostly from the <a href="https://www.haskell.org/" rel="external">Haskell project</a>.
  </td>
  </tr>
</table>

In the following we present the two examples [**`Factorial`**](#factorial) and [**`QuickSort`**](#quicksort).

Build tools rely on one or more configuration files to achieve their tasks. In our case we created the following configuration files for example [**`Factorial`**](#factorial):

| Build tool                    | Configuration file                       | Parent file                |
|-------------------------------|------------------------------------------|----------------------------|
| **`build.bat`**                   | **`build.properties`**                   | n.a.                       |
| [**`cabal.exe`**][cabal_cli]      | [**`build.gradle`**](Factorial/Factorial.cabal) | n.a.                |
| [**`mvn.cmd`**][apache_maven_cli] | [**`pom.xml`**](Factorial/pom.xml)       | [**`pom.xml`**](./pom.xml) |
| [**`stack.exe`**][stack_cli]      | [**`stack.yaml`**](Factorial/stack.yaml) | n.a.                       |


## <span id="factorial">Factorial</span>

The directory structure of project `Factorial` looks as follows:
<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/cd">cd</a></b>
H:\examples\Factorial
&nbsp;
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/tree">tree</a> /a /f . | findstr /v "^[A-Z]"</b>
|   .gitignore
|   <a href="Factorial/.hlint.yaml">.hlint.yaml</a>
|   <a href="Factorial/build.bat">build.bat</a>
|   <a href="Factorial/Factorial.cabal">Factorial.cabal</a>
|   <a href="Factorial/stack.yaml">stack.yaml</a>
|   Setup.hs
|
\---app
        <a href="./Factorial/app/Main.hs">Main.hs</a>
</pre>

### <span id="factorial_cabal">***Cabal build/run***</span>

Command `cabal run all` builds and execute the [Haskell] application (configuration file [`Factorial.cabal`](./Factorial/Factorial.cabal)):
<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> cabal</b>
C:\opt\ghc-8.10.3\bin\cabal.exe
&nbsp;
<b>&gt; cabal run all</b>
Resolving dependencies...
Build profile: -w ghc-8.10.3 -O1
In order, the following will be built (use -v for more details):
 - Factorial-0.1.0.0 (exe:Factorial) (first run)
Configuring executable 'Factorial' for Factorial-0.1.0.0..
Preprocessing executable 'Factorial' for Factorial-0.1.0.0..
Building executable 'Factorial' for Factorial-0.1.0.0..
[1 of 1] Compiling Main             ( app\Main.hs, H:\examples\Factorial\dist-newstyle\build\x86_64-windows\ghc-8.10.3\Factorial-0.1.0.0\x\Factorial\build\Factorial\Factorial-tmp\Main.o )
Linking H:\examples\Factorial\dist-newstyle\build\x86_64-windows\ghc-8.10.3\Factorial-0.1.0.0\x\Factorial\build\Factorial\Factorial.exe ...
factorialRec(5) =120
factorialRec2(5)=120
factorialFold(5)=120
factorialProd(5)=120
</pre>

### <span id="factorial_stack">***Stack build/run***</span>

Command `stack run` builds and executes the [Haskell] application (configuration file [`stack.yaml`](./Factorial/stack.yaml)):
<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1" rel="external">where</a> stack</b>
C:\opt\ghc-8.10.3\stack\stack.exe
&nbsp;
<b>&gt; <a href="https://docs.haskellstack.org/en/stable/build_command/">stack</a> --silent run</b>
factorialRec(5) =120
factorialRec2(5)=120
factorialFold(5)=120
factorialProd(5)=120
</pre>

### <span id="factorial_maven">***Maven build/run***</span>

Command `mvn -q compile run` builds and executes the [Haskell] application (configuration file [`pom.xml`](./Factorial/pom.xml))

<pre style="font-size:80%;">
<b>&gt; <a href="https://maven.apache.org/ref/3.6.3/maven-embedder/cli.html">mvn</a> -q clean compile exec:exec</b>
[1 of 1] Compiling Main             ( app\Main.hs, target\gen\Main.o )
Linking target/Main.exe ...
factorialRec(5) =120
factorialRec2(5)=120
factorialFold(5)=120
factorialProd(5)=120
</pre>

> **:mag_right:** We can build a profile-version of the project and execute the profile-instrumented [Haskell] application as follows:
> <pre style="font-size:80%;">
> <b>&gt; stack build --profile</b>
> &nbsp;
> <b>&gt; <a href="https://docs.haskellstack.org/en/stable/build_command/">stack</a> exec target\dist\29cc6475\build\Factorial\Factorial.exe -- +RTS -p</b>
> factorialRec(5) =120
> factorialRec2(5)=120
> factorialFold(5)=120
> factorialProd(5)=120
> </pre>
> Profiling results are stored in file `Factorial.prof`:
> <pre style="font-size:80%;">
> <b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/more">more</a> Factorial.prof</b>
>         Tue Mar 03 22:21 2020 Time and Allocation Profiling Report  (Final)
> &nbsp;
>            Factorial.exe +RTS -p -RTS
> &nbsp;
>         total time  =        0.00 secs   (0 ticks @ 1000 us, 1 processor)
>         total alloc =      56,488 bytes  (excludes profiling overheads)
> &nbsp;
> COST CENTRE MODULE           SRC                         %time %alloc
> &nbsp;
> main        Main             app\Main.hs:(33,1)-(39,13)    0.0   36.0
> CAF         GHC.IO.Handle.FD &lt;entire-module&gt;               0.0   61.5
> &nbsp;
>                                                                                       individual      inherited
> COST CENTRE      MODULE                   SRC                        no.     entries  %time %alloc   %time %alloc
> &nbsp;
> MAIN             MAIN                     &lt;built-in&gt;                 125          0    0.0    0.9     0.0  100.0
>  CAF             GHC.IO.Handle.Text       &lt;entire-module&gt;            176          0    0.0    0.1     0.0    0.1
> [..]
> </pre>

### <span id="factorial_batch">***Batch build/run***</span>

Command [`build clean run`](Factorial/build.bat) builds and executes the [Haskell] application:
<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> build</b>
H:\examples\Factorial\build.bat
&nbsp;
<b>&gt; <a href="Factorial/build.bat">build</a> clean run</b>
factorialRec(5) =120
factorialRec2(5)=120
factorialFold(5)=120
factorialProd(5)=120
</pre>

> **:mag_right:** Commands `build` and `mvn` accept one or more subcommands while `cabal` and `stack` accept only one. For instance the following command lines produce the same result:
> <pre style="font-size:80%;">
> <b>&gt; <a href="Factorial/build.bat">build</a> clean run</b>
> <b>&gt; <a href="https://maven.apache.org/ref/3.6.3/maven-embedder/cli.html">mvn</a> clean compile exec:exec</b>
> <b>&gt; cabal clean && cabal run all</b>
> <b>&gt; stack clean && stack run</b>
> </pre>

Command [`build -debug clean run`](Factorial/build.bat) also displays the internally executed commands:
<pre style="font-size:80%;">
<b>&gt; <a href="Factorial/build.bat">build</a> -debug clean run</b>
[build] _CLEAN=1 _COMPILE=1 _DOC=0 _RUN=1 _VERBOSE=0
[build] rmdir /s /q "H:\examples\Factorial\target"
[build] ghc.exe -Wall -Werror -o "H:\examples\Factorial\target\Main.exe" -hidir "H:\examples\Factorial\target\gen" -odir "H:\examples\Factorial\target\gen"  "H:\examples\Factorial\app\Main.hs"
[1 of 1] Compiling Main             ( H:\examples\Factorial\app\Main.hs, H:\examples\Factorial\target\gen\Main.o )
Linking H:\examples\Factorial\target\Main.exe ...
[build] H:\examples\Factorial\target\Main.exe
factorialRec(5) =120
factorialRec2(5)=120
factorialFold(5)=120
factorialProd(5)=120
[build] _EXITCODE=0
</pre>

## <span id="quicksort">QuickSort</span>

The directory structure of project `QuickSort` looks as follows:
<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/cd">cd</a></b>
H:\examples\QuickSort
&nbsp;
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/tree">tree</a> /a /f . | findstr /v "^[A-Z]"</b>
|   .gitignore
|   <a href="./QuickSort/build.bat">build.bat</a>
|   <a href="./QuickSort/QuickSort.cabal">QuickSort.cabal</a>
|   Setup.hs
|   <a href="./QuickSort/stack.yaml">stack.yaml</a>
|
\---app
        <a href="./QuickSort/app/Main.hs">Main.hs</a>
</pre>

### <span id="quicksort_cabal">***Cabal build/run***</span>

Command `cabal run all` builds and executes the [Haskell] application (configuration file [`QuickSort.cabal`](./QuickSort/QuickSort.cabal)):

<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> cabal</b>
C:\opt\ghc-8.10.3\bin\cabal.exe
&nbsp;
<b>&gt; cabal run all</b>
Resolving dependencies...
Build profile: -w ghc-8.10.3 -O1
In order, the following will be built (use -v for more details):
 - QuickSort-0.1.0.0 (exe:QuickSort) (first run)
Configuring executable 'QuickSort' for QuickSort-0.1.0.0..
Preprocessing executable 'QuickSort' for QuickSort-0.1.0.0..
Building executable 'QuickSort' for QuickSort-0.1.0.0..
[1 of 1] Compiling Main             ( app\Main.hs, H:\examples\QuickSort\dist-newstyle\build\x86_64-windows\ghc-8.10.3\QuickSort-0.1.0.0\x\QuickSort\build\QuickSort\QuickSort-tmp\Main.o )
Linking H:\examples\QuickSort\dist-newstyle\build\x86_64-windows\ghc-8.10.3\QuickSort-0.1.0.0\x\QuickSort\build\QuickSort\QuickSort.exe ...
input list       : [8,4,0,3,1,23,11,18]
sorted(filter)   : [0,1,3,4,8,11,18,23]
sorted(list comp): [0,1,3,4,8,11,18,23]
</pre>

### <span id="quicksort_stack">***Stack build/run***</span>

Command `stack run` builds and executes the [Haskell] application (configuration file [`stack.yaml`](./QuickSort/stack.yaml)):

<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> stack</b>
C:\opt\ghc-8.10.3\stack\stack.exe
&nbsp;
<b>&gt; stack --silent run</b>
input list       : [8,4,0,3,1,23,11,18]
sorted(filter)   : [0,1,3,4,8,11,18,23]
sorted(list comp): [0,1,3,4,8,11,18,23]
</pre>

### <span id="quicksort_batch">***Batch build/run***</span>

Command [`build clean run`](QuickSort/build.bat) builds and executes the [Haskell] application:
<pre style="font-size:80%;">
<b>&gt; <a href="https://docs.microsoft.com/en-us/windows-server/administration/windows-commands/where_1">where</a> build</b>
H:\examples\QuickSort\build.bat
&nbsp;
<b>&gt; <a href="QuickSort/build.bat">build</a> clean run</b>
input list       : [8,4,0,3,1,23,11,18]
sorted(filter)   : [0,1,3,4,8,11,18,23]
sorted(list comp): [0,1,3,4,8,11,18,23]
</pre>

<!--
## <span id="footnotes">Footnotes</span>

<a name="footnote_01">[1]</a> ***hlint installation*** [↩](#anchor_01)

<p style="margin:0 0 1em 20px;">
We use <a href="https://www.haskell.org/cabal/"><code>cabal</code></a> to install package <a href="https://hackage.haskell.org/package/hlint"><code>hlint</code></a>; see  document <a href="CABAL.md"><code>CABAL.md</code></a> for more information about its usage.
</p>
-->

***

*[mics](https://lampwww.epfl.ch/~michelou/)/January 2021* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[apache_maven_cli]: https://maven.apache.org/ref/3.6.3/maven-embedder/cli.html
[cabal]: https://www.haskell.org/cabal/
[cabal_changelog]: https://hackage.haskell.org/package/Cabal/changelog
[cabal_cli]: https://cabal.readthedocs.io/en/stable/intro.html
[cabal_downloads]: https://www.haskell.org/cabal/download.html
[dotty_examples]: https://github.com/michelou/dotty-examples
[ghc_parser]: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/parser
[graalsqueak_examples]: https://github.com/michelou/graalsqueak-examples
[haskell]: https://www.haskell.org
[kotlin_examples]: https://github.com/michelou/kotlin-examples
[llvm_examples]: https://github.com/michelou/llvm-examples
[stack_cli]: https://docs.haskellstack.org/en/stable/build_command/
[stack_userguide]: https://docs.haskellstack.org/en/stable/GUIDE/
[windows_batch_file]: https://en.wikibooks.org/wiki/Windows_Batch_Scripting
