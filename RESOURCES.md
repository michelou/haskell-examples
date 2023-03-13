# <span id="top">Haskell Resources</span> <span style="size:25%;"><a href="README.md">↩</a></span>

<table style="font-family:Helvetica,Arial;line-height:1.6;">
  <tr>
  <td style="border:0;padding:0 10px 0 0;min-width:100px;"><a href="https://www.haskell.org/" rel="external"><img style="border:0;" src="./docs/images/Double_lambda.png" width="100" alt="Haskell logo"/></a></td>
  <td style="border:0;padding:0;vertical-align:text-top;">This document gathers <a href="https://www.haskell.org/" rel="external">Haskell</a> related resources we have collected so far.
  </td>
  </tr>
</table>

## <span id="articles">Articles</span>

- [Reader-Friendly Haskell Imports](https://www.extrema.is/articles/reader-friendly-haskell-imports) by Travis Cardwell, September 2021.
- [Haskell Documentation with Haddock][article_haddock] by Veronica Romashkina and Dimitrii Kovanikou, December 2020.
- [My thoughts on Haskell in 2020][article_sampellegrini] by Marco Sampelligrini, December 26, 2019.
- [Setting up Haskell in VS Code with Stack and the IDE Engine][article_doig], November 15, 2019.
- [GHC Parser][ghc_parser]
[Simple and robust layout rules for parsing programming language code][article_abela].
- [Enable All The Warnings][article_tagher] by Max Tagher, August 9, 2018.
- [Cleaning Up Our Projects with Hpack!][article_hpack], July 17, 2017.
- [Practical Haskell - Getting started with Stack][article_seanhess], August 4, 2015.
- [Haskell Design Patterns: .Extended Modules][article_jaspervdj], January 20, 2015.
- [A history of Haskell: Being lazy with class][article_hudack] by John Hudak et al., June 2007.

## <span id="blogs">Blogs</span>

- [Harry Garrood's Blog](https://harry.garrood.me/) :
  - [Easy incremental Haskell CI builds with GHC 9.4](https://harry.garrood.me/blog/easy-incremental-haskell-ci-builds-with-ghc-9.4/), September 2022.
  - [Down with Show! Part 3: A replacement for Show](https://harry.garrood.me/blog/down-with-show-part-3/), December 2018.
  - [Down with Show! Part 2: What's wrong with the Show type class](https://harry.garrood.me/blog/down-with-show-part-2/), December 2018.
  - [Down with Show! Part 1: Rules of thumb for when to use a type class](https://harry.garrood.me/blog/down-with-show-part-1/), December 2018.
- [Hasura blog posts](https://hasura.io/blog/tagged/haskell/):
  - [Hasura and Well-Typed collaborate on Haskell tooling](https://hasura.io/blog/hasura-and-well-typed-collaborate-on-haskell-tooling/), May 2022.
  - [Parser Combinators: a Walkthrough](https://hasura.io/blog/parser-combinators-walkthrough/), December 2020.
- [Algebraic Data Types in Haskell][blog_dreimanis] by Gints Dreimanis, March 2022.
- [Typed Template Haskell in GHC 9][blog_lassarote] by Heitor Toledo Lassarote de Paula, January 2022.
- Pierre Guillemot Blog Posts
  - [Haskell series part 10: `Maybe`, `Just` and `Nothing`](https://blog.kalvad.com/haskell-series-part-10/), January 2022.
  - [Haskell series part 9: modules and exceptions](https://blog.kalvad.com/haskell-series-part-9/), December 2021.
  - [Haskell series part 8: IO](https://blog.kalvad.com/haskell-series-part-8/), December 2021.
  - [Haskell series part 7: `map`, `filter` and `foldl`](https://blog.kalvad.com/haskell-series-part-7/), November 2021.
  - [Haskell series part 6: higher order functions](https://blog.kalvad.com/haskell-series-part-6/), November 2021.
  - [Haskell series part 5: pattern matching](https://blog.kalvad.com/haskell-series-part-5/), October 2021.
  - [Haskell series part 4: tuples and pattern matching](https://blog.kalvad.com/haskell-series-part-4/), September 2021.
  - [Haskell series part 3: infix and prefix functions](https://blog.kalvad.com/haskell-series-part-3/), April 2021.
  - [Haskell series part 2: lists and function declarations](https://blog.kalvad.com/haskell-series-part-2/), April 2021.
  - [Haskell series part 1: introduction](https://blog.kalvad.com/haskell-series-part-1/), July 2021.
- [Parser Combinators in Haskell][blog_lassarote_comb] by Heitor Toledo Lassarote de Paula, December 2021.
- [Past and Present of Haskell](blog_mostovoy) by J. Mostovoy, July 1, 2021.
- [How to build hybrid Haskell and Java programs](blog_dominguez) by F. Domínguez and A. Herrman, March 26, 2021.
- [Alexis King's Blog](https://lexi-lambda.github.io/) :
  - [An introduction to typeclass metaprogramming][blog_king] by Alexis King, March 25, 2021.
  - [Names are not type safety](https://lexi-lambda.github.io/blog/2020/11/01/names-are-not-type-safety/), November 2020.
  - [Types as axioms, or: playing god with static types](https://lexi-lambda.github.io/blog/2020/08/13/types-as-axioms-or-playing-god-with-static-types/), August 2020.
  - [An opinionated guide to Haskell in 2018](https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/), February 2018.
- [Haskell Explained](https://haskell-explained.gitlab.io/blog/) by Raghu Kaippully
  - [Polysemy is fun! - Part 2](https://haskell-explained.gitlab.io/blog/posts/2019/07/31/polysemy-is-cool-part-2/), July 2019.
  - [Polysemy is fun! - Part 1](https://haskell-explained.gitlab.io/blog/posts/2019/07/28/polysemy-is-cool-part-1/), July 2019.
- [Haskell for all](https://www.haskellforall.com/) by Gabriel Gonzalez
  - [Module organization guidelines for Haskell projects](https://www.haskellforall.com/2021/05/module-organization-guidelines-for.html), May 19, 2021.
  - [The trick to avoid deeply-nested error-handling code](https://www.haskellforall.com/2021/05/the-trick-to-avoid-deeply-nested-error.html), May 19, 2021.
- [Kowainik's Blog Posts](https://kowainik.github.io/tags/haskell)
  - [Fix(ity) Me](https://kowainik.github.io/posts/fixity), May 2021.
  - [Many Faces of Internal Functions](https://kowainik.github.io/posts/internal-functions), March 2021.
  - [Arrows Zoo](https://kowainik.github.io/posts/arrows-zoo), March 2021.
  - [Totality](https://kowainik.github.io/posts/totality), February 2021.
- [Lessons Learned From A Year Of Writing Haskell](https://wespiser.com/posts/2021-01-03-Lessons-Learned-From-A-Year-Of-Haskell.html) by Adam Wespiser, January 3, 2021.
- [Michael's Blog Posts](https://www.snoyman.com/blog)
  - [Haskell: The Bad Parts, part 3](https://www.snoyman.com/blog/2020/12/haskell-bad-parts-3/), December 9, 2020.
  - [Haskell: The Bad Parts, part 2](https://www.snoyman.com/blog/2020/11/haskell-bad-parts-2/), November 9, 2020.
  - [Haskell: The Bad Parts, part 1](https://www.snoyman.com/blog/2020/10/haskell-bad-parts-1/), October 28, 2020.
- [Making the most of Cabal][blog_lau], June 6, 2020.
-  [Kindness for Mean Girls: a discussion on type-level programming in Haskell][blog_nadeem] by Ayman Nadeem, May 2020.
- [GHC Blog](https://serokell.io/blog/ghc) by the [Serokell team](https://serokell.io/team):
  - [Rust vs. Haskell](https://serokell.io/blog/rust-vs-haskell) by Jay Zelenskyi, February 2023.
  - [Introduction to Haskell Typeclasses](https://serokell.io/blog/haskell-typeclasses) by Gints Dreimanis, April 2022.
  - [Algebraic Data Types in Haskell](https://serokell.io/blog/algebraic-data-types-in-haskell) by Gints Dreimanis, March 2022.
  - [Haskell to Core: Understanding Haskell Features Through Their Desugaring](https://serokell.io/blog/haskell-to-core) by Vladislav Zaviakov, August 2020.
  - [Type Witnesses in Haskell](https://serokell.io/blog/haskell-type-level-witness) by Sandeep Chandrika, February 21, 2020.
- [Haskell for impatient Scala developer: Getting into speed][blog_msitko], February 8, 2020.
- [No, dynamic type systems are not inherently more open](https://lexi-lambda.github.io/blog/2020/01/19/no-dynamic-type-systems-are-not-inherently-more-open/), January 19, 2020.
- [Deeper Stack Knowledge][blog_deeper_stack], October 2018.
- Build a CPU: [Part 1](https://yager.io/CPU/CPU2.html), October 2017, and [Part 2](https://yager.io/CPU/CPU2.html), April 2018.
- [Structuring your first Haskell project with Stack](https://sakshamsharma.com/2018/03/haskell-proj-struct/) by Saksham Sharma, March 2018.
- [PUSHER blog posts](https://making.pusher.com/):
  - [Memory profiling in Haskell](https://making.pusher.com/memory-profiling-in-haskell/), January 2016.
  - [Low latency, large working set, and GHC’s garbage collector: pick two of three](https://making.pusher.com/latency-working-set-ghc-gc-pick-two/), May 2016.
  - [Top tips and tools for optimizing Haskell](https://making.pusher.com/top-tips-and-tools-for-optimising-haskell/), October 2015.
- [Haskell on Windows](https://gundersen.net/haskell-on-windows/) by Espen Gundersen, April 2013.

## <span id="books">Books</span> [**&#x25B4;**](#top)

- [Production Haskell][book_parsons] by Matt Parsons, February 2023.<br/><span style="font-size:80%;">(ISBN 979-8-3717-9141-2, 430 pages)</span>
- [Haskell in Depth][book_bragilevsky] by Vitaly Bragilevsky, 2021.<br/><span style="font-size:80%;">(Manning, ISBN 978-1-6172-9540-9, 665 pages)</span>
- [Haskell from the Very Beginning][book_very_beginning], by John Whitington, September 2019.<br/><span style="font-size:80%;">(Coherent Press, ISBN 978-0-9576-7113-3, 214 pages)</span>
- [Practical Haskell][book_practical_haskell], by Alejandro Serrano, 2019.</br><span style="font-size:80%;">(Apress, ISBN 978-1-4842-4479-1, 2<sup>nd</sup> Edition, 595 pages)</span>
- [Thinking with Types][book_thinking_with_types], by Sandy Maguire, LeanPub 2018.
- &#128077; [Get Programming with Haskell][book_get_programming], by Will Kurt, 2018.<br/><span style="font-size:80%;">(Manning Publishing, ISBN 978-1-6172-9376-4, 616 pages)</span>
- [Parallel and Concurrent Programming in Haskell][parconc_book], OReilly 2013.<br/><span style="font-size:80%;">(on GitHub: [parconc-examples][parconc_examples])</span>
- [Learn You a Haskell for Great Good!][book_lipovaca] by Miran Lipovača
April 2011.<br/><span style="font-size:80%;">(No Starch Press, ISBN 978-1-5932-7283-8, 400 pages)</span>
- [Real World Haskell][book_real_world], by Bryan O'Sullivan &amp; al., OReilly 2008.<br/><span style="font-size:80%;">([online book](http://book.realworldhaskell.org/), freely available)</span>

## <span id="courses">Courses</span>

- [Advanced Programming](https://www.cis.upenn.edu/~cis552/current/) (CIS 552), by [Stéphanie Weirich](https://www.cis.upenn.edu/~sweirich/), Fall 2020.
- [Functional and Logic Programming](https://www.cs.ubc.ca/~poole/cs312/2019/) (CPSC 312), by David Poole, Fall 2019.

## <span id="news">News</span>

- [Haskell Planetarium][haskell_planetarium] &ndash; link aggregator of discussions about Haskell.
- [Haskell::Reddit](https://www.reddit.com/r/haskell/)
- [Haskell Weekly][haskell_weekly]

## <span id="projects">Projects</span> [**&#x25B4;**](#top)

- [haskell-language-server](https://github.com/haskell/haskell-language-server) &ndash; the official Haskell language server (LSP) implementation.
- [Kowainik][kowainik_github]: [relude][kowainik_relude],  [summoner][kowainik_summoner], [tomland][kowainik_tomland] &ndash; a Haskell <b>ST</b>atic <b>AN</b>alysis tool.
- [LibHunt Haskell](https://www.libhunt.com/l/haskell) &ndash; discover trending Haskell open-source projects.
- [polysemy](https://github.com/polysemy-research/polysemy) &ndash; a library for writing high-power, low-boilerplate DSLs.

## <span id="tutorials">Tutorials</span>

- [Monday Morning Haskell][tuto_mmhaskell].
- [Haskell at Work][tuto_haskell_at_work], January 2019.
- [Pitfalls in Haskell][haskell_pitfalls], 2014.
- [Learn You a Haskell for Great Good!][learn_you_haskell], April 2011.
  - [Making Our Own Types and Typeclasses](http://learnyouahaskell.com/making-our-own-types-and-typeclasses).
- [A Gentle Introduction do Haskell][haskell_tutorial], June 2000.
- [FP Complete Haskell](https://www.fpcomplete.com/haskell/learn/).
- [Haskell Crash Course](https://yager.io/CrashCourse/Haskell.html).
- [What I wisk I knew when learning Haskell](http://dev.stephendiehl.com/hask/) by Stephen Diehl, version 2.5.
- [Wise Man's Hasekll](https://andre.tips/wmh/).

## <span id="videos">Videos</span>

- [Advanced Functional Programming in Haskell][video_hutton] by Graham Hutton (17 sessions).
  - [AFP 15 - Making Append Vanish](https://www.youtube.com/watch?v=WQy7Bzr03R4), April 2021.
  - [AFP 14 - Induction](https://www.youtube.com/watch?v=uykHCg2VUjc), March 2021.
- [Dependent Types in Haskell][video_weirich] by Stephane Weirich, October 2017.
- [Haskell taketh away: limiting side effects for parallel programming][video_newton], June 2017.
- [Haskell Tutorial][video_banas] by Derek Banas, August 2015 (76 min).

***

*[mics](https://lampwww.epfl.ch/~michelou/)/March 2023* [**&#9650;**](#top)
<span id="bottom">&nbsp;</span>

<!-- link refs -->

[article_abela]: http://www.cse.chalmers.se/~abela/master/layout-parsing.html
[article_doig]: https://medium.com/@dogwith1eye/setting-up-haskell-in-vs-code-with-stack-and-the-ide-engine-81d49eda3ecf
[article_haddock]: https://kowainik.github.io/posts/haddock-tips
[article_hpack]: https://mmhaskell.com/blog/2017/7/17/cleaning-up-our-projects-with-hpack
[article_hudack]: https://www.researchgate.net/publication/221501761_A_history_of_Haskell_Being_lazy_with_class]
[article_jaspervdj]: https://jaspervdj.be/posts/2015-01-20-haskell-design-patterns-extended-modules.html
[article_sampellegrini]: https://alpacaaa.net/thoughts-on-haskell-2020/
[article_seanhess]: https://seanhess.github.io/2015/08/04/practical-haskell-getting-started.html
[article_tagher]: https://medium.com/mercury-bank/enable-all-the-warnings-a0517bc081c3
[blog_deeper_stack]: https://mmhaskell.com/blog/2018/10/8/deeper-stack-knowledge
[blog_dominguez]: https://www.tweag.io/blog/2021-03-25-haskell-java/
[blog_dreimanis]: https://serokell.io/blog/algebraic-data-types-in-haskell
[blog_king]: https://lexi-lambda.github.io/blog/2021/03/25/an-introduction-to-typeclass-metaprogramming/
[blog_lassarote]: https://serokell.io/blog/typed-template-haskell-in-ghc-9
[blog_lassarote_comb]: https://serokell.io/blog/parser-combinators-in-haskell
[blog_lau]: https://lukelau.me/haskell/posts/making-the-most-of-cabal/
[blog_mostovoy]: https://serokell.io/blog/past-and-present-of-haskell
[blog_msitko]: https://msitko.pl/blog/2020/02/08/haskell-getting-into-speed.html
[blog_nadeem]: https://www.aymannadeem.com/haskell/2020/05/15/Kindness-for-Mean-Girls.html
[book_bragilevsky]: https://www.manning.com/books/haskell-in-depth
[book_get_programming]: https://www.manning.com/books/get-programming-with-haskell
[book_lipovaca]: https://nostarch.com/lyah.htm
[book_parsons]: https://www.amazon.com/Production-Haskell-Succeeding-Industry/dp/B0BTNSJRKD
[book_practical_haskell]: https://www.apress.com/gp/book/9781484244791
[book_real_world]: http://book.realworldhaskell.org/
[book_thinking_with_types]: https://leanpub.com/thinking-with-types
[book_very_beginning]: https://www.bookdepository.com/Haskell-from-Very-Beginning-John-Whitington/9780957671133
[ghc_parser]: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/parser
[haskell_weekly]: https://haskellweekly.news/newsletter.html
[kowainik_github]: https://kowainik.github.io/
[kowainik_relude]: https://kowainik.github.io/projects/relude
[kowainik_summoner]: https://kowainik.github.io/projects/summoner
[kowainik_tomland]: https://kowainik.github.io/projects/tomland
[haskell_pitfalls]: http://users.jyu.fi/~sapekiis/haskell-pitfalls/
[haskell_planetarium]: https://haskell.pl-a.net/
[haskell_tutorial]: https://www.haskell.org/tutorial/index.html
[learn_you_haskell]: http://learnyouahaskell.com/chapters
[parconc_book]: https://simonmar.github.io/pages/pcph.html
[parconc_examples]: https://github.com/simonmar/parconc-examples
[tuto_haskell_at_work]: https://haskell-at-work.com/
[tuto_mmhaskell]: https://mmhaskell.com/
[video_banas]: https://www.youtube.com/watch?v=02_H3LjqMr8
[video_hutton]: https://www.youtube.com/watch?v=-qhbNGghVfc&list=PLF1Z-APd9zK5uFc8FKr_di9bfsYv8-lbc
[video_newton]: https://www.youtube.com/watch?v=lC5UWG5N8oY
[video_weirich]: https://www.youtube.com/watch?v=wNa3MMbhwS4
